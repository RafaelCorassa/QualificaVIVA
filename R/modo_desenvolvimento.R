# Recarga seletiva para desenvolvimento --------------------------------------
#
# Este módulo permite atualizar partes seguras do aplicativo sem encerrar a
# sessão Shiny. A base disponível para a etapa e os resultados já calculados
# ficam nos `reactiveVal` do servidor e não são apagados pela recarga. Após uma
# padronização bem-sucedida, a base bruta completa é descartada normalmente.
#
# A ativação e a desativação ficam em `R/configuracao_aplicativo.R`, no campo:
#
#   configuracao_viva_quali$desenvolvimento$ativo
#
# O arquivo de configuração é lido em um ambiente isolado antes de cada etapa.
# Dessa forma, trocar TRUE/FALSE passa a valer no próximo clique, sem reiniciar
# o aplicativo.

# Relação explícita de módulos seguros para cada operação. A ordem respeita as
# dependências: dados e funções compartilhadas são carregados antes dos módulos
# específicos que os utilizam.
arquivos_recarregamento_desenvolvimento <- function(etapa) {
  planos <- list(
    padronizacao = c(
      "R/regras_padronizacao.R",
      "R/padronizar_base.R"
    ),
    enriquecimento = "R/enriquecer_cnes.R",
    validade = c(
      "R/regras_validade.R",
      "R/analisar_validade.R"
    ),
    consistencia = c(
      "R/regras_consistencia.R",
      "R/analisar_consistencia.R"
    ),
    relatorio_validade = c(
      "R/referencias_territoriais.R",
      "R/indicadores_relatorio_validade.R",
      "R/priorizacao_relatorio.R",
      "R/visualizacoes_relatorio.R",
      "R/relatorio_validade.R"
    ),
    relatorio_consistencia = c(
      "R/referencias_territoriais.R",
      "R/indicadores_relatorio_validade.R",
      "R/indicadores_relatorio_consistencia.R",
      "R/priorizacao_relatorio.R",
      "R/visualizacoes_relatorio.R",
      "R/visualizacoes_relatorio_consistencia.R",
      "R/relatorio_validade.R",
      "R/relatorio_consistencia.R"
    )
  )
  if (!etapa %in% names(planos)) {
    stop(sprintf("Etapa de desenvolvimento desconhecida: %s.", etapa), call. = FALSE)
  }
  planos[[etapa]]
}

# Lê somente a configuração salva no disco, em ambiente isolado, para que a
# mudança do interruptor TRUE/FALSE seja detectada mesmo com o app em execução.
ler_configuracao_desenvolvimento <- function(
    caminho = file.path("R", "configuracao_aplicativo.R")) {
  if (!file.exists(caminho)) {
    stop(sprintf("Arquivo de configuração não encontrado: %s.", caminho), call. = FALSE)
  }
  ambiente_leitura <- new.env(parent = baseenv())
  sys.source(caminho, envir = ambiente_leitura)
  configuracao <- ambiente_leitura$configuracao_viva_quali$desenvolvimento
  if (is.null(configuracao) || !is.logical(configuracao$ativo) ||
      length(configuracao$ativo) != 1L || is.na(configuracao$ativo)) {
    stop(
      "A opção desenvolvimento$ativo deve ser definida como TRUE ou FALSE.",
      call. = FALSE
    )
  }
  configuracao
}

# Recarrega a configuração e os módulos da etapa dentro do mesmo ambiente usado
# pelo aplicativo. Retorna TRUE quando houve recarga e FALSE no modo normal.
recarregar_modulos_desenvolvimento <- function(
    etapa, ambiente, diretorio_projeto = getwd()) {
  caminho_configuracao <- file.path(
    diretorio_projeto, "R", "configuracao_aplicativo.R"
  )
  configuracao_desenvolvimento <- ler_configuracao_desenvolvimento(
    caminho_configuracao
  )
  if (!isTRUE(configuracao_desenvolvimento$ativo)) return(invisible(FALSE))

  arquivos_relativos <- arquivos_recarregamento_desenvolvimento(etapa)
  arquivos <- file.path(diretorio_projeto, arquivos_relativos)
  ausentes <- arquivos_relativos[!file.exists(arquivos)]
  if (length(ausentes)) {
    stop(sprintf(
      "Módulos de desenvolvimento não encontrados: %s.",
      paste(ausentes, collapse = ", ")
    ), call. = FALSE)
  }

  # Valida a sintaxe de todos os arquivos antes de substituir qualquer função.
  # Assim, um erro de digitação não deixa apenas metade da etapa atualizada.
  arquivos_para_validar <- c(caminho_configuracao, arquivos)
  for (arquivo in arquivos_para_validar) parse(file = arquivo)

  # A configuração é sempre a primeira a ser atualizada, garantindo que títulos,
  # dimensões e cores recém-salvos sejam usados pelas funções recarregadas.
  sys.source(caminho_configuracao, envir = ambiente)
  for (arquivo in arquivos) sys.source(arquivo, envir = ambiente)

  if (isTRUE(configuracao_desenvolvimento$mostrar_mensagens)) {
    message(sprintf(
      "[Modo de desenvolvimento] Etapa '%s' recarregada: %s.",
      etapa, paste(basename(arquivos), collapse = ", ")
    ))
  }
  invisible(TRUE)
}
