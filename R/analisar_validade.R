# Motor da análise de validade -------------------------------------------------
#
# Recebe a base já padronizada e gera um indicador lógico por regra. `TRUE`
# representa um registro sinalizado, `FALSE` um registro não sinalizado e `NA`
# uma regra impossível de executar por falta ou ambiguidade de variáveis.

# Localiza uma variável sem diferenciar letras maiúsculas e minúsculas.
localizar_coluna_validade <- function(nomes, variavel) {
  which(tolower(nomes) == tolower(variavel))
}

# Confere se as variáveis exigidas existiam na base original e permanecem
# identificáveis após a padronização. Colunas criadas pelo processo não tornam
# uma regra artificialmente executável.
mapear_dependencias_validade <- function(base_padronizada, variaveis_originais,
                                         dependencias) {
  nomes_base <- names(base_padronizada)
  ausentes <- character()
  ambiguas <- character()
  mapeamento <- character()

  for (variavel in dependencias) {
    origem <- localizar_coluna_validade(variaveis_originais, variavel)
    if (!length(origem)) {
      ausentes <- c(ausentes, variavel)
      next
    }
    if (length(origem) > 1L) {
      ambiguas <- c(ambiguas, variavel)
      next
    }
    indice_base <- localizar_coluna_validade(nomes_base, variavel)
    if (length(indice_base) != 1L) {
      if (!length(indice_base)) ausentes <- c(ausentes, variavel) else ambiguas <- c(ambiguas, variavel)
      next
    }
    mapeamento[[variavel]] <- nomes_base[[indice_base]]
  }

  list(
    colunas = mapeamento,
    ausentes = unique(ausentes),
    ambiguas = unique(ambiguas)
  )
}

# Converte valores para texto previsível e troca ausências por string vazia.
texto_validade <- function(x) {
  resultado <- if (is.character(x)) x else as.character(x)
  if (anyNA(resultado)) resultado[is.na(resultado)] <- ""
  resultado
}

# Regra 1: ausência do código 1 em todas as naturezas de violência.
avaliar_regra_1 <- function(base, colunas) {
  n_sim <- integer(nrow(base))
  for (variavel in regras_validade[["1"]]$variaveis) {
    valor <- texto_validade(base[[colunas[[variavel]]]])
    n_sim <- n_sim + as.integer(valor == "1")
  }
  n_sim == 0L
}

# Regra 3: nome do paciente iniciado por termos incompatíveis com pessoa viva.
avaliar_regra_3 <- function(base, colunas) {
  nome <- texto_validade(base[[colunas[["nm_pacient"]]]])
  grepl("^(FETO |ABORTO|NATIMORT|NASCITUR)", nome, perl = TRUE)
}

# Auxiliar da regra 4: procura o código afirmativo 1 em um conjunto de campos.
possui_caractere_um <- function(base, colunas, variaveis) {
  resultado <- rep(FALSE, nrow(base))
  for (variavel in variaveis) {
    valor <- texto_validade(base[[colunas[[variavel]]]])
    resultado <- resultado | grepl("1", valor, fixed = TRUE)
  }
  resultado
}

# Regra 4: homem de 20 a 59 anos fora de todas as condições de definição de caso.
avaliar_regra_4 <- function(base, colunas) {
  obter_texto <- function(variavel) texto_validade(base[[colunas[[variavel]]]])
  idade <- suppressWarnings(as.numeric(obter_texto("nu_idade_n")))

  variaveis_deficiencia <- c(
    "def_trans", "def_fisica", "def_mental", "def_visual", "def_auditi",
    "tran_ment", "tran_comp", "def_out"
  )
  variaveis_violencia <- c(
    "les_autop", "viol_sexu", "viol_tort", "viol_traf", "viol_legal"
  )
  variaveis_relacao <- c(
    "rel_pai", "rel_mae", "rel_pad", "rel_mad", "rel_conj", "rel_excon",
    "rel_namo", "rel_exnam", "rel_filho", "rel_irmao", "rel_cuida",
    "rel_pol", "rel_propri"
  )

  obter_texto("cs_sexo") == "M" &
    !is.na(idade) & idade >= 4020 & idade <= 4059 &
    obter_texto("cs_raca") != "5" &
    !obter_texto("orient_sex") %in% c("1", "2", "3") &
    !obter_texto("ident_gen") %in% c("1", "2", "3") &
    !possui_caractere_um(base, colunas, variaveis_deficiencia) &
    !possui_caractere_um(base, colunas, variaveis_violencia) &
    !possui_caractere_um(base, colunas, variaveis_relacao)
}

# Relação entre o identificador do catálogo e a função que implementa a regra.
funcoes_regras_validade <- list(
  `1` = avaliar_regra_1,
  `3` = avaliar_regra_3,
  `4` = avaliar_regra_4
)

# Orquestra dependências, execução e consolidação de todas as regras.
# Retorna indicadores por registro, resumo por regra, totais, avisos e o
# mapeamento efetivo de colunas usado em cada avaliação.
executar_analise_validade <- function(base_padronizada, variaveis_originais) {
  if (!is.data.frame(base_padronizada)) {
    stop("A base padronizada não está disponível como data.frame.", call. = FALSE)
  }
  if (is.null(variaveis_originais)) {
    stop("A relação de variáveis existentes na base original não está disponível.", call. = FALSE)
  }

  numero_registros <- nrow(base_padronizada)
  indicadores <- data.frame(id_registro = seq_len(numero_registros))
  resumo_regras <- list()
  mapeamentos <- list()
  avisos <- character()
  numero_problemas <- integer(numero_registros)

  # Regras sem requisitos completos são registradas como não executáveis; o
  # restante da análise continua para não ocultar resultados válidos.
  for (id in names(regras_validade)) {
    regra <- regras_validade[[id]]
    mapa <- mapear_dependencias_validade(
      base_padronizada, variaveis_originais, regra$variaveis
    )
    mapeamentos[[id]] <- mapa$colunas

    impedimentos <- c(
      if (length(mapa$ausentes)) paste0("Ausentes na base original: ", paste(mapa$ausentes, collapse = ", ")),
      if (length(mapa$ambiguas)) paste0("Nomes ambíguos: ", paste(mapa$ambiguas, collapse = ", "))
    )

    if (length(impedimentos)) {
      indicador <- rep(NA, numero_registros)
      status <- "Não executável"
      detalhe <- paste(impedimentos, collapse = "; ")
      avisos <- c(avisos, sprintf("Regra %s não executável — %s.", id, detalhe))
    } else {
      indicador <- funcoes_regras_validade[[id]](base_padronizada, mapa$colunas)
      indicador[is.na(indicador)] <- FALSE
      status <- "Executada"
      detalhe <- ""
    }

    indicadores[[regra$indicador]] <- indicador
    if (identical(status, "Executada")) {
      numero_problemas <- numero_problemas + as.integer(indicador)
    }
    sinalizados <- if (all(is.na(indicador))) NA_integer_ else sum(indicador, na.rm = TRUE)
    percentual <- if (is.na(sinalizados) || numero_registros == 0L) {
      if (is.na(sinalizados)) NA_real_ else 0
    } else {
      100 * sinalizados / numero_registros
    }
    resumo_regras[[id]] <- data.frame(
      regra = regra$id,
      descricao = regra$descricao,
      status = status,
      detalhe = detalhe,
      registros_sinalizados = sinalizados,
      percentual_total = percentual,
      stringsAsFactors = FALSE
    )
  }

  indicadores$numero_problemas_validade <- as.integer(numero_problemas)
  indicadores$problema_validade <- numero_problemas > 0L

  tabela_regras <- do.call(rbind, resumo_regras)
  rownames(tabela_regras) <- NULL
  registros_problematicos <- sum(indicadores$problema_validade)
  ocorrencias <- sum(indicadores$numero_problemas_validade)

  list(
    por_registro = indicadores,
    por_regra = tabela_regras,
    resumo = list(
      notificacoes = numero_registros,
      registros_problematicos = registros_problematicos,
      percentual_problematicos = if (numero_registros) 100 * registros_problematicos / numero_registros else 0,
      ocorrencias = ocorrencias
    ),
    avisos = unique(avisos),
    mapeamentos = mapeamentos
  )
}

# Monta a tabela detalhada exibida/baixada: todas as regras ou uma regra isolada.
obter_registros_validade <- function(base_padronizada, resultado, selecao = "todos") {
  por_registro <- resultado$por_registro
  if (identical(selecao, "todos")) {
    filtro <- por_registro$problema_validade
    variaveis <- c("nu_notific", "dt_notific", "nm_pacient", "cs_sexo", "nu_idade_n")
    # A visão geral usa o total por registro. Indicadores individuais seriam
    # uma duplicação larga e continuam disponíveis na seleção de cada regra.
    indicadores_mostrar <- character()
  } else {
    regra <- regras_validade[[as.character(selecao)]]
    if (is.null(regra)) stop("Regra de validade desconhecida.", call. = FALSE)
    indicador <- por_registro[[regra$indicador]]
    filtro <- !is.na(indicador) & indicador
    variaveis <- unique(c("nu_notific", "dt_notific", regra$variaveis))
    indicadores_mostrar <- regra$indicador
  }

  indices <- which(filtro)
  saida <- data.frame(id_registro = por_registro$id_registro[indices])
  for (variavel in variaveis) {
    coluna <- localizar_coluna_validade(names(base_padronizada), variavel)
    if (length(coluna) == 1L) saida[[variavel]] <- base_padronizada[[coluna]][indices]
  }
  for (indicador in indicadores_mostrar) {
    saida[[indicador]] <- por_registro[[indicador]][indices]
  }
  saida$numero_problemas_validade <- por_registro$numero_problemas_validade[indices]
  saida
}
