# Gerenciamento de memória e artefatos temporários ----------------------------
#
# Reúne operações pequenas e seguras usadas nas fronteiras entre importação,
# análise e relatório. O coletor é acionado somente após referências grandes
# serem removidas, evitando pausas repetidas dentro de cálculos vetoriais curtos.

# Solicita ao R a devolução de páginas não utilizadas após uma etapa de grande
# alocação. `completa = FALSE` é apropriado para liberações intermediárias.
liberar_memoria_ram <- function(completa = FALSE) {
  invisible(gc(verbose = FALSE, full = isTRUE(completa)))
}

# Retém apenas metadados pequenos da padronização. A base final possui um estado
# próprio na sessão e não deve permanecer duplicada dentro deste resultado.
compactar_resultado_padronizacao <- function(resultado) {
  resultado$base_padronizada <- NULL
  resultado
}

# O resumo e o período bastam para a interface depois que `tp_unid` foi anexada.
# A fotografia CNES e a segunda referência à base enriquecida são descartadas.
compactar_resultado_enriquecimento <- function(resultado) {
  list(resumo = resultado$resumo, periodo = resultado$periodo)
}

# Após o HTML/PDF existir em disco, a sessão precisa somente de caminhos e dos
# metadados exibidos. Esta função também protege contra extensões futuras do
# gerador que voltem a incluir tabelas grandes no retorno.
compactar_artefato_relatorio <- function(artefato) {
  artefato$dados_agregados <- NULL
  artefato
}

# Exclui apenas diretórios de artefatos localizados sob a pasta temporária desta
# sessão do R. Caminhos de homologação ou saídas escolhidas pelo usuário nunca
# são removidos por esta rotina.
remover_arquivos_temporarios_relatorio <- function(artefato, raiz = tempdir()) {
  if (is.null(artefato) || !is.list(artefato)) return(invisible(FALSE))
  caminhos <- unlist(
    artefato[intersect(c("caminho_html", "caminho_pdf"), names(artefato))],
    use.names = FALSE
  )
  caminhos <- caminhos[!is.na(caminhos) & nzchar(caminhos)]
  if (!length(caminhos)) return(invisible(FALSE))

  raiz_normalizada <- normalizePath(raiz, winslash = "/", mustWork = TRUE)
  prefixo_raiz <- paste0(tolower(raiz_normalizada), "/")
  diretorios <- unique(dirname(caminhos))
  removido <- FALSE
  for (diretorio in diretorios) {
    diretorio_normalizado <- normalizePath(
      diretorio, winslash = "/", mustWork = FALSE
    )
    dentro_da_raiz <- startsWith(tolower(diretorio_normalizado), prefixo_raiz)
    if (dentro_da_raiz && dir.exists(diretorio_normalizado)) {
      unlink(diretorio_normalizado, recursive = TRUE, force = TRUE)
      removido <- TRUE
    }
  }
  invisible(removido)
}
