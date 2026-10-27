# Importação de arquivos DBF ---------------------------------------------------
#
# Lê bases no formato dBase por meio do pacote `foreign`, preservando textos
# como caracteres (`as.is = TRUE`) para que a padronização trate a codificação.

importar_dbf <- function(caminho) {
  validar_arquivo_leitura(caminho)
  # Avisos na leitura são promovidos a erro: um DBF parcialmente interpretado
  # não deve avançar silenciosamente para a etapa de padronização.
  dados <- tryCatch(
    foreign::read.dbf(caminho, as.is = TRUE),
    warning = function(w) {
      stop(sprintf("Aviso ao ler o DBF: %s", conditionMessage(w)), call. = FALSE)
    }
  )
  dados <- as.data.frame(dados, check.names = FALSE, stringsAsFactors = FALSE)
  validar_base_importada(dados)
  dados
}
