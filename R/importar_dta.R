# Importação de arquivos Stata (DTA) ------------------------------------------
#
# O pacote `haven` oferece suporte às versões modernas do formato Stata e
# mantém rótulos/metadados nas colunas. A conversão para `data.frame` preserva
# nomes exatamente como encontrados, pois eles serão normalizados depois.

importar_dta <- function(caminho) {
  validar_arquivo_leitura(caminho)

  dados <- haven::read_dta(
    file = caminho,
    encoding = NULL,
    .name_repair = "minimal"
  )
  dados <- as.data.frame(dados, check.names = FALSE, stringsAsFactors = FALSE)
  validar_base_importada(dados)
  dados
}
