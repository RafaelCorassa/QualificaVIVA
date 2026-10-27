# Testes dos leitores e das validações compartilhadas de importação.
# Abrangem detecção de formato, parâmetros, estruturas inválidas e arquivos reais
# temporários nos formatos suportados.
test_that("a extensão é reconhecida sem diferenciar maiúsculas", {
  expect_equal(identificar_formato("base.CSV"), "csv")
  expect_equal(identificar_formato("base.dbf"), "dbf")
  expect_equal(identificar_formato("base.DTA"), "dta")
  expect_equal(identificar_formato("base.xls"), "xls")
  expect_equal(identificar_formato("base.XLSX"), "xlsx")
})

test_that("formatos não permitidos são rejeitados", {
  expect_error(identificar_formato("base.txt"), "Formato não suportado")
  expect_error(identificar_formato("sem_extensao"), "Formato não suportado")
})

test_that("CSV é importado com diferentes separadores", {
  csv_ponto_virgula <- tempfile(fileext = ".csv")
  csv_virgula <- tempfile(fileext = ".csv")
  writeLines(c("codigo;nome", "1;João", "2;Maria"), csv_ponto_virgula,
             useBytes = TRUE)
  writeLines(c("codigo,nome", "1,Ana", "2,Caio"), csv_virgula,
             useBytes = TRUE)

  dados_pv <- importar_csv(csv_ponto_virgula, separador = ";", codificacao = "UTF-8")
  dados_v <- importar_csv(csv_virgula, separador = ",", codificacao = "UTF-8")
  expect_equal(names(dados_pv), c("codigo", "nome"))
  expect_equal(nrow(dados_pv), 2L)
  expect_equal(names(dados_v), c("codigo", "nome"))
  expect_equal(nrow(dados_v), 2L)
})

test_that("configuração incorreta de CSV gera aviso de uma coluna", {
  arquivo <- tempfile(fileext = ".csv")
  writeLines(c("a;b", "1;2"), arquivo)
  resultado <- ler_csv_configurado(arquivo, separador = ",", codificacao = "UTF-8")
  expect_equal(resultado$numero_colunas, 1L)
  expect_match(resultado$avisos, "apenas uma coluna")
})

test_that("parâmetros CSV inválidos são rejeitados de forma explícita", {
  expect_error(validar_parametros_csv("::", "\""), "exatamente um caractere")
  expect_error(validar_parametros_csv(";", "`"), "não é válido")
})

test_that("XLS e XLSX são listados e importados", {
  arquivo_xls <- readxl::readxl_example("datasets.xls")
  arquivo_xlsx <- readxl::readxl_example("datasets.xlsx")
  for (arquivo in c(arquivo_xls, arquivo_xlsx)) {
    planilhas <- listar_planilhas_excel(arquivo)
    expect_true(length(planilhas) >= 1L)
    dados <- importar_excel(arquivo, planilhas[[1]])
    expect_s3_class(dados, "data.frame")
    expect_gt(ncol(dados), 0L)
  }
})

test_that("DBF é importado sem alterar nomes e valores", {
  arquivo <- tempfile(fileext = ".dbf")
  original <- data.frame(CODIGO = c(1, 2), TEXTO = c("A", "B"), check.names = FALSE)
  foreign::write.dbf(original, arquivo)
  dados <- importar_dbf(arquivo)
  expect_equal(names(dados), names(original))
  expect_equal(as.numeric(dados$CODIGO), as.numeric(original$CODIGO))
  expect_equal(as.character(dados$TEXTO), as.character(original$TEXTO))
})

test_that("DTA moderno é importado preservando nomes, rótulos e valores", {
  arquivo <- tempfile(fileext = ".dta")
  categoria <- haven::labelled(c(1, 2), labels = c(Sim = 1, Não = 2))
  original <- data.frame(CODIGO = c(10, 20), CATEGORIA = categoria,
                         TEXTO = c("Ação", "Saúde"), check.names = FALSE)
  haven::write_dta(original, arquivo, version = 15)

  dados <- importar_dta(arquivo)
  expect_equal(names(dados), names(original))
  expect_equal(as.numeric(dados$CODIGO), as.numeric(original$CODIGO))
  expect_equal(as.numeric(dados$CATEGORIA), as.numeric(original$CATEGORIA))
  expect_equal(attr(dados$CATEGORIA, "labels"), attr(original$CATEGORIA, "labels"))
  expect_equal(as.character(dados$TEXTO), as.character(original$TEXTO))
})

test_that("arquivo vazio e arquivos inválidos são tratados", {
  vazio <- tempfile(fileext = ".csv")
  file.create(vazio)
  expect_error(importar_csv(vazio), "vazio")

  invalido <- tempfile(fileext = ".xlsx")
  writeLines("não é uma planilha", invalido)
  expect_error(listar_planilhas_excel(invalido))

  dta_invalido <- tempfile(fileext = ".dta")
  writeLines("não é um arquivo Stata", dta_invalido)
  expect_error(importar_dta(dta_invalido))
})
