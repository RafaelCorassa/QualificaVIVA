# Testes de descarte incremental de objetos e arquivos temporários ------------

test_that("resultados mantidos pela sessão não conservam cópias das bases", {
  base <- data.frame(a = rep("valor", 100L), stringsAsFactors = FALSE)
  padronizacao <- compactar_resultado_padronizacao(list(
    base_padronizada = base, resumo = list(registros = 100L)
  ))
  enriquecimento <- compactar_resultado_enriquecimento(list(
    base_enriquecida = base,
    referencia_cnes = data.frame(CNES = "0000001"),
    resumo = list(registros = 100L),
    periodo = list(competencia = "202501")
  ))
  artefato <- compactar_artefato_relatorio(list(
    dados_agregados = list(tabela = base), caminho_html = "relatorio.html"
  ))

  expect_false("base_padronizada" %in% names(padronizacao))
  expect_identical(names(enriquecimento), c("resumo", "periodo"))
  expect_false("dados_agregados" %in% names(artefato))
})

test_that("diretórios temporários de relatórios são removidos com segurança", {
  diretorio <- tempfile("artefato-memoria-", tmpdir = tempdir())
  dir.create(diretorio)
  caminho <- file.path(diretorio, "relatorio.html")
  writeLines("relatório", caminho, useBytes = TRUE)

  expect_true(remover_arquivos_temporarios_relatorio(list(caminho_html = caminho)))
  expect_false(dir.exists(diretorio))
})

test_that("Shiny descarta a base bruta e a cópia interna após padronizar", {
  ambiente_app <- new.env(parent = globalenv())
  caminho_app <- testthat::test_path("..", "..", "app.R")
  suppressWarnings(suppressPackageStartupMessages(
    sys.source(caminho_app, envir = ambiente_app, chdir = TRUE)
  ))
  ambiente_app$recarregar_modulos_desenvolvimento <- function(...) invisible(FALSE)

  shiny::testServer(ambiente_app$server, {
    bruta <- data.frame(
      NU_ANO = c("2024", "2025"),
      ID_UNIDADE = c("1", "2"),
      DT_NOTIFIC = c("20240101", "20250201"),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
    base_bruta(bruta)
    previa_base_bruta(bruta)
    metadados_importados(list(formato = "DTA"))

    session$setInputs(padronizar_base = 1, codificacao_padronizacao = "auto")
    session$flushReact()

    expect_null(base_bruta())
    expect_s3_class(base_padronizada(), "data.frame")
    expect_false("base_padronizada" %in% names(resultado_padronizacao()))
    expect_equal(nrow(previa_base_bruta()), 2L)
  })
})
