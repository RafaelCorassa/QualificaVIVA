# Testes de integração entre a interface Shiny e o módulo de consistência.
# Confirmam que a navegação preserva a sessão e que a análise usa a base atual.
test_that("navegação preserva a sessão e a consistência usa a base padronizada", {
  ambiente_app <- new.env(parent = globalenv())
  caminho_app <- testthat::test_path("..", "..", "app.R")
  suppressWarnings(suppressPackageStartupMessages(
    sys.source(caminho_app, envir = ambiente_app, chdir = TRUE)
  ))

  html_ui <- paste(as.character(ambiente_app$ui), collapse = "")
  expect_match(html_ui, ">Sobre<", fixed = TRUE)
  expect_match(html_ui, "OpenAI Codex", fixed = TRUE)
  expect_match(html_ui, "rodape-aplicativo", fixed = TRUE)

  variaveis <- unique(unlist(lapply(ambiente_app$regras_consistencia, `[[`, "variaveis"),
                            use.names = FALSE))
  base_teste <- as.data.frame(
    stats::setNames(rep(list(""), length(variaveis)), variaveis),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  base_teste$cs_sexo <- "M"
  base_teste$cs_gestant <- "5"

  shiny::testServer(ambiente_app$server, {
    base_bruta(data.frame(ORIGINAL = "preservada", check.names = FALSE))
    base_padronizada(base_teste)
    resultado_padronizacao(list(variaveis_originais = names(base_teste)))

    session$setInputs(abrir_consistencia = 1)
    session$flushReact()
    expect_equal(base_bruta()$ORIGINAL, "preservada")
    expect_identical(base_padronizada(), base_teste)

    session$setInputs(executar_consistencia = 1)
    session$flushReact()
    expect_true(resultado_consistencia()$por_registro$regra_4)

    session$setInputs(consistencia_validade = 1)
    session$flushReact()
    expect_true(resultado_consistencia()$por_registro$regra_4)
    expect_identical(base_padronizada(), base_teste)
  })
})

test_that("controles territoriais incluem Brasil e municípios e limpam todas as escolhas", {
  ambiente_app <- new.env(parent = globalenv())
  caminho_app <- testthat::test_path("..", "..", "app.R")
  suppressWarnings(suppressPackageStartupMessages(
    sys.source(caminho_app, envir = ambiente_app, chdir = TRUE)
  ))

  shiny::testServer(ambiente_app$server, {
    variaveis <- unique(c(
      unlist(lapply(ambiente_app$regras_validade, `[[`, "variaveis"), use.names = FALSE),
      unlist(lapply(ambiente_app$regras_consistencia, `[[`, "variaveis"), use.names = FALSE),
      "sg_uf_not", "id_municip", "id_unidade"
    ))
    base_teste <- as.data.frame(
      stats::setNames(rep(list(""), length(variaveis)), variaveis),
      stringsAsFactors = FALSE, check.names = FALSE
    )
    base_teste$sg_uf_not <- "35"
    base_teste$id_municip <- "355030"
    base_teste$id_unidade <- "0000001"
    base_padronizada(base_teste)
    resultado_validade(ambiente_app$executar_analise_validade(
      base_teste, names(base_teste)
    ))
    resultado_consistencia(ambiente_app$executar_analise_consistencia(
      base_teste, names(base_teste)
    ))
    session$flushReact()

    ui_validade <- paste(as.character(output$relatorio_validade_ui), collapse = "")
    ui_consistencia <- paste(
      as.character(output$relatorio_consistencia_ui), collapse = ""
    )
    expect_match(ui_validade, 'id="relatorio_incluir_brasil"', fixed = TRUE)
    expect_match(ui_validade, 'id="relatorio_municipios"', fixed = TRUE)
    expect_match(
      ui_consistencia, 'id="relatorio_consistencia_incluir_brasil"',
      fixed = TRUE
    )
    expect_match(
      ui_consistencia, 'id="relatorio_consistencia_municipios"', fixed = TRUE
    )

    mensagens <- list()
    session$sendInputMessage <- function(inputId, message) {
      mensagens[[inputId]] <<- message
    }
    session$setInputs(
      relatorio_incluir_brasil = TRUE,
      relatorio_regioes = "3", relatorio_ufs = "35",
      relatorio_municipios = "355030", limpar_selecao_relatorio = 1
    )
    session$flushReact()
    expect_false(mensagens$relatorio_incluir_brasil$value)
    expect_length(mensagens$relatorio_regioes$value, 0L)
    expect_length(mensagens$relatorio_ufs$value, 0L)
    expect_length(mensagens$relatorio_municipios$value, 0L)

    mensagens <- list()
    session$setInputs(
      relatorio_consistencia_incluir_brasil = TRUE,
      relatorio_consistencia_regioes = "3", relatorio_consistencia_ufs = "35",
      relatorio_consistencia_municipios = "355030",
      consistencia_limpar_relatorio = 1
    )
    session$flushReact()
    expect_false(mensagens$relatorio_consistencia_incluir_brasil$value)
    expect_length(mensagens$relatorio_consistencia_regioes$value, 0L)
    expect_length(mensagens$relatorio_consistencia_ufs$value, 0L)
    expect_length(mensagens$relatorio_consistencia_municipios$value, 0L)

    session$setInputs(
      relatorio_incluir_brasil = FALSE,
      relatorio_regioes = character(), relatorio_ufs = character(),
      relatorio_municipios = character(), gerar_relatorio_validade = 1
    )
    session$flushReact()
    expect_match(erro_relatorio(), "Selecione ao menos uma abrangência")

    session$setInputs(
      relatorio_consistencia_incluir_brasil = FALSE,
      relatorio_consistencia_regioes = character(),
      relatorio_consistencia_ufs = character(),
      relatorio_consistencia_municipios = character(),
      gerar_relatorio_consistencia = 1
    )
    session$flushReact()
    expect_match(
      erro_relatorio_consistencia(), "Selecione ao menos uma abrangência"
    )
  })
})
