# Testes da apresentação interativa dos gráficos de Pareto --------------------

criar_tabela_pareto_interativo_teste <- function() {
  data.frame(
    nome_exibicao = c("Local A", "Local B", "Local C"),
    notificacoes = c(100L, 80L, 40L),
    registros_invalidos = c(20L, 10L, 5L),
    percentual_invalidos = c(20, 12.5, 12.5),
    stringsAsFactors = FALSE
  )
}

test_that("Pareto interativo incorpora indicadores já agregados no SVG", {
  tabela <- criar_tabela_pareto_interativo_teste()
  html <- as.character(pareto_interativo_relatorio(
    tabela, "nome_exibicao", "Pareto de teste", "validade"
  ))

  expect_match(html, "svg-pareto-interativo", fixed = TRUE)
  expect_match(html, "barra-pareto-interativa", fixed = TRUE)
  expect_match(html, "linha-pareto-interativa", fixed = TRUE)
  expect_match(html, "segmento-interacao-pareto", fixed = TRUE)
  expect_match(html, "data-nome=\"Local A\"", fixed = TRUE)
  expect_match(html, "data-absoluto=\"20\"", fixed = TRUE)
  expect_match(html, "data-percentual=\"20\"", fixed = TRUE)
  expect_match(html, "data-acumulado=", fixed = TRUE)
  expect_match(html, "tooltip-pareto-interativo", fixed = TRUE)
})

test_that("Pareto de consistência usa a terminologia do módulo", {
  tabela <- criar_tabela_pareto_interativo_teste()
  html <- as.character(pareto_interativo_relatorio(
    tabela, "nome_exibicao", "Pareto de teste", "consistencia"
  ))

  expect_match(
    html,
    "data-rotulo-absoluto=\"Registros inconsistentes\"",
    fixed = TRUE
  )
  expect_match(
    html,
    "data-rotulo-percentual=\"Percentual inconsistente\"",
    fixed = TRUE
  )
})

test_that("configuração central controla a proporção vertical do Pareto", {
  dimensoes <- dimensoes_figura_relatorio("pareto")
  expect_equal(dimensoes$altura, 9.5)
  expect_gt(dimensoes$altura, 7.5)
})

test_that("folha de estilos posiciona verticalmente a escala dos mapas", {
  css <- css_relatorio_validade()
  expect_match(
    css,
    "grid-template-rows:auto auto 190px auto",
    fixed = TRUE
  )
  expect_match(
    css,
    "background:linear-gradient(0deg",
    fixed = TRUE
  )
  expect_match(javascript_paretos_interativos(), "pointerenter", fixed = TRUE)
})
