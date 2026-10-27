# Testes dos mapas SVG usados nos relatórios HTML -----------------------------
#
# Os testes confirmam que a interatividade reutiliza os indicadores agregados,
# respeita o recorte regional e mantém uma alternativa estática para impressão.

test_that("mapa interativo nacional contém todas as UFs e os dois indicadores", {
  skip_if_not_installed("sf")
  referencias <- carregar_referencias_territoriais(
    testthat::test_path("..", "..", "data", "reference")
  )
  tabela <- referencias$ufs
  tabela$registros_invalidos <- seq_len(nrow(tabela))
  tabela$percentual_invalidos <- seq_len(nrow(tabela)) / 10

  mapa <- preparar_dados_mapa_interativo_relatorio(
    referencias$geografia_ufs, tabela
  )
  expect_equal(nrow(mapa), 27L)
  expect_equal(mapa$registros_invalidos, tabela$registros_invalidos[
    match(mapa$codigo_uf, tabela$codigo_uf)
  ])
  expect_true(all(nzchar(mapa$caminho_svg)))

  html <- as.character(mapa_interativo_ufs_relatorio(
    mapa, "registros_invalidos", "Brasil — mapa de teste", "validade"
  ))
  expect_equal(lengths(regmatches(
    html, gregexpr("class=\"estado-mapa-interativo\"", html, fixed = TRUE)
  )), 27L)
  expect_match(html, "data-absoluto=\"1\"", fixed = TRUE)
  expect_match(html, "data-percentual=\"0.1\"", fixed = TRUE)
  expect_match(html, "tooltip-mapa-interativo", fixed = TRUE)

  html_consistencia <- as.character(mapa_interativo_ufs_relatorio(
    mapa, "percentual_invalidos", "Brasil — mapa de teste", "consistencia"
  ))
  expect_match(
    html_consistencia,
    "data-rotulo-absoluto=\"Registros inconsistentes\"",
    fixed = TRUE
  )
  expect_match(
    html_consistencia,
    "data-rotulo-percentual=\"Percentual inconsistente\"",
    fixed = TRUE
  )
})

test_that("mapa regional inclui somente as UFs da região escolhida", {
  skip_if_not_installed("sf")
  referencias <- carregar_referencias_territoriais(
    testthat::test_path("..", "..", "data", "reference")
  )
  tabela <- referencias$ufs
  tabela$registros_invalidos <- seq_len(nrow(tabela))
  tabela$percentual_invalidos <- seq_len(nrow(tabela)) / 10

  sudeste <- preparar_dados_mapa_interativo_relatorio(
    referencias$geografia_ufs, tabela, "3"
  )
  expect_setequal(sudeste$sigla_uf, c("ES", "MG", "RJ", "SP"))
  expect_equal(nrow(sudeste), 4L)
})

test_that("bloco de mapa separa HTML interativo da imagem de impressão", {
  skip_if_not_installed("sf")
  referencias <- carregar_referencias_territoriais(
    testthat::test_path("..", "..", "data", "reference")
  )
  tabela <- referencias$ufs
  tabela$registros_invalidos <- seq_len(nrow(tabela))
  tabela$percentual_invalidos <- seq_len(nrow(tabela)) / 10
  dados <- list(geografia_ufs = referencias$geografia_ufs)

  html <- as.character(bloco_mapa_relatorio(dados, tabela))
  expect_match(html, "mapa-interativo somente-tela", fixed = TRUE)
  expect_match(html, "mapa-estatico-impressao", fixed = TRUE)
  expect_match(html, "data-rotulo-absoluto=\"Registros inválidos\"", fixed = TRUE)
  expect_match(javascript_mapas_interativos(), "pointerenter", fixed = TRUE)
  expect_match(css_relatorio_validade(), ".mapa-estatico-impressao", fixed = TRUE)

  html_consistencia <- as.character(bloco_mapa_consistencia_relatorio(dados, tabela))
  expect_match(
    html_consistencia,
    "data-rotulo-absoluto=\"Registros inconsistentes\"",
    fixed = TRUE
  )
  expect_match(html_consistencia, "mapa-estatico-impressao", fixed = TRUE)
})
