# Testes do modelo compartilhado de priorização ------------------------------
#
# Protegem os cortes P95, a ordem mutuamente exclusiva das categorias, os casos
# extremos e a equivalência de dados entre SVG interativo e figura estática.

criar_tabela_priorizacao_teste <- function() {
  data.frame(
    codigo = sprintf("%03d", 1:21),
    nome = paste("Local", 1:21),
    notificacoes = c(rep(500L, 20), 100L),
    registros_invalidos = c(seq(10, 180, by = 10), 200, 200, 0),
    percentual_invalidos = c(seq_len(17), 100, 100, 2, 0),
    stringsAsFactors = FALSE
  )
}

testthat::test_that("P95 e o corte absoluto adicional produzem categorias exclusivas", {
  dados <- classificar_prioridades(criar_tabela_priorizacao_teste())
  testthat::expect_equal(unique(dados$limiar_absoluto_p95), 200)
  testthat::expect_equal(unique(dados$limiar_relativo_p95), 100)
  testthat::expect_identical(dados$prioridade_id[19], "prioridade_1")
  testthat::expect_identical(dados$prioridade_id[20], "prioridade_1")
  testthat::expect_identical(dados$prioridade_id[18], "prioridade_3")
  testthat::expect_identical(dados$prioridade_id[1], "monitoramento")
  testthat::expect_identical(dados$prioridade_id[21], "sem_problemas")
})

testthat::test_that("empates exatamente no P95 recebem a mesma decisão", {
  dados <- classificar_prioridades(criar_tabela_priorizacao_teste())
  testthat::expect_true(all(dados$registros_invalidos[c(19, 20)] >=
                              dados$limiar_absoluto_p95[c(19, 20)]))
  testthat::expect_equal(dados$percentil_absoluto[c(19, 20)], c(100, 100))
})

testthat::test_that("Prioridade 3 exige trinta problemas inclusive no limite", {
  config <- configuracao_priorizacao()
  config$percentil_absoluto <- 0.75
  config$percentil_relativo <- 0.75
  tabela <- data.frame(
    notificacoes = rep(100L, 8),
    registros_invalidos = c(5, 10, 20, 29, 30, 40, 100, 100),
    percentual_invalidos = c(1, 2, 3, 100, 100, 4, 100, 5)
  )
  dados <- classificar_prioridades(tabela, config)
  testthat::expect_identical(dados$prioridade_id[4], "monitoramento")
  testthat::expect_identical(dados$prioridade_id[5], "prioridade_3")
})

testthat::test_that("notificações não alteram a categoria de prioridade", {
  tabela <- criar_tabela_priorizacao_teste()
  primeiro <- classificar_prioridades(tabela)$prioridade_id
  tabela$notificacoes <- rev(seq_len(nrow(tabela))) * 100000L
  segundo <- classificar_prioridades(tabela)$prioridade_id
  testthat::expect_identical(segundo, primeiro)
})

testthat::test_that("ausências, zeros e território sem problemas são seguros", {
  tabela <- data.frame(
    notificacoes = c(0, 10, NA),
    registros_invalidos = c(0, NA, -1),
    percentual_invalidos = c(NaN, NA, Inf)
  )
  dados <- classificar_prioridades(tabela)
  testthat::expect_true(all(dados$prioridade_id == "sem_problemas"))
  testthat::expect_true(all(dados$limiar_absoluto_p95 == 0))
  testthat::expect_true(all(dados$limiar_relativo_p95 == 0))
})

testthat::test_that("território com uma localidade é determinístico", {
  tabela <- data.frame(
    notificacoes = 100L, registros_invalidos = 12L, percentual_invalidos = 12
  )
  dados <- classificar_prioridades(tabela)
  testthat::expect_equal(dados$limiar_absoluto_p95, 12)
  testthat::expect_equal(dados$limiar_relativo_p95, 12)
  testthat::expect_identical(dados$prioridade_id, "prioridade_1")
  testthat::expect_equal(dados$percentil_absoluto, 100)
})

testthat::test_that("muitos zeros e extremo grande não deformam os cálculos", {
  tabela <- data.frame(
    notificacoes = rep(100L, 103),
    registros_invalidos = c(rep(0, 100), 1, 2, 10000),
    percentual_invalidos = c(rep(0, 100), 1, 2, 99)
  )
  dados <- classificar_prioridades(tabela)
  testthat::expect_gt(dados$limiar_absoluto_p95[1], 2)
  testthat::expect_identical(dados$prioridade_id[103], "prioridade_1")
  testthat::expect_true(all(dados$prioridade_id[1:100] == "sem_problemas"))
})

testthat::test_that("transformação visual preserva os valores originais", {
  tabela <- criar_tabela_priorizacao_teste()
  preparada <- preparar_tabela_prioridades(tabela, "nome", "codigo")
  antes <- preparada[, c("registros_invalidos", "percentual_invalidos")]
  grafico <- grafico_matriz_prioridade(preparada, "nome_prioridade", "Teste")
  testthat::expect_s3_class(grafico, "ggplot")
  testthat::expect_identical(
    preparada[, c("registros_invalidos", "percentual_invalidos")], antes
  )
  testthat::expect_equal(nrow(ggplot2::ggplot_build(grafico)$data[[1]]), 20L)
})

testthat::test_that("matriz estática usa círculos e não desenha linhas de corte", {
  preparada <- preparar_tabela_prioridades(
    criar_tabela_priorizacao_teste(), "nome", "codigo"
  )
  original <- grafico_matriz_prioridade(
    preparada, "nome_prioridade", "Original", escala = "original"
  )
  transformada <- grafico_matriz_prioridade(
    preparada, "nome_prioridade", "Transformada", escala = "transformada"
  )
  testthat::expect_length(original$layers, 2L)
  testthat::expect_s3_class(original$layers[[1]]$geom, "GeomPoint")
  testthat::expect_identical(original$layers[[1]]$aes_params$shape, 16)
  testthat::expect_identical(
    original$scales$get_scales("x")$trans$name, "identity"
  )
  testthat::expect_false(identical(
    transformada$scales$get_scales("x")$trans$name, "identity"
  ))
})

testthat::test_that("pseudo-log amplia a separação próxima da origem", {
  valores <- c(0, 1, 2, 5, 10, 1000)
  pseudo <- scales::pseudo_log_trans(sigma = 1, base = 10)$transform(valores)
  linear <- valores
  pseudo <- pseudo / max(pseudo)
  linear <- linear / max(linear)
  testthat::expect_gt(diff(range(pseudo[1:5])), diff(range(linear[1:5])))
  testthat::expect_equal(pseudo[1], 0)
})

testthat::test_that("SVG, tooltip e tabela recebem a mesma classificação", {
  preparada <- preparar_tabela_prioridades(
    criar_tabela_priorizacao_teste(), "nome", "codigo"
  )
  carga <- dados_json_matriz_prioridade(preparada, "Matriz teste", "validade")
  testthat::expect_identical(carga$dados$prioridade_id, preparada$prioridade_id)
  testthat::expect_identical(carga$dados$nome, preparada$nome_prioridade)
  testthat::expect_identical(carga$visual$escala_inicial, "transformada")
  grafico <- grafico_matriz_prioridade(
    preparada, "nome_prioridade", "Matriz teste", "validade"
  )
  dados_estaticos <- ggplot2::ggplot_build(grafico)$plot$data
  testthat::expect_identical(
    as.character(dados_estaticos$prioridade_id),
    as.character(carga$dados$prioridade_id[carga$dados$problemas > 0])
  )
  testthat::expect_equal(
    dados_estaticos$registros_invalidos,
    carga$dados$problemas[carga$dados$problemas > 0]
  )
  bloco <- as.character(bloco_matriz_prioridade_relatorio(
    criar_tabela_priorizacao_teste(), "nome", "codigo", "Matriz teste", "validade"
  ))
  testthat::expect_match(bloco, "svg-matriz-prioridade", fixed = TRUE)
  testthat::expect_match(bloco, "Mostrar somente prioridades", fixed = TRUE)
  testthat::expect_match(bloco, "data-escala=\"original\"", fixed = TRUE)
  testthat::expect_match(bloco, "data-escala=\"transformada\"", fixed = TRUE)
  testthat::expect_match(bloco, "tabela-prioridades", fixed = TRUE)
  testthat::expect_match(bloco, "matriz-estatica-impressao", fixed = TRUE)
  testthat::expect_false(grepl("https://", bloco, fixed = TRUE))
})

testthat::test_that("HTML prepara as duas escalas e alterna somente a camada", {
  codigo <- javascript_matrizes_prioridade()
  testthat::expect_match(codigo, "camadaOriginal = camada.cloneNode(true)", fixed = TRUE)
  testthat::expect_match(codigo, "grupo.style.display", fixed = TRUE)
  testthat::expect_match(codigo, "var forma = elemento('circle'", fixed = TRUE)
  testthat::expect_match(codigo, "area-svg-prioridade", fixed = TRUE)
  testthat::expect_false(grepl("linhaCorte", codigo, fixed = TRUE))
  testthat::expect_false(grepl("P95 absoluto", codigo, fixed = TRUE))
  testthat::expect_false(grepl("P95 relativo", codigo, fixed = TRUE))
  testthat::expect_match(codigo, "passosLineares(maxX, 6)", fixed = TRUE)
  testthat::expect_match(codigo, "posicionarRotulo", fixed = TRUE)
  testthat::expect_match(codigo, "em-destaque", fixed = TRUE)
  testthat::expect_match(codigo, "esmaecida", fixed = TRUE)
  testthat::expect_match(
    codigo, "evento.target.classList.contains('ponto-prioridade')", fixed = TRUE
  )
})

testthat::test_that("escala inicial é validada explicitamente", {
  testthat::expect_identical(validar_escala_matriz_prioridade("original"), "original")
  testthat::expect_identical(
    validar_escala_matriz_prioridade("transformada"), "transformada"
  )
  testthat::expect_error(
    validar_escala_matriz_prioridade("log"),
    "original.*transformada"
  )
})

testthat::test_that("Validade e Consistência compartilham o mesmo classificador", {
  testthat::expect_true("modulo" %in% names(formals(bloco_matriz_prioridade_relatorio)))
  testthat::expect_false(any(c("base_padronizada", "resultado_validade",
                               "resultado_consistencia") %in%
                              names(formals(classificar_prioridades))))
  testthat::expect_true("R/priorizacao_relatorio.R" %in%
    arquivos_recarregamento_desenvolvimento("relatorio_validade"))
  testthat::expect_true("R/priorizacao_relatorio.R" %in%
    arquivos_recarregamento_desenvolvimento("relatorio_consistencia"))
})
