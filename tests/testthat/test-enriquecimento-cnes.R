# Testes do enriquecimento mensal com estabelecimentos do CNES ---------------

test_that("ano vem de nu_ano e mês somente dos registros desse ano", {
  base <- data.frame(
    nu_ano = c("2023", "2024", "2025", "2025", "2025", "inválido"),
    dt_notific = c(
      "20231231", "20261231", "2025-03-10", "2024-10-02", "", "20250140"
    ),
    stringsAsFactors = FALSE
  )

  periodo <- determinar_periodo_cnes(base)

  expect_identical(periodo$ano_referencia, 2025L)
  expect_identical(periodo$mes_referencia, 10L)
  expect_identical(periodo$competencia, "202510")
  expect_identical(periodo$anos_invalidos, 1L)
  expect_identical(periodo$datas_ausentes, 1L)
  expect_identical(periodo$datas_invalidas, 1L)
  expect_identical(periodo$divergencias_ano_data, 2L)
})

test_that("ausência de ano ou mês válido interrompe com mensagem específica", {
  expect_error(
    determinar_periodo_cnes(data.frame(dt_notific = as.Date("2025-01-01"))),
    "'nu_ano' está ausente",
    fixed = TRUE
  )
  expect_error(
    determinar_periodo_cnes(data.frame(
      nu_ano = c("", "20X5"), dt_notific = c("20250101", "20250201")
    )),
    "não possui nenhum ano válido"
  )
  expect_error(
    determinar_periodo_cnes(data.frame(
      nu_ano = c("2024", "2025"), dt_notific = c("20241201", "data inválida")
    )),
    "não há 'dt_notific' válida.*2025"
  )
})

test_that("somente uma competência CNES-ST é solicitada e validada", {
  chamadas <- new.env(parent = emptyenv())
  chamadas$total <- 0L
  chamadas$argumentos <- NULL
  download_falso <- function(...) {
    chamadas$total <- chamadas$total + 1L
    chamadas$argumentos <- list(...)
    data.frame(
      CNES = c("123", "0000123", "7654321"),
      COMPETEN = c("202510", "202510", "202510"),
      TP_UNID = c("01", "01", "02"),
      OUTRA = 1:3,
      stringsAsFactors = FALSE
    )
  }
  periodo <- list(
    ano_referencia = 2025L, mes_referencia = 10L, competencia = "202510"
  )

  resultado <- baixar_referencia_cnes(periodo, download_falso)

  expect_identical(chamadas$total, 1L)
  expect_identical(chamadas$argumentos, list(
    year_start = 2025L,
    month_start = 10L,
    year_end = 2025L,
    month_end = 10L,
    uf = "all",
    information_system = "CNES-ST",
    vars = c("CNES", "COMPETEN", "TP_UNID"),
    stop_on_error = TRUE
  ))
  expect_identical(names(resultado$referencia), c("CNES", "COMPETEN", "TP_UNID"))
  expect_identical(resultado$referencia$CNES, c("0000123", "7654321"))
  expect_identical(resultado$duplicacoes_reduzidas, 1L)
})

test_that("competência divergente e TP_UNID conflitante interrompem", {
  expect_error(
    preparar_referencia_cnes(data.frame(
      CNES = "1", COMPETEN = "202509", TP_UNID = "01"
    ), "202510"),
    "não corresponde integralmente à competência 202510"
  )
  expect_error(
    preparar_referencia_cnes(data.frame(
      CNES = c("1", "0000001"),
      COMPETEN = c("202510", "202510"),
      TP_UNID = c("01", "02")
    ), "202510"),
    "valores conflitantes de TP_UNID.*0000001"
  )
})

test_that("linkage usa apenas id_unidade igual a CNES e preserva linhas", {
  base <- data.frame(
    id_unidade = c("123", "7654321", "9999999", ""),
    nu_ano = c("2023", "2024", "2025", "2025"),
    dt_notific = as.Date(c("2023-01-01", "2024-02-01", "2025-10-01", NA)),
    valor = c("a", "b", "c", "d"),
    stringsAsFactors = FALSE
  )
  referencia <- data.frame(
    CNES = c("0000123", "7654321"),
    COMPETEN = c("202510", "202510"),
    TP_UNID = c("01", "02"),
    stringsAsFactors = FALSE
  )

  resultado <- relacionar_base_cnes(base, referencia)

  expect_identical(nrow(resultado$base_enriquecida), nrow(base))
  expect_identical(resultado$base_enriquecida$valor, base$valor)
  expect_identical(
    names(resultado$base_enriquecida), c(names(base), "tp_unid")
  )
  expect_identical(resultado$base_enriquecida$tp_unid, c("01", "02", NA, NA))
  expect_false(any(c("CNES", "COMPETEN") %in% names(resultado$base_enriquecida)))
  expect_identical(resultado$resumo$unidades_localizadas, 2L)
  expect_identical(resultado$resumo$unidades_nao_localizadas, 1L)
  expect_identical(resultado$resumo$ids_ausentes_ou_invalidos, 1L)

  # Depois de escolhida a fotografia, nenhuma variável temporal participa do vínculo.
  base_alterada <- base
  base_alterada$nu_ano <- rev(base_alterada$nu_ano)
  base_alterada$dt_notific <- rev(base_alterada$dt_notific)
  referencia_alterada <- referencia
  referencia_alterada$COMPETEN <- "199901"
  repetido <- relacionar_base_cnes(base_alterada, referencia_alterada)
  expect_identical(repetido$base_enriquecida$tp_unid,
                   resultado$base_enriquecida$tp_unid)
})

test_that("a mesma fotografia enriquece registros de todos os anos", {
  base <- data.frame(
    id_unidade = c("1", "1", "2"),
    nu_ano = c("2023", "2024", "2025"),
    dt_notific = as.Date(c("2023-01-01", "2024-07-01", "2025-10-01")),
    stringsAsFactors = FALSE
  )
  chamadas <- 0L
  download_falso <- function(...) {
    chamadas <<- chamadas + 1L
    data.frame(
      CNES = c("1", "2"), COMPETEN = "202510", TP_UNID = c("05", "07")
    )
  }

  resultado <- enriquecer_base_cnes(base, download_falso)

  expect_identical(chamadas, 1L)
  expect_identical(resultado$periodo$competencia, "202510")
  expect_identical(resultado$base_enriquecida$tp_unid, c("05", "05", "07"))
  expect_identical(names(resultado$referencia_cnes), c("CNES", "COMPETEN", "TP_UNID"))
})

test_that("integração Shiny substitui a base somente após enriquecimento bem-sucedido", {
  ambiente_app <- new.env(parent = globalenv())
  caminho_app <- testthat::test_path("..", "..", "app.R")
  suppressWarnings(suppressPackageStartupMessages(
    sys.source(caminho_app, envir = ambiente_app, chdir = TRUE)
  ))
  ambiente_app$enriquecer_base_cnes <- function(base_padronizada, ...) {
    base_padronizada$tp_unid <- "01"
    list(
      base_enriquecida = base_padronizada,
      referencia_cnes = data.frame(CNES = "0000001", COMPETEN = "202501", TP_UNID = "01"),
      periodo = list(competencia = "202501"),
      resumo = list(competencia = "202501")
    )
  }
  ambiente_app$recarregar_modulos_desenvolvimento <- function(...) invisible(FALSE)

  shiny::testServer(ambiente_app$server, {
    base_teste <- data.frame(
      id_unidade = "1", nu_ano = "2025", dt_notific = as.Date("2025-01-01")
    )
    base_padronizada(base_teste)
    resultado_padronizacao(list(variaveis_originais = names(base_teste)))
    resultado_validade(list(anterior = TRUE))

    session$setInputs(enriquecer_base = 1)
    session$flushReact()

    expect_identical(base_padronizada()$tp_unid, "01")
    expect_identical(resultado_enriquecimento()$periodo$competencia, "202501")
    expect_null(resultado_validade())
    expect_null(erro_enriquecimento())
  })
})
