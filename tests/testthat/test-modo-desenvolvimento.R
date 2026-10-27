# Testes da recarga seletiva em desenvolvimento ------------------------------
#
# Estes testes garantem que o interruptor possa ser alterado com o aplicativo
# em execução e que apenas os módulos declarados para cada etapa sejam lidos.

testthat::test_that("configuração possui interruptor de desenvolvimento válido", {
  opcao <- configuracao_viva_quali$desenvolvimento$ativo
  testthat::expect_type(opcao, "logical")
  testthat::expect_length(opcao, 1L)
  testthat::expect_false(is.na(opcao))
})

testthat::test_that("cada etapa possui uma relação explícita de módulos seguros", {
  etapas <- c(
    "padronizacao", "enriquecimento", "validade", "consistencia",
    "relatorio_validade", "relatorio_consistencia"
  )
  for (etapa in etapas) {
    arquivos <- arquivos_recarregamento_desenvolvimento(etapa)
    testthat::expect_true(length(arquivos) > 0L)
    testthat::expect_true(all(file.exists(testthat::test_path("..", "..", arquivos))))
  }
  testthat::expect_error(
    arquivos_recarregamento_desenvolvimento("etapa_inexistente"),
    "Etapa de desenvolvimento desconhecida"
  )
})

testthat::test_that("interruptor salvo em disco vale no próximo comando", {
  projeto_teste <- tempfile("viva-modo-desenvolvimento-")
  diretorio_r_teste <- file.path(projeto_teste, "R")
  dir.create(diretorio_r_teste, recursive = TRUE)
  on.exit(unlink(projeto_teste, recursive = TRUE), add = TRUE)

  caminho_configuracao <- file.path(diretorio_r_teste, "configuracao_aplicativo.R")
  writeLines(c(
    "configuracao_viva_quali <- list(",
    "  desenvolvimento = list(ativo = TRUE, mostrar_mensagens = FALSE)",
    ")"
  ), caminho_configuracao, useBytes = TRUE)
  writeLines(
    "marcador_regras <- 'versao_inicial'",
    file.path(diretorio_r_teste, "regras_validade.R"),
    useBytes = TRUE
  )
  writeLines(
    "marcador_analise <- 'versao_inicial'",
    file.path(diretorio_r_teste, "analisar_validade.R"),
    useBytes = TRUE
  )

  ambiente_teste <- new.env(parent = baseenv())
  testthat::expect_true(recarregar_modulos_desenvolvimento(
    "validade", ambiente_teste, projeto_teste
  ))
  testthat::expect_identical(ambiente_teste$marcador_regras, "versao_inicial")
  testthat::expect_identical(ambiente_teste$marcador_analise, "versao_inicial")

  # Simula uma edição posterior e a volta ao modo normal sem reiniciar o app.
  writeLines(
    "marcador_analise <- 'versao_alterada'",
    file.path(diretorio_r_teste, "analisar_validade.R"),
    useBytes = TRUE
  )
  writeLines(c(
    "configuracao_viva_quali <- list(",
    "  desenvolvimento = list(ativo = FALSE, mostrar_mensagens = FALSE)",
    ")"
  ), caminho_configuracao, useBytes = TRUE)

  testthat::expect_false(recarregar_modulos_desenvolvimento(
    "validade", ambiente_teste, projeto_teste
  ))
  testthat::expect_identical(ambiente_teste$marcador_analise, "versao_inicial")
})
