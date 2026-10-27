# Testes da configuração central ----------------------------------------------
#
# Protegem a área editável contra nomes ausentes e valores inválidos que
# impediriam a inicialização ou a geração dos relatórios.

testthat::test_that("configuração possui as áreas necessárias", {
  testthat::expect_true(all(
    c("aplicativo", "relatorios") %in% names(configuracao_viva_quali)
  ))
  testthat::expect_true(all(
    c("geral", "validade", "consistencia") %in%
      names(configuracao_viva_quali$relatorios)
  ))
  testthat::expect_true(all(
    c("percentil_absoluto", "percentil_relativo",
      "minimo_problemas_prioridade_3", "categorias") %in%
      names(configuracao_priorizacao())
  ))
  testthat::expect_identical(
    configuracao_viva_quali$aplicativo$nome, "QualificaVIVA"
  )
  testthat::expect_true(all(
    c("logo", "simbolo", "logo_arquivo", "simbolo_arquivo") %in%
      names(configuracao_viva_quali$aplicativo$identidade)
  ))
  testthat::expect_true(file.exists(testthat::test_path(
    "..", "..", "www", "identidade", "qualifica-viva-logo.png"
  )))
  testthat::expect_true(file.exists(testthat::test_path(
    "..", "..", "www", "identidade", "qualifica-viva-simbolo.png"
  )))
})

testthat::test_that("configuração centraliza autoria e identifica campos pendentes", {
  autoria <- configuracao_autoria()
  testthat::expect_true(all(
    c("autores", "instituicao", "contato", "ano", "versao",
      "ferramenta_apoio", "declaracao_apoio",
      "declaracao_responsabilidade") %in% names(autoria)
  ))
  testthat::expect_match(valor_autoria("autores"), "preencher", fixed = TRUE)
  testthat::expect_identical(autoria$ferramenta_apoio, "OpenAI Codex")
  testthat::expect_error(valor_autoria("campo_inexistente"), "desconhecido")
})

testthat::test_that("títulos e dimensões são obtidos pela configuração", {
  testthat::expect_identical(
    titulo_de_relatorio("consistencia", "mapas"),
    configuracao_de_relatorio("consistencia")$titulos$mapas
  )
  testthat::expect_identical(
    titulo_de_relatorio("validade", "brasil"), "Brasil"
  )
  testthat::expect_identical(titulo_numerado_relatorio(3, "Brasil"), "3. Brasil")
  testthat::expect_gt(dimensoes_figura_relatorio("mapas")$largura, 0)
  testthat::expect_gt(dimensoes_figura_relatorio("mapas")$altura, 0)
})

testthat::test_that("nomes de configuração incorretos produzem erros claros", {
  testthat::expect_error(
    configuracao_de_relatorio("desconhecido"),
    "Módulo de relatório desconhecido"
  )
  testthat::expect_error(
    titulo_de_relatorio("validade", "titulo_inexistente"),
    "não foi definido"
  )
  testthat::expect_error(cor_viva_quali("cor_inexistente"), "desconhecida")
})

testthat::test_that("disposição dos mapas aceita somente opções documentadas", {
  testthat::expect_identical(
    classe_layout_mapas("vertical"), "mapas-verticais"
  )
  testthat::expect_identical(
    classe_layout_mapas("lado_a_lado"), "mapas-lado-a-lado"
  )
  testthat::expect_error(
    classe_layout_mapas("diagonal"), "lado_a_lado.*vertical"
  )
})
