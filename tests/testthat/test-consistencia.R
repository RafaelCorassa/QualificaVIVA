# Testes unitários das regras de consistência ----------------------------------
# As funções auxiliares criam bases mínimas com todas as dependências, permitindo
# modificar somente os campos relevantes para cada regra.
criar_base_consistencia <- function(n = 1L) {
  variaveis <- unique(unlist(lapply(regras_consistencia, `[[`, "variaveis"),
                            use.names = FALSE))
  as.data.frame(
    stats::setNames(rep(list(rep("", n)), length(variaveis)), variaveis),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

registro_consistencia <- function(valores = list()) {
  base <- criar_base_consistencia()
  for (nome in names(valores)) base[[nome]] <- valores[[nome]]
  base
}

executar_registro_consistencia <- function(valores = list(), originais = NULL) {
  base <- registro_consistencia(valores)
  if (is.null(originais)) originais <- names(base)
  executar_analise_consistencia(base, originais)
}

casos_regras_consistencia <- list(
  `1` = list(sim = list(dt_ocor = "2024-01-01", dt_nasc = "2024-01-01"),
             nao = list(dt_ocor = "2024-01-02", dt_nasc = "2024-01-01")),
  `2` = list(sim = list(dt_notific = "2024-01-01", dt_nasc = "2024-01-01"),
             nao = list(dt_notific = "2024-01-02", dt_nasc = "2024-01-01")),
  `3` = list(sim = list(cs_sexo = "F", nu_idade_n = "4010", cs_gestant = "6"),
             nao = list(cs_sexo = "F", nu_idade_n = "4009", cs_gestant = "6")),
  `4` = list(sim = list(cs_sexo = "M", cs_gestant = "5"),
             nao = list(cs_sexo = "M", cs_gestant = "6")),
  `5` = list(sim = list(cs_escol_n = "1"), nao = list(cs_escol_n = "01")),
  `6` = list(sim = list(nu_idade_n = "4006", cs_escol_n = "02"),
             nao = list(nu_idade_n = "4007", cs_escol_n = "02")),
  `7` = list(sim = list(nu_idade_n = "4009", sit_conjug = "1"),
             nao = list(nu_idade_n = "4010", sit_conjug = "1")),
  `8` = list(sim = list(cs_sexo = "F", nu_idade_n = "4009", cs_gestant = "1"),
             nao = list(cs_sexo = "F", nu_idade_n = "4010", cs_gestant = "1")),
  `9` = list(sim = list(nu_idade_n = "4013", id_ocupa_n = "123456"),
             nao = list(nu_idade_n = "4014", id_ocupa_n = "123456")),
  `10` = list(sim = list(nu_idade_n = "4009", orient_sex = "1"),
              nao = list(nu_idade_n = "4010", orient_sex = "1")),
  `11` = list(sim = list(def_espec = "OUTRA", def_out = "2"),
              nao = list(def_espec = "OUTRA", def_out = "1")),
  `12` = list(sim = list(def_trans = "2", def_fisica = "1"),
              nao = list(def_trans = "1", def_fisica = "1")),
  `13` = list(sim = list(def_trans = "1"),
              nao = list(def_trans = "1", def_fisica = "1")),
  `15` = list(sim = list(les_autop = "1", rel_propri = "2"),
              nao = list(les_autop = "1", rel_propri = "1")),
  `16` = list(sim = list(les_autop = "", rel_propri = "1"),
              nao = list(les_autop = "1", rel_propri = "1")),
  `17` = list(sim = list(les_autop = "1", nu_idade_n = "4000"),
              nao = list(les_autop = "1", nu_idade_n = "4001")),
  `18` = list(sim = list(les_autop = "1", viol_psico = "1"),
              nao = list(les_autop = "1", viol_psico = "2")),
  `19` = list(sim = list(les_autop = "1", ag_ameaca = "1"),
              nao = list(les_autop = "1", ag_ameaca = "2")),
  `20` = list(sim = list(les_autop = "1", num_envolv = "2"),
              nao = list(les_autop = "1", num_envolv = "1")),
  `21` = list(sim = list(les_autop = "1", rel_propri = "1", cs_sexo = "M", autor_sexo = "2"),
              nao = list(les_autop = "1", rel_propri = "1", cs_sexo = "M", autor_sexo = "1")),
  `22` = list(sim = list(les_autop = "1", rel_propri = "1", nu_idade_n = "4010", cicl_vid = "1"),
              nao = list(les_autop = "1", rel_propri = "1", nu_idade_n = "4010", cicl_vid = "2")),
  `23` = list(sim = list(viol_legal = "1", rel_pol = ""),
              nao = list(viol_legal = "1", rel_pol = "1")),
  `24` = list(sim = list(viol_infan = "1", nu_idade_n = "4018"),
              nao = list(viol_infan = "1", nu_idade_n = "4017")),
  `25` = list(sim = list(rel_mae = "1", autor_sexo = "1"),
              nao = list(rel_mae = "1", autor_sexo = "2")),
  `26` = list(sim = list(rel_pai = "1", autor_sexo = "2"),
              nao = list(rel_pai = "1", autor_sexo = "1")),
  `27A` = list(sim = list(nu_idade_n = "4015", rel_conj = "1"),
               nao = list(nu_idade_n = "4016", rel_conj = "1")),
  `27B` = list(sim = list(nu_idade_n = "4013", rel_patrao = "1", viol_infan = "2"),
               nao = list(nu_idade_n = "4014", rel_patrao = "1", viol_infan = "2")),
  `28` = list(sim = list(viol_sexu = "1"),
              nao = list(viol_sexu = "1", sex_estupr = "1")),
  `29` = list(sim = list(sex_porno = "1", nu_idade_n = "4020"),
              nao = list(sex_porno = "1", nu_idade_n = "4019")),
  `30` = list(sim = list(viol_sexu = "", sex_assedi = "1"),
              nao = list(viol_sexu = "1", sex_assedi = "1")),
  `31` = list(sim = list(cs_sexo = "M", atend_mulh = "1"),
              nao = list(cs_sexo = "F", atend_mulh = "1")),
  `32` = list(sim = list(nu_idade_n = "4020", infan_juv = "1"),
              nao = list(nu_idade_n = "4019", infan_juv = "1")),
  `33` = list(sim = list(rel_cat = "1", rel_trab = ""),
              nao = list(rel_cat = "1", rel_trab = "1")),
  `34` = list(sim = list(sg_uf_ocor = "35", id_mn_ocor = ""),
              nao = list(sg_uf_ocor = "35", id_mn_ocor = "355030")),
  `35` = list(sim = list(sg_uf = "35", id_mn_resi = ""),
              nao = list(sg_uf = "35", id_mn_resi = "355030"))
)

test_that("cada regra identifica o caso positivo e rejeita o caso semelhante", {
  expect_setequal(names(casos_regras_consistencia), names(regras_consistencia))
  for (id in names(casos_regras_consistencia)) {
    caso <- casos_regras_consistencia[[id]]
    indicador <- regras_consistencia[[id]]$indicador
    positivo <- executar_registro_consistencia(caso$sim)$por_registro[[indicador]]
    negativo <- executar_registro_consistencia(caso$nao)$por_registro[[indicador]]
    expect_true(positivo, info = paste("caso positivo da regra", id))
    expect_false(negativo, info = paste("caso negativo da regra", id))
  }
})

test_that("limites superiores adicionais das regras 3 e 8 são preservados", {
  regra_3_59 <- executar_registro_consistencia(list(
    cs_sexo = "F", nu_idade_n = "4059", cs_gestant = "6"
  ))$por_registro$regra_3
  regra_3_60 <- executar_registro_consistencia(list(
    cs_sexo = "F", nu_idade_n = "4060", cs_gestant = "6"
  ))$por_registro$regra_3
  regra_8_69 <- executar_registro_consistencia(list(
    cs_sexo = "F", nu_idade_n = "4069", cs_gestant = "1"
  ))$por_registro$regra_8
  regra_8_70 <- executar_registro_consistencia(list(
    cs_sexo = "F", nu_idade_n = "4070", cs_gestant = "1"
  ))$por_registro$regra_8

  expect_true(regra_3_59)
  expect_false(regra_3_60)
  expect_false(regra_8_69)
  expect_true(regra_8_70)
})

test_that("um registro pode atender a várias regras e os totais são calculados", {
  resultado <- executar_registro_consistencia(list(
    les_autop = "1", rel_propri = "2", viol_psico = "1", ag_ameaca = "1"
  ))
  expect_true(resultado$por_registro$regra_15)
  expect_true(resultado$por_registro$regra_18)
  expect_true(resultado$por_registro$regra_19)
  expect_equal(resultado$por_registro$numero_problemas_consistencia, 3L)
  expect_true(resultado$por_registro$problema_consistencia)
  expect_equal(resultado$resumo$registros_problematicos, 1L)
  expect_equal(resultado$resumo$ocorrencias, 3L)
})

test_that("resultado sem problemas é calculado corretamente", {
  base <- criar_base_consistencia(2)
  resultado <- executar_analise_consistencia(base, names(base))

  expect_false(any(resultado$por_registro$problema_consistencia))
  expect_equal(resultado$por_registro$numero_problemas_consistencia, c(0L, 0L))
  expect_equal(resultado$resumo$registros_problematicos, 0L)
  expect_equal(resultado$resumo$percentual_problematicos, 0)
  expect_equal(resultado$resumo$ocorrencias, 0L)
})

test_that("variável originalmente ausente torna somente as regras dependentes não executáveis", {
  base <- criar_base_consistencia(2)
  originais <- setdiff(names(base), "dt_ocor")
  resultado <- executar_analise_consistencia(base, originais)

  regra_1 <- resultado$por_regra[resultado$por_regra$regra == "1", ]
  regra_2 <- resultado$por_regra[resultado$por_regra$regra == "2", ]
  expect_equal(regra_1$status, "Não executável")
  expect_match(regra_1$detalhe, "dt_ocor")
  expect_true(all(is.na(resultado$por_registro$regra_1)))
  expect_equal(regra_2$status, "Executada")
})

test_that("regras 16 e 28 preservam o tratamento intencional de campos vazios", {
  regra_16 <- executar_registro_consistencia(list(
    les_autop = "", rel_propri = "1"
  ))$por_registro$regra_16
  regra_28 <- executar_registro_consistencia(list(
    viol_sexu = "1", sex_assedi = "", sex_estupr = "2", sex_pudor = "8",
    sex_porno = "9", sex_explo = "", sex_outro = "2"
  ))$por_registro$regra_28

  expect_true(regra_16)
  expect_true(regra_28)
})

test_that("grupos contam registros únicos e preservam ocorrências", {
  base <- criar_base_consistencia(2)
  base$les_autop <- c("1", "1")
  base$rel_propri <- c("2", "2")
  base$viol_psico <- c("1", "")
  resultado <- executar_analise_consistencia(base, names(base))
  grupo <- resultado$por_grupo[
    resultado$por_grupo$grupo == "Lesão autoprovocada", , drop = FALSE
  ]

  expect_equal(grupo$registros_sinalizados, 2L)
  expect_equal(grupo$ocorrencias, 3L)
})

test_that("consulta por regra mostra somente sinalizados e variáveis relevantes", {
  base <- criar_base_consistencia(2)
  base$cs_sexo <- c("M", "F")
  base$cs_gestant <- c("5", "5")
  resultado <- executar_analise_consistencia(base, names(base))
  registros <- obter_registros_consistencia(base, resultado, "4")

  expect_equal(registros$id_registro, 1L)
  expect_true(all(c("cs_sexo", "cs_gestant", "regra_4",
                    "numero_problemas_consistencia") %in% names(registros)))
})
