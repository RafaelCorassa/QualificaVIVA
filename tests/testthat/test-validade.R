# Testes unitários das regras de validade --------------------------------------
# A fábrica cria todas as variáveis requeridas e cada caso altera apenas os
# valores necessários, facilitando a leitura do critério que está sendo testado.
criar_base_validade <- function(n = 1L) {
  variaveis <- unique(unlist(lapply(regras_validade, `[[`, "variaveis"),
                            use.names = FALSE))
  dados <- as.data.frame(
    stats::setNames(rep(list(rep("2", n)), length(variaveis)), variaveis),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  dados$nm_pacient <- rep("PESSOA TESTE", n)
  dados$cs_sexo <- rep("F", n)
  dados$nu_idade_n <- rep("4020", n)
  dados$cs_raca <- rep("1", n)
  dados$orient_sex <- rep("9", n)
  dados$ident_gen <- rep("9", n)
  dados$viol_fisic <- rep("1", n)
  dados
}

executar_base_validade <- function(base, originais = names(base)) {
  executar_analise_validade(base, originais)
}

test_that("regra 1 sinaliza ausência de código 1 inclusive com campos vazios", {
  base <- criar_base_validade(3)
  variaveis <- regras_validade[["1"]]$variaveis
  for (variavel in variaveis) base[[variavel]] <- c("2", "2", "")
  base$viol_fisic[[2]] <- "1"

  resultado <- executar_base_validade(base)
  expect_equal(resultado$por_registro$regra_1, c(TRUE, FALSE, TRUE))
})

test_that("regra 3 preserva prefixos e limites definidos no Stata", {
  nomes <- c("FETO JOAO", "FETO", "ABORTO RETIDO", "NATIMORTO",
             "NASCITURO", "XABORTO")
  base <- criar_base_validade(length(nomes))
  base$nm_pacient <- nomes

  resultado <- executar_base_validade(base)
  expect_equal(resultado$por_registro$regra_3,
               c(TRUE, FALSE, TRUE, TRUE, TRUE, FALSE))
})

test_that("regra 4 respeita idade inclusiva e cada grupo de exclusão", {
  base <- criar_base_validade(9)
  base$cs_sexo <- rep("M", 9)
  base$nu_idade_n <- c("4020", "4059", "4019", "4060", "4030",
                       "4030", "4030", "4030", "4030")
  base$cs_raca[[5]] <- "5"
  base$orient_sex[[6]] <- "1"
  base$def_trans[[7]] <- "1"
  base$viol_sexu[[8]] <- "1"
  base$rel_pai[[9]] <- "1"

  resultado <- executar_base_validade(base)
  expect_equal(resultado$por_registro$regra_4,
               c(TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE))
})

test_that("um registro pode atender simultaneamente às três regras", {
  base <- criar_base_validade()
  for (variavel in regras_validade[["1"]]$variaveis) base[[variavel]] <- "2"
  base$nm_pacient <- "ABORTO RETIDO"
  base$cs_sexo <- "M"
  base$nu_idade_n <- "4035"

  resultado <- executar_base_validade(base)
  expect_true(all(unlist(resultado$por_registro[c("regra_1", "regra_3", "regra_4")])))
  expect_equal(resultado$por_registro$numero_problemas_validade, 3L)
  expect_true(resultado$por_registro$problema_validade)
  expect_equal(resultado$resumo$ocorrencias, 3L)
  expect_equal(resultado$resumo$registros_problematicos, 1L)
})

test_that("resultado sem notificações problemáticas é calculado corretamente", {
  base <- criar_base_validade(2)
  resultado <- executar_base_validade(base)

  expect_false(any(resultado$por_registro$problema_validade))
  expect_equal(resultado$por_registro$numero_problemas_validade, c(0L, 0L))
  expect_equal(resultado$resumo$registros_problematicos, 0L)
  expect_equal(resultado$resumo$percentual_problematicos, 0)
  expect_equal(resultado$resumo$ocorrencias, 0L)
})

test_that("variável originalmente ausente torna a regra não executável", {
  base <- criar_base_validade(2)
  base$viol_outr <- ""
  originais <- setdiff(names(base), "viol_outr")
  resultado <- executar_base_validade(base, originais)

  linha <- subset(resultado$por_regra, regra == 1L)
  expect_equal(linha$status, "Não executável")
  expect_match(linha$detalhe, "viol_outr")
  expect_true(all(is.na(resultado$por_registro$regra_1)))
  expect_true(any(grepl("Regra 1 não executável", resultado$avisos)))
})

test_that("regra 4 utiliza a variável corrigida tran_ment", {
  base <- criar_base_validade()
  base$cs_sexo <- "M"
  base$nu_idade_n <- "4030"
  resultado <- executar_base_validade(base, names(base))

  linha <- subset(resultado$por_regra, regra == 4L)
  expect_equal(linha$status, "Executada")
  expect_equal(linha$detalhe, "")
  expect_true(resultado$por_registro$regra_4)
})

test_that("identificadores 1, 3 e 4 e descrições são preservados", {
  resultado <- executar_base_validade(criar_base_validade())
  expect_equal(resultado$por_regra$regra, c(1L, 3L, 4L))
  expect_true(all(nzchar(resultado$por_regra$descricao)))
})

test_that("consulta por regra retorna somente sinalizados e variáveis relevantes", {
  base <- criar_base_validade(2)
  base$nm_pacient <- c("ABORTO", "PESSOA")
  resultado <- executar_base_validade(base)
  registros <- obter_registros_validade(base, resultado, "3")

  expect_equal(registros$id_registro, 1L)
  expect_true(all(c("nm_pacient", "regra_3", "numero_problemas_validade") %in%
                    names(registros)))
})
