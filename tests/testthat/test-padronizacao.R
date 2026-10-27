# Testes do pipeline de padronização -------------------------------------------
# A base sintética contém valores válidos, inválidos e uma coluna desconhecida
# para verificar transformações, auditoria, preservação e mensagens de erro.
criar_base_sintetica_padronizacao <- function() {
  data.frame(
    CS_SEXO = c("F", "X"),
    NM_PACIENT = c("  José  da-Silva ", "Ana@123"),
    DT_NOTIFIC = c("2024-01-31", "20240230"),
    LOCAL_OCOR = c("1", "77"),
    CS_ESCOL_N = c("10", "11"),
    VIOL_MOTIV = c("9", "77"),
    ID_MN_RESI = c("110001", "999999"),
    ID_MN_OCOR = c("530010", "abc"),
    EXTRA_USUARIO = c("preservar 1", "preservar 2"),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

test_that("a base bruta é preservada e variáveis adicionais permanecem", {
  base_bruta <- criar_base_sintetica_padronizacao()
  copia <- base_bruta
  resultado <- padronizar_base(base_bruta)

  expect_identical(base_bruta, copia)
  expect_true("EXTRA_USUARIO" %in% names(resultado$base_padronizada))
  expect_equal(resultado$base_padronizada$EXTRA_USUARIO, base_bruta$EXTRA_USUARIO)
})

test_that("nomes esperados são normalizados e ausências são registradas", {
  base_bruta <- criar_base_sintetica_padronizacao()
  resultado <- padronizar_base(base_bruta)
  dados <- resultado$base_padronizada

  expect_true(all(c("cs_sexo", "nm_pacient", "cs_raca") %in% names(dados)))
  expect_true("CS_SEXO" %in% names(base_bruta))
  expect_true("cs_raca" %in% resultado$variaveis_ausentes$variavel)
  expect_true(all(dados$cs_raca == ""))
  expect_equal(resultado$resumo$variaveis_presentes, 8L)
  expect_equal(resultado$resumo$variaveis_ausentes,
               resultado$resumo$variaveis_esperadas - 8L)
})

test_that("códigos categóricos válidos são mantidos e inválidos removidos", {
  resultado <- padronizar_base(criar_base_sintetica_padronizacao())
  expect_equal(resultado$base_padronizada$cs_sexo, c("F", ""))
  linha <- subset(resultado$auditoria,
                  variavel == "cs_sexo" & tipo_transformacao == "validacao_dominio")
  expect_equal(linha$valores_invalidos, 1L)
})

test_that("campos abertos são normalizados sem conteúdo na auditoria", {
  resultado <- padronizar_base(criar_base_sintetica_padronizacao())
  expect_equal(resultado$base_padronizada$nm_pacient,
               c("JOSE DASILVA", "ANA123"))
  expect_equal(names(resultado$auditoria),
               c("variavel", "tipo_transformacao", "valores_alterados",
                 "valores_invalidos"))
  expect_false(any(grepl("JOSE|ANA123", unlist(resultado$auditoria))))
})

test_that("datas YMD válidas viram Date e inválidas são contabilizadas", {
  resultado <- padronizar_base(criar_base_sintetica_padronizacao())
  datas <- resultado$base_padronizada$dt_notific
  expect_s3_class(datas, "Date")
  expect_equal(datas[[1]], as.Date("2024-01-31"))
  expect_true(is.na(datas[[2]]))
  expect_equal(resultado$resumo$datas_invalidas, 1L)
})

test_that("conversão otimizada de datas preserva valores e contagens de auditoria", {
  valores <- c(
    "20240131", "2024-01-31", " 20240131 ", "20240230", "", NA_character_
  )
  simples <- converter_data_ymd(valores)
  detalhado <- converter_data_ymd(valores, detalhes = TRUE)

  expect_identical(detalhado$valores, simples)
  expect_equal(
    simples,
    as.Date(c("2024-01-31", "2024-01-31", "2024-01-31", NA, NA, NA))
  )
  expect_equal(detalhado$alterados, 4L)
  expect_equal(detalhado$invalidos, 1L)

  datas_prontas <- as.Date(c("2024-01-31", NA))
  detalhe_pronto <- converter_data_ymd(datas_prontas, detalhes = TRUE)
  expect_identical(detalhe_pronto$valores, datas_prontas)
  expect_equal(detalhe_pronto$alterados, 0L)
  expect_equal(detalhe_pronto$invalidos, 0L)
})

test_that("LOCAL_OCOR, CS_ESCOL_N e VIOL_MOTIV são formatados e validados", {
  dados <- padronizar_base(criar_base_sintetica_padronizacao())$base_padronizada
  expect_equal(dados$local_ocor, c("01", ""))
  expect_equal(dados$cs_escol_n, c("10", ""))
  expect_equal(dados$viol_motiv, c("09", ""))
})

test_that("municípios explícitos, inválidos e faixa do DF são tratados", {
  resultado <- padronizar_base(criar_base_sintetica_padronizacao())
  dados <- resultado$base_padronizada
  expect_equal(dados$id_mn_resi, c("110001", ""))
  expect_equal(dados$id_mn_ocor, c("530010", ""))
  expect_equal(resultado$resumo$municipios_invalidos, 2L)
})

test_that("larguras Stata são reproduzidas por truncamento", {
  base <- criar_base_sintetica_padronizacao()
  base$NO_LOG_OCO <- c(strrep("A", 60), "curto")
  dados <- padronizar_base(base)$base_padronizada
  expect_equal(nchar(dados$no_log_oco), c(50L, 5L))
})

test_that("a validação inicial reúne todos os impedimentos encontrados", {
  base <- data.frame(a = 1, b = 2, check.names = FALSE)
  names(base) <- c("CS_SEXO", "cs_sexo")
  referencia_inexistente <- file.path(tempdir(), "referencia-que-nao-existe.csv")

  erro <- tryCatch(
    padronizar_base(base, caminho_municipios = referencia_inexistente),
    erro_padronizacao = identity
  )

  expect_s3_class(erro, "erro_padronizacao")
  expect_length(erro$erros, 2L)
  expect_true(any(grepl("múltiplas colunas.*cs_sexo", erro$erros,
                        ignore.case = TRUE)))
  expect_true(any(grepl("referência municipal não existe", erro$erros,
                        ignore.case = TRUE)))
  expect_equal(extrair_erros_padronizacao(erro), erro$erros)
})

test_that("falhas de transformação informam etapa e variável", {
  base <- criar_base_sintetica_padronizacao()
  classe_anterior <- getS3method("format", "campo_com_erro", optional = TRUE)
  format.campo_com_erro <- function(x, ...) stop("tipo sintético incompatível")
  assign("format.campo_com_erro", format.campo_com_erro,
         envir = .GlobalEnv)
  on.exit({
    if (is.null(classe_anterior)) {
      rm("format.campo_com_erro", envir = .GlobalEnv)
    } else {
      assign("format.campo_com_erro", classe_anterior, envir = .GlobalEnv)
    }
  }, add = TRUE)
  base$NM_PACIENT <- structure(c(1, 2), class = c("campo_com_erro", "numeric"))

  erro <- tryCatch(padronizar_base(base), erro_padronizacao = identity)
  expect_s3_class(erro, "erro_padronizacao")
  expect_match(erro$erros,
               "conversão para texto.*variável 'nm_pacient'.*tipo sintético incompatível")
})

test_that("bytes Windows-1252 inválidos em UTF-8 são corrigidos antes do truncamento", {
  byte_legado <- rawToChar(as.raw(c(0xA7, 0x20, 0x41, 0x4E)))
  Encoding(byte_legado) <- "UTF-8"
  base <- data.frame(AG_ESPEC = byte_legado, stringsAsFactors = FALSE,
                     check.names = FALSE)
  bytes_originais <- charToRaw(base$AG_ESPEC)

  resultado <- padronizar_base(base, codificacao_texto = "auto")

  expect_equal(resultado$base_padronizada$ag_espec, "AN")
  expect_identical(charToRaw(base$AG_ESPEC), bytes_originais)
  linha <- subset(
    resultado$auditoria,
    variavel == "AG_ESPEC" & tipo_transformacao == "normalizacao_codificacao_auto"
  )
  expect_equal(linha$valores_alterados, 1L)
  expect_equal(linha$valores_invalidos, 0L)
})

test_that("UTF-8 forçado relata variável que contém bytes incompatíveis", {
  byte_legado <- rawToChar(as.raw(c(0xA7, 0x20, 0x41, 0x4E)))
  Encoding(byte_legado) <- "UTF-8"
  base <- data.frame(AG_ESPEC = byte_legado, stringsAsFactors = FALSE,
                     check.names = FALSE)

  erro <- tryCatch(
    padronizar_base(base, codificacao_texto = "UTF-8"),
    erro_padronizacao = identity
  )

  expect_s3_class(erro, "erro_padronizacao")
  expect_match(
    erro$erros,
    "normalização da codificação.*variável 'AG_ESPEC'.*não puderam ser convertidos de UTF-8"
  )
})
