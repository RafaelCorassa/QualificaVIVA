# Testes de agregação e geração do relatório de validade.
# A referência sintética permite conferir resultados territoriais sem depender
# de atualização externa dos cadastros oficiais.
criar_cenario_relatorio <- function() {
  ufs <- data.frame(
    codigo_uf = c("12", "33", "35", "50"), sigla_uf = c("AC", "RJ", "SP", "MS"),
    nome_uf = c("Acre", "Rio de Janeiro", "São Paulo", "Mato Grosso do Sul"),
    codigo_regiao = c("1", "3", "3", "5"),
    nome_regiao = c("Norte", "Sudeste", "Sudeste", "Centro-Oeste"),
    stringsAsFactors = FALSE
  )
  municipios <- data.frame(
    codigo_sinan_6 = c("120040", "330455", "355030", "500270"),
    codigo_ibge_7 = c("1200401", "3304557", "3550308", "5002704"),
    nome_municipio = c("Rio Branco", "Rio de Janeiro", "São Paulo", "Campo Grande"),
    codigo_uf = ufs$codigo_uf, sigla_uf = ufs$sigla_uf,
    codigo_regiao = ufs$codigo_regiao, nome_regiao = ufs$nome_regiao,
    stringsAsFactors = FALSE
  )
  base <- data.frame(
    sg_uf_not = c("35", "35", "33", "12", "50", "99"),
    id_municip = c("355030", "355030", "330455", "120040", "500270", "999999"),
    id_unidade = c("0000001", "0000001", "0000002", "0000003", "0000004", "0000005"),
    nu_ano = rep("2020", 6), stringsAsFactors = FALSE
  )
  resultado <- list(
    por_registro = data.frame(
      id_registro = 1:6,
      regra_1 = c(TRUE, FALSE, TRUE, FALSE, TRUE, TRUE),
      regra_3 = c(FALSE, FALSE, TRUE, FALSE, FALSE, TRUE),
      regra_4 = c(FALSE, FALSE, FALSE, FALSE, TRUE, FALSE),
      numero_problemas_validade = c(1L, 0L, 2L, 0L, 2L, 2L),
      problema_validade = c(TRUE, FALSE, TRUE, FALSE, TRUE, TRUE)
    ),
    por_regra = data.frame(
      regra = c("Regra 1", "Regra 3", "Regra 4"),
      descricao = c("Descrição 1", "Descrição 3", "Descrição 4"),
      status = rep("Executada", 3), detalhe = rep("", 3),
      registros_sinalizados = c(4L, 2L, 1L),
      percentual_total = c(66.67, 33.33, 16.67), stringsAsFactors = FALSE
    )
  )
  list(base = base, resultado = resultado,
       referencias = list(ufs = ufs, municipios = municipios, cnes = NULL,
                          geografia_ufs = NULL))
}

# Acrescenta um segundo município paulista e reutiliza o CNES 0000001 nos dois
# territórios, com regras diferentes, para proteger a granularidade municipal.
criar_cenario_municipal_validade <- function() {
  cenario <- criar_cenario_relatorio()
  cenario$referencias$municipios <- rbind(
    cenario$referencias$municipios,
    data.frame(
      codigo_sinan_6 = "350950", codigo_ibge_7 = "3509502",
      nome_municipio = "Campinas", codigo_uf = "35", sigla_uf = "SP",
      codigo_regiao = "3", nome_regiao = "Sudeste",
      stringsAsFactors = FALSE
    )
  )
  cenario$referencias$municipios <- cenario$referencias$municipios[order(
    as.integer(cenario$referencias$municipios$codigo_uf),
    cenario$referencias$municipios$nome_municipio,
    cenario$referencias$municipios$codigo_sinan_6
  ), , drop = FALSE]
  cenario$base <- rbind(
    cenario$base,
    data.frame(
      sg_uf_not = c("35", "35"),
      id_municip = c("350950", "350950"),
      id_unidade = c("0000001", "0000006"),
      nu_ano = c("2020", "2020"), stringsAsFactors = FALSE
    )
  )
  cenario$resultado$por_registro <- rbind(
    cenario$resultado$por_registro,
    data.frame(
      id_registro = 7:8,
      regra_1 = c(FALSE, FALSE), regra_3 = c(TRUE, TRUE),
      regra_4 = c(FALSE, TRUE),
      numero_problemas_validade = c(1L, 2L),
      problema_validade = c(TRUE, TRUE)
    )
  )
  cenario$resultado$por_regra$registros_sinalizados <- c(4L, 4L, 2L)
  cenario$resultado$por_regra$percentual_total <-
    100 * cenario$resultado$por_regra$registros_sinalizados / nrow(cenario$base)
  cenario
}

test_that("territorialização usa somente os códigos de notificação e não perde registros", {
  cenario <- criar_cenario_relatorio()
  registros <- preparar_registros_territoriais(
    cenario$base, cenario$resultado, cenario$referencias
  )
  expect_equal(nrow(registros), nrow(cenario$base))
  expect_equal(registros$nome_uf[1], "São Paulo")
  expect_equal(registros$nome_municipio[3], "Rio de Janeiro")
  expect_false(registros$uf_localizada[6])
  expect_false(registros$municipio_localizado[6])
})

test_that("agregações preservam totais, contribuições e códigos não localizados", {
  cenario <- criar_cenario_relatorio()
  dados <- pre_agregar_relatorio_validade(
    cenario$base, cenario$resultado, cenario$referencias
  )
  expect_equal(dados$brasil$notificacoes, 6L)
  expect_equal(dados$brasil$registros_invalidos, 4L)
  expect_equal(sum(dados$ufs$notificacoes), 6L)
  expect_equal(sum(dados$ufs$registros_invalidos), 4L)
  expect_equal(sum(dados$ufs$contribuicao_invalidos), 100)
  expect_true(any(dados$ufs$codigo_uf == "99" & is.na(dados$ufs$sigla_uf)))
  expect_equal(dados$relacionamentos$registros[2], 1L)
})

test_that("detalhamento territorial pode ser limitado às UFs solicitadas", {
  cenario <- criar_cenario_relatorio()
  dados <- pre_agregar_relatorio_validade(
    cenario$base, cenario$resultado, cenario$referencias,
    ufs_detalhadas = "35"
  )

  expect_equal(dados$brasil$notificacoes, nrow(cenario$base))
  expect_true(all(is.na(dados$municipios$codigo_uf) | dados$municipios$codigo_uf == "35"))
  expect_true(all(is.na(dados$cnes$codigo_uf) | dados$cnes$codigo_uf == "35"))
})

test_that("percentuais, prioridade e Pareto são calculados de forma consistente", {
  cenario <- criar_cenario_relatorio()
  dados <- pre_agregar_relatorio_validade(
    cenario$base, cenario$resultado, cenario$referencias
  )
  sp <- dados$ufs[dados$ufs$codigo_uf == "35", ]
  expect_equal(sp$percentual_invalidos, 50)
  pareto <- calcular_pareto(dados$ufs, "codigo_uf")
  expect_true(all(diff(pareto$registros_invalidos) <= 0))
  expect_equal(tail(pareto$contribuicao_acumulada, 1), 100)
  faixas <- resumir_faixas_pareto(pareto)
  expect_true(all(faixas$unidades_necessarias >= 1L))
  expect_true(all(faixas$percentual_alcancado >= c(50, 80)))
})

test_that("seleções são normalizadas na ordem oficial", {
  cenario <- criar_cenario_relatorio()
  config <- criar_config_relatorio(
    cenario$base, cenario$referencias,
    regioes = c("5", "3", "1", "3"), ufs = c("50", "12", "35", "33")
  )
  expect_equal(config$regioes_selecionadas, c("1", "3", "5"))
  expect_equal(config$ufs_selecionadas, c("12", "33", "35", "50"))
  expect_equal(config$periodo, "2020")
  expect_equal(config$escala_matriz_prioridade, "transformada")
})

test_that("configuração aceita Brasil isolado e normaliza municípios oficiais", {
  cenario <- criar_cenario_municipal_validade()
  nacional <- criar_config_relatorio(
    base_padronizada = cenario$base, referencias = cenario$referencias,
    incluir_brasil = TRUE
  )
  expect_true(nacional$incluir_brasil)
  expect_length(nacional$regioes_selecionadas, 0L)
  expect_length(nacional$ufs_selecionadas, 0L)
  expect_length(nacional$municipios_selecionados, 0L)

  municipal <- criar_config_relatorio(
    base_padronizada = cenario$base, referencias = cenario$referencias,
    municipios = c("355030", "350950.0", "355030", "999999", "inválido")
  )
  expect_equal(municipal$municipios_selecionados, c("350950", "355030"))
  expect_equal(municipal$municipios_descartados, "999999")
  expect_error(
    criar_config_relatorio(cenario$base, cenario$referencias),
    "Selecione ao menos uma abrangência"
  )
  expect_error(
    criar_config_relatorio(
      cenario$base, cenario$referencias, municipios = "999999"
    ),
    "Selecione ao menos uma abrangência"
  )
})

test_that("opções municipais incluem apenas códigos oficiais presentes na base", {
  cenario <- criar_cenario_municipal_validade()
  escolhas <- listar_municipios_disponiveis(cenario$base, cenario$referencias)
  expect_true(all(c("350950", "355030") %in% unname(escolhas)))
  expect_false("999999" %in% unname(escolhas))
  expect_match(names(escolhas)[match("350950", escolhas)], "Campinas \\(SP\\)")
})

test_that("CNES municipal não mistura municípios e não altera a agregação estadual", {
  cenario <- criar_cenario_municipal_validade()
  municipal <- pre_agregar_relatorio_validade(
    cenario$base, cenario$resultado, cenario$referencias,
    ufs_detalhadas = character(),
    municipios_detalhados = c("350950", "355030")
  )
  compartilhado <- municipal$cnes_municipio[
    municipal$cnes_municipio$id_unidade == "0000001", , drop = FALSE
  ]
  expect_equal(sort(compartilhado$codigo_sinan_6), c("350950", "355030"))
  expect_equal(
    compartilhado$notificacoes[compartilhado$codigo_sinan_6 == "350950"], 1L
  )
  expect_equal(
    compartilhado$notificacoes[compartilhado$codigo_sinan_6 == "355030"], 2L
  )
  expect_equal(
    compartilhado$registros_invalidos[compartilhado$codigo_sinan_6 == "350950"], 1L
  )
  expect_equal(
    compartilhado$registros_invalidos[compartilhado$codigo_sinan_6 == "355030"], 1L
  )

  somente_uf <- pre_agregar_relatorio_validade(
    cenario$base, cenario$resultado, cenario$referencias,
    ufs_detalhadas = "35"
  )
  uf_e_municipio <- pre_agregar_relatorio_validade(
    cenario$base, cenario$resultado, cenario$referencias,
    ufs_detalhadas = "35", municipios_detalhados = "350950"
  )
  expect_equal(somente_uf$cnes, uf_e_municipio$cnes)
  expect_equal(somente_uf$municipios, uf_e_municipio$municipios)
})

test_that("documentos de validade respeitam abrangências e numeração dinâmica", {
  cenario <- criar_cenario_municipal_validade()
  dados <- pre_agregar_relatorio_validade(
    cenario$base, cenario$resultado, cenario$referencias,
    ufs_detalhadas = "35", municipios_detalhados = "350950"
  )

  config_nacional <- criar_config_relatorio(
    cenario$base, cenario$referencias, incluir_brasil = TRUE
  )
  html_nacional <- as.character(montar_documento_relatorio_validade(
    config_nacional, dados
  ))
  expect_match(html_nacional, 'id="brasil"', fixed = TRUE)
  expect_false(grepl('id="regiao-', html_nacional, fixed = TRUE))
  expect_false(grepl('id="uf-', html_nacional, fixed = TRUE))
  expect_false(grepl('id="municipio-', html_nacional, fixed = TRUE))
  expect_match(html_nacional, ">1. Brasil<", fixed = TRUE)
  expect_match(html_nacional, ">2. Metodologia e auditoria<", fixed = TRUE)
  expect_match(html_nacional, 'class="rodape-relatorio"', fixed = TRUE)
  expect_match(html_nacional, 'class="logo-relatorio"', fixed = TRUE)
  expect_match(html_nacional, "data:image/png;base64", fixed = TRUE)
  expect_match(html_nacional, "OpenAI Codex", fixed = TRUE)

  config_estadual <- criar_config_relatorio(
    cenario$base, cenario$referencias, ufs = "35"
  )
  html_estadual <- as.character(montar_documento_relatorio_validade(
    config_estadual, dados
  ))
  expect_false(grepl('id="brasil"', html_estadual, fixed = TRUE))
  expect_false(grepl('id="regiao-', html_estadual, fixed = TRUE))
  expect_match(html_estadual, 'id="uf-sp"', fixed = TRUE)
  expect_match(html_estadual, ">1. São Paulo (SP)<", fixed = TRUE)

  config_municipal <- criar_config_relatorio(
    cenario$base, cenario$referencias, municipios = "350950"
  )
  html_municipal <- as.character(montar_documento_relatorio_validade(
    config_municipal, dados
  ))
  expect_false(grepl('href="#brasil"', html_municipal, fixed = TRUE))
  expect_false(grepl('id="brasil"', html_municipal, fixed = TRUE))
  expect_false(grepl('id="regiao-', html_municipal, fixed = TRUE))
  expect_false(grepl('id="uf-', html_municipal, fixed = TRUE))
  expect_match(html_municipal, 'href="#municipio-350950"', fixed = TRUE)
  expect_match(html_municipal, 'id="municipio-350950"', fixed = TRUE)
  expect_match(html_municipal, "1. Campinas (SP)", fixed = TRUE)
  expect_match(html_municipal, "2. Metodologia e auditoria", fixed = TRUE)
  expect_match(html_municipal, "Pareto dos estabelecimentos", fixed = TRUE)
  expect_match(html_municipal, "Matriz de prioridade territorial", fixed = TRUE)
  expect_match(html_municipal, "Regras dominantes nos principais estabelecimentos", fixed = TRUE)
  expect_false(grepl("Municípios prioritários", html_municipal, fixed = TRUE))

  config_combinada <- criar_config_relatorio(
    cenario$base, cenario$referencias, incluir_brasil = TRUE,
    regioes = "3", ufs = "35", municipios = "350950"
  )
  html_combinado <- as.character(montar_documento_relatorio_validade(
    config_combinada, dados
  ))
  expect_match(html_combinado, ">1. Brasil<", fixed = TRUE)
  expect_match(html_combinado, ">2. Região Sudeste<", fixed = TRUE)
  expect_match(html_combinado, ">3. São Paulo (SP)<", fixed = TRUE)
  expect_match(html_combinado, ">4. Campinas (SP)<", fixed = TRUE)
  expect_match(html_combinado, ">5. Metodologia e auditoria<", fixed = TRUE)
})

test_that("gerador público de validade aceita Brasil isolado e rejeita seleção vazia", {
  cenario <- criar_cenario_relatorio()
  diretorio <- tempfile("relatorio-nacional-validade-")
  dir.create(diretorio)
  dir_ref <- file.path(diretorio, "reference")
  dir.create(dir_ref)
  utils::write.csv(
    cenario$referencias$ufs, file.path(dir_ref, "ufs.csv"), row.names = FALSE
  )
  utils::write.csv(
    cenario$referencias$municipios, file.path(dir_ref, "municipios.csv"),
    row.names = FALSE
  )
  artefato <- gerar_relatorio_validade(
    base_padronizada = cenario$base,
    resultado_validade = cenario$resultado,
    incluir_brasil = TRUE,
    diretorio_referencias = dir_ref,
    diretorio_saida = file.path(diretorio, "saida"),
    gerar_pdf = FALSE
  )
  html <- paste(readLines(artefato$caminho_html, encoding = "UTF-8"), collapse = "\n")
  expect_match(html, 'id="brasil"', fixed = TRUE)
  expect_false(grepl('id="regiao-', html, fixed = TRUE))
  expect_false(grepl('id="uf-', html, fixed = TRUE))
  expect_false(grepl('id="municipio-', html, fixed = TRUE))

  expect_error(
    gerar_relatorio_validade(
      base_padronizada = cenario$base,
      resultado_validade = cenario$resultado,
      diretorio_referencias = dir_ref,
      diretorio_saida = file.path(diretorio, "vazio"),
      gerar_pdf = FALSE
    ),
    "Selecione ao menos uma abrangência"
  )
})

test_that("regras dominantes são derivadas dos indicadores existentes", {
  cenario <- criar_cenario_relatorio()
  dados <- pre_agregar_relatorio_validade(
    cenario$base, cenario$resultado, cenario$referencias
  )
  dominantes <- identificar_regras_dominantes(
    dados$regras_municipio, "codigo_uf", "codigo_sinan_6"
  )
  linha <- dominantes[dominantes$codigo_sinan_6 == "330455", ]
  expect_equal(linha$regra_dominante, "Regra 1, Regra 3")
  expect_equal(linha$ocorrencias_regra_dominante, 1L)
})

test_that("HTML usa a mesma configuração agregada e respeita seções selecionadas", {
  cenario <- criar_cenario_relatorio()
  diretorio <- tempfile("relatorio-html-")
  dir.create(diretorio)
  dir_ref <- file.path(diretorio, "reference")
  dir.create(dir_ref)
  utils::write.csv(cenario$referencias$ufs, file.path(dir_ref, "ufs.csv"), row.names = FALSE)
  utils::write.csv(cenario$referencias$municipios, file.path(dir_ref, "municipios.csv"), row.names = FALSE)
  artefato <- gerar_relatorio_validade(
    cenario$base, cenario$resultado, regioes = "3", ufs = "35",
    diretorio_referencias = dir_ref, diretorio_saida = file.path(diretorio, "saida"),
    gerar_pdf = FALSE, gerado_em = as.POSIXct("2026-08-30 12:00:00", tz = "America/Sao_Paulo"),
    escala_matriz = "original"
  )
  html <- paste(readLines(artefato$caminho_html, encoding = "UTF-8"), collapse = "\n")
  expect_true(file.exists(artefato$caminho_html))
  expect_false("dados_agregados" %in% names(artefato))
  expect_match(html, 'id="config-relatorio"', fixed = TRUE)
  expect_match(html, "1. Região Sudeste", fixed = TRUE)
  expect_match(html, "2. São Paulo (SP)", fixed = TRUE)
  expect_false(grepl('href="#brasil"', html, fixed = TRUE))
  expect_match(html, "Rio de Janeiro (RJ)", fixed = TRUE)
  expect_match(html, "svg-matriz-prioridade", fixed = TRUE)
  expect_match(html, "Mostrar somente prioridades", fixed = TRUE)
  expect_match(html, "matriz-estatica-impressao", fixed = TRUE)
  expect_match(html, "Matriz de prioridade territorial", fixed = TRUE)
  expect_match(html, "svg-pareto-interativo", fixed = TRUE)
  expect_match(html, "tooltip-pareto-interativo", fixed = TRUE)
  expect_match(html, "pareto-estatico-impressao", fixed = TRUE)
  expect_false(grepl("<th>Acumulada</th>", html, fixed = TRUE))
  expect_false(grepl("Municípios necessários", html, fixed = TRUE))
  expect_false(grepl("Estabelecimentos necessários", html, fixed = TRUE))
  expect_false(grepl("Ranking de municípios", html, fixed = TRUE))
  expect_false(grepl("Ranking de estabelecimentos", html, fixed = TRUE))
  expect_lt(
    regexpr("svg-pareto-interativo", html, fixed = TRUE)[[1]],
    regexpr("<th>Município</th>", html, fixed = TRUE)[[1]]
  )
  expect_false(grepl("cdn.plot.ly", html, fixed = TRUE))
  expect_equal(artefato$config_relatorio$regioes_selecionadas, "3")
  expect_equal(artefato$config_relatorio$ufs_selecionadas, "35")
  expect_equal(artefato$config_relatorio$escala_matriz_prioridade, "original")
  expect_match(html, '"escala_inicial":"original"', fixed = TRUE)
})

test_that("regra não executável aparece no relatório sem percentuais artificiais", {
  cenario <- criar_cenario_relatorio()
  cenario$resultado$por_regra$status[2] <- "Não executável"
  cenario$resultado$por_regra$detalhe[2] <- "Ausente na base original"
  cenario$resultado$por_registro$regra_3 <- NA
  dados <- pre_agregar_relatorio_validade(
    cenario$base, cenario$resultado, cenario$referencias
  )
  bloco <- as.character(bloco_regras_relatorio(
    dados$regras_brasil, dados$regras_metodologicas
  ))
  expect_match(bloco, "Não executável", fixed = TRUE)
  expect_false(grepl("NA%", bloco, fixed = TRUE))
})

test_that("referência CNES reconhece as colunas cnes e fantasia", {
  diretorio <- tempfile("referencia-cnes-")
  dir.create(diretorio)
  utils::write.csv(
    data.frame(cnes = c("123", "7654321"),
               fantasia = c("Hospital Municipal", "Unidade Central")),
    file.path(diretorio, "cnes.csv"), row.names = FALSE
  )
  referencia <- ler_referencia_cnes(diretorio)
  expect_equal(referencia$dados$id_unidade, c("0000123", "7654321"))
  expect_equal(referencia$dados$nome_unidade,
               c("Hospital Municipal", "Unidade Central"))
})

test_that("modelo Excel CNES é reconhecido pelo aplicativo", {
  diretorio <- testthat::test_path("..", "..", "data", "reference")
  referencia <- ler_referencia_cnes(diretorio)
  expect_match(basename(referencia$arquivo), "cnes.xlsx", fixed = TRUE)
  expect_named(referencia$dados, c("id_unidade", "nome_unidade"))
  expect_true(nrow(referencia$dados) >= 0L)
})

test_that("nome fantasia vazio não invalida um código CNES válido", {
  diretorio <- tempfile("referencia-cnes-sem-fantasia-")
  dir.create(diretorio)
  utils::write.csv(
    data.frame(cnes = c("0259187", "0392812"), fantasia = c("", "Unidade B")),
    file.path(diretorio, "cnes.csv"), row.names = FALSE, na = ""
  )
  referencia <- ler_referencia_cnes(diretorio)
  expect_equal(referencia$dados$id_unidade, c("0259187", "0392812"))
  expect_true(is.na(referencia$dados$nome_unidade[1]))
  expect_equal(referencia$dados$nome_unidade[2], "Unidade B")

  dados <- list(
    ufs = data.frame(sigla_uf = character(), nome_uf = character(),
                     codigo_uf = character()),
    municipios = data.frame(nome_municipio = character(),
                             codigo_sinan_6 = character()),
    cnes = data.frame(id_unidade = "0259187", nome_unidade = NA_character_),
    possui_referencia_cnes = TRUE
  )
  rotulos <- preparar_rotulos_relatorio(dados)
  expect_equal(rotulos$cnes$nome_exibicao,
               "CNES 0259187 — nome fantasia não disponível")
})

test_that("nomes fantasia substituem os códigos CNES no relatório", {
  cenario <- criar_cenario_relatorio()
  cenario$referencias$cnes <- data.frame(
    id_unidade = sprintf("%07d", 1:5),
    nome_unidade = paste("Unidade de Saúde", LETTERS[1:5]),
    stringsAsFactors = FALSE
  )
  registros <- preparar_registros_territoriais(
    cenario$base, cenario$resultado, cenario$referencias
  )
  expect_equal(registros$nome_unidade[1], "Unidade de Saúde A")
  dados <- preparar_rotulos_relatorio(pre_agregar_relatorio_validade(
    cenario$base, cenario$resultado, cenario$referencias
  ))
  expect_true(all(grepl("^Unidade de Saúde", dados$cnes$nome_exibicao)))
  expect_false(any(grepl("^CNES ", dados$cnes$nome_exibicao)))
})

test_that("referência CNES rejeita códigos duplicados", {
  diretorio <- tempfile("referencia-cnes-duplicada-")
  dir.create(diretorio)
  utils::write.csv(
    data.frame(cnes = c("123", "123"), fantasia = c("Unidade A", "Unidade B")),
    file.path(diretorio, "cnes.csv"), row.names = FALSE
  )
  expect_error(ler_referencia_cnes(diretorio), "CNES duplicados")
})

test_that("gráfico de Pareto usa rótulos verticais sem colapsar nomes repetidos", {
  tabela <- data.frame(
    nome_exibicao = c("Unidade com nome extenso", "Unidade com nome extenso", "Outra unidade"),
    notificacoes = c(100L, 80L, 50L),
    registros_invalidos = c(20L, 10L, 5L),
    stringsAsFactors = FALSE
  )
  grafico <- grafico_pareto_validade(tabela, "nome_exibicao", "Pareto")
  expect_equal(grafico$theme$axis.text.x$angle, 90)
  expect_equal(nrow(ggplot2::ggplot_build(grafico)$data[[1]]), 3L)
})
