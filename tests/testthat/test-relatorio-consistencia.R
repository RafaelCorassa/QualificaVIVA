# Testes de agregação e geração do relatório de consistência.
# O cenário controlado reúne quatro UFs, municípios, CNES e regras para validar
# filtros territoriais, grupos, rótulos e artefatos HTML/PDF.
criar_cenario_relatorio_consistencia <- function() {
  ufs <- data.frame(
    codigo_uf = c("12", "33", "35", "50"),
    sigla_uf = c("AC", "RJ", "SP", "MS"),
    nome_uf = c("Acre", "Rio de Janeiro", "São Paulo", "Mato Grosso do Sul"),
    codigo_regiao = c("1", "3", "3", "5"),
    nome_regiao = c("Norte", "Sudeste", "Sudeste", "Centro-Oeste"),
    stringsAsFactors = FALSE
  )
  municipios <- data.frame(
    codigo_sinan_6 = c("120040", "330455", "355030", "500270"),
    codigo_ibge_7 = c("1200401", "3304557", "3550308", "5002704"),
    nome_municipio = c("Rio Branco", "Rio de Janeiro", "São Paulo", "Campo Grande"),
    codigo_uf = ufs$codigo_uf,
    sigla_uf = ufs$sigla_uf,
    codigo_regiao = ufs$codigo_regiao,
    nome_regiao = ufs$nome_regiao,
    stringsAsFactors = FALSE
  )
  base <- data.frame(
    sg_uf_not = c("35", "35", "33", "12", "50", "99"),
    id_municip = c("355030", "355030", "330455", "120040", "500270", "999999"),
    id_unidade = sprintf("%07d", 1:6),
    nu_ano = rep("2021", 6),
    stringsAsFactors = FALSE
  )
  indicadores <- data.frame(
    id_registro = 1:6,
    regra_1 = c(TRUE, FALSE, TRUE, FALSE, FALSE, FALSE),
    regra_2 = c(FALSE, TRUE, TRUE, FALSE, FALSE, FALSE),
    regra_27a = c(FALSE, FALSE, FALSE, TRUE, FALSE, FALSE),
    regra_28 = c(FALSE, FALSE, FALSE, FALSE, TRUE, FALSE),
    numero_problemas_consistencia = c(1L, 1L, 2L, 1L, 1L, 0L),
    problema_consistencia = c(TRUE, TRUE, TRUE, TRUE, TRUE, FALSE)
  )
  regras <- data.frame(
    regra = c("1", "2", "27A", "28"),
    indicador = c("regra_1", "regra_2", "regra_27a", "regra_28"),
    descricao = paste("Descrição", c("1", "2", "27A", "28")),
    grupo_id = c(1L, 1L, 6L, 7L),
    grupo = c(
      rep("Integridade cadastral, temporal e territorial", 2),
      "Perfil e vínculo do provável autor", "Violência sexual"
    ),
    status = rep("Executada", 4),
    detalhe = rep("", 4),
    registros_sinalizados = c(2L, 2L, 1L, 1L),
    percentual_total = 100 * c(2, 2, 1, 1) / 6,
    stringsAsFactors = FALSE
  )
  grupos <- data.frame(
    grupo_id = c(1L, 6L, 7L),
    grupo = c(
      "Integridade cadastral, temporal e territorial",
      "Perfil e vínculo do provável autor", "Violência sexual"
    ),
    status = rep("Executado", 3),
    registros_sinalizados = c(3L, 1L, 1L),
    percentual_total = 100 * c(3, 1, 1) / 6,
    ocorrencias = c(4L, 1L, 1L),
    stringsAsFactors = FALSE
  )
  list(
    base = base,
    resultado = list(
      por_registro = indicadores,
      por_regra = regras,
      por_grupo = grupos
    ),
    referencias = list(
      ufs = ufs, municipios = municipios, cnes = NULL,
      arquivo_cnes = NULL, geografia_ufs = NULL
    )
  )
}

criar_cenario_municipal_consistencia <- function() {
  cenario <- criar_cenario_relatorio_consistencia()
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
      sg_uf_not = c("35", "35"), id_municip = c("350950", "350950"),
      id_unidade = c("0000001", "0000007"), nu_ano = c("2021", "2021"),
      stringsAsFactors = FALSE
    )
  )
  cenario$resultado$por_registro <- rbind(
    cenario$resultado$por_registro,
    data.frame(
      id_registro = 7:8,
      regra_1 = c(FALSE, FALSE), regra_2 = c(FALSE, FALSE),
      regra_27a = c(TRUE, FALSE), regra_28 = c(FALSE, TRUE),
      numero_problemas_consistencia = c(1L, 1L),
      problema_consistencia = c(TRUE, TRUE)
    )
  )
  cenario$resultado$por_regra$registros_sinalizados <- c(2L, 2L, 2L, 2L)
  cenario$resultado$por_regra$percentual_total <-
    100 * cenario$resultado$por_regra$registros_sinalizados / nrow(cenario$base)
  cenario
}

test_that("agregação do relatório preserva grupos, regras e 27A", {
  cenario <- criar_cenario_relatorio_consistencia()
  dados <- pre_agregar_relatorio_consistencia(
    cenario$base, cenario$resultado, cenario$referencias
  )
  grupo_1 <- dados$grupos_brasil[dados$grupos_brasil$grupo_id == 1L, ]

  expect_equal(dados$brasil$notificacoes, 6L)
  expect_equal(dados$brasil$registros_invalidos, 5L)
  expect_equal(grupo_1$registros_invalidos, 3L)
  expect_equal(grupo_1$ocorrencias_invalidas, 4L)
  expect_true("27A" %in% dados$regras_brasil$regra_id)
  expect_equal(sum(dados$ufs$notificacoes), 6L)
})

test_that("agregação matricial preserva exatamente o algoritmo de referência", {
  cenario <- criar_cenario_relatorio_consistencia()
  registros <- preparar_registros_territoriais_consistencia(
    cenario$base, cenario$resultado, cenario$referencias
  )

  agregar_regras_referencia <- function(grupos) {
    executadas <- cenario$resultado$por_regra[
      cenario$resultado$por_regra$status == "Executada", , drop = FALSE
    ]
    partes <- lapply(seq_len(nrow(executadas)), function(i) {
      regra <- executadas[i, ]
      temporario <- registros
      temporario$problema_validade <- as.logical(temporario[[regra$indicador]])
      temporario$numero_problemas_validade <- as.integer(temporario[[regra$indicador]])
      tabela <- agregar_indicadores_validade(temporario, grupos)
      tabela$regra_id <- as.character(regra$regra)
      tabela$regra <- paste("Regra", regra$regra)
      tabela$descricao_regra <- regra$descricao
      tabela$grupo_id <- as.integer(regra$grupo_id)
      tabela$grupo <- regra$grupo
      tabela
    })
    saida <- do.call(rbind, partes)
    rownames(saida) <- NULL
    saida
  }

  agregar_grupos_referencia <- function(grupos) {
    partes <- lapply(seq_len(nrow(cenario$resultado$por_grupo)), function(i) {
      grupo <- cenario$resultado$por_grupo[i, ]
      regras <- cenario$resultado$por_regra[
        cenario$resultado$por_regra$grupo_id == grupo$grupo_id &
          cenario$resultado$por_regra$status == "Executada", , drop = FALSE
      ]
      matriz <- as.matrix(registros[regras$indicador])
      temporario <- registros
      temporario$problema_validade <- rowSums(matriz == TRUE, na.rm = TRUE) > 0L
      temporario$numero_problemas_validade <- rowSums(matriz == TRUE, na.rm = TRUE)
      tabela <- agregar_indicadores_validade(temporario, grupos)
      tabela$grupo_id <- as.integer(grupo$grupo_id)
      tabela$grupo <- grupo$grupo
      tabela$status_grupo <- grupo$status
      tabela
    })
    saida <- do.call(rbind, partes)
    rownames(saida) <- NULL
    saida
  }

  dimensoes <- list(
    character(), c("codigo_regiao", "nome_regiao"),
    c("codigo_uf", "codigo_sinan_6", "nome_municipio"),
    c("codigo_uf", "id_unidade", "nome_unidade")
  )
  for (grupos in dimensoes) {
    expect_equal(
      agregar_regras_consistencia(registros, cenario$resultado$por_regra, grupos),
      agregar_regras_referencia(grupos)
    )
    expect_equal(
      agregar_grupos_consistencia(
        registros, cenario$resultado$por_regra,
        cenario$resultado$por_grupo, grupos
      ),
      agregar_grupos_referencia(grupos)
    )
  }
})

test_that("consistência municipal separa CNES repetido e preserva a UF", {
  cenario <- criar_cenario_municipal_consistencia()
  municipal <- pre_agregar_relatorio_consistencia(
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
    compartilhado$notificacoes[compartilhado$codigo_sinan_6 == "355030"], 1L
  )
  expect_equal(
    compartilhado$registros_invalidos[compartilhado$codigo_sinan_6 == "350950"], 1L
  )
  expect_equal(
    compartilhado$registros_invalidos[compartilhado$codigo_sinan_6 == "355030"], 1L
  )
  expect_true(nrow(municipal$regras_cnes_municipio) > 0L)
  expect_true(nrow(municipal$grupos_cnes_municipio) > 0L)

  somente_uf <- pre_agregar_relatorio_consistencia(
    cenario$base, cenario$resultado, cenario$referencias,
    ufs_detalhadas = "35"
  )
  uf_e_municipio <- pre_agregar_relatorio_consistencia(
    cenario$base, cenario$resultado, cenario$referencias,
    ufs_detalhadas = "35", municipios_detalhados = "350950"
  )
  expect_equal(somente_uf$cnes, uf_e_municipio$cnes)
  expect_equal(somente_uf$municipios, uf_e_municipio$municipios)
})

test_that("seção municipal de consistência contém grupos, regras e estabelecimentos", {
  cenario <- criar_cenario_municipal_consistencia()
  dados <- pre_agregar_relatorio_consistencia(
    cenario$base, cenario$resultado, cenario$referencias,
    ufs_detalhadas = character(), municipios_detalhados = "350950"
  )
  config <- criar_config_relatorio(
    cenario$base, cenario$referencias, municipios = "350950"
  )
  html <- as.character(montar_documento_relatorio_consistencia(config, dados))

  expect_false(grepl('href="#brasil"', html, fixed = TRUE))
  expect_false(grepl('id="brasil"', html, fixed = TRUE))
  expect_false(grepl('id="regiao-', html, fixed = TRUE))
  expect_false(grepl('id="uf-', html, fixed = TRUE))
  expect_match(html, 'href="#municipio-350950"', fixed = TRUE)
  expect_match(html, 'id="municipio-350950"', fixed = TRUE)
  expect_match(html, "1. Campinas (SP)", fixed = TRUE)
  expect_match(
    html, titulo_de_relatorio("consistencia", "sintese_grupos"), fixed = TRUE
  )
  expect_match(html, "Regra 27A", fixed = TRUE)
  expect_match(html, "Pareto dos estabelecimentos", fixed = TRUE)
  expect_match(html, "Matriz de prioridade territorial", fixed = TRUE)
  expect_match(html, "Grupos dominantes nos principais estabelecimentos", fixed = TRUE)
  expect_match(html, "Expandir regras individuais", fixed = TRUE)
  expect_false(grepl("Municípios prioritários", html, fixed = TRUE))
  expect_match(
    html,
    paste0(">2. ", titulo_de_relatorio("consistencia", "metodologia"), "<"),
    fixed = TRUE
  )
})

test_that("consistência aceita relatório nacional e combinação das quatro abrangências", {
  cenario <- criar_cenario_municipal_consistencia()
  dados <- pre_agregar_relatorio_consistencia(
    cenario$base, cenario$resultado, cenario$referencias,
    ufs_detalhadas = "35", municipios_detalhados = "350950"
  )
  nacional <- criar_config_relatorio(
    cenario$base, cenario$referencias, incluir_brasil = TRUE
  )
  html_nacional <- as.character(montar_documento_relatorio_consistencia(
    nacional, dados
  ))
  expect_match(html_nacional, 'id="brasil"', fixed = TRUE)
  expect_false(grepl('id="regiao-', html_nacional, fixed = TRUE))
  expect_false(grepl('id="uf-', html_nacional, fixed = TRUE))
  expect_false(grepl('id="municipio-', html_nacional, fixed = TRUE))
  expect_match(html_nacional, 'class="rodape-relatorio"', fixed = TRUE)
  expect_match(html_nacional, 'class="logo-relatorio"', fixed = TRUE)
  expect_match(html_nacional, "data:image/png;base64", fixed = TRUE)
  expect_match(html_nacional, "OpenAI Codex", fixed = TRUE)

  combinada <- criar_config_relatorio(
    cenario$base, cenario$referencias, incluir_brasil = TRUE,
    regioes = "3", ufs = "35", municipios = "350950"
  )
  html_combinado <- as.character(montar_documento_relatorio_consistencia(
    combinada, dados
  ))
  expect_match(html_combinado, ">1. Brasil<", fixed = TRUE)
  expect_match(html_combinado, ">2. Região Sudeste<", fixed = TRUE)
  expect_match(html_combinado, ">3. São Paulo (SP)<", fixed = TRUE)
  expect_match(html_combinado, ">4. Campinas (SP)<", fixed = TRUE)
  expect_match(
    html_combinado,
    paste0(">5. ", titulo_de_relatorio("consistencia", "metodologia"), "<"),
    fixed = TRUE
  )
})

test_that("gerador público de consistência aceita Brasil e rejeita ausência de abrangência", {
  cenario <- criar_cenario_relatorio_consistencia()
  diretorio <- tempfile("relatorio-vazio-consistencia-")
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
  artefato <- gerar_relatorio_consistencia(
    base_padronizada = cenario$base,
    resultado_consistencia = cenario$resultado,
    incluir_brasil = TRUE,
    diretorio_referencias = dir_ref,
    diretorio_saida = file.path(diretorio, "nacional"),
    gerar_pdf = FALSE
  )
  html <- paste(readLines(artefato$caminho_html, encoding = "UTF-8"), collapse = "\n")
  expect_match(html, 'id="brasil"', fixed = TRUE)
  expect_false(grepl('id="regiao-', html, fixed = TRUE))
  expect_false(grepl('id="uf-', html, fixed = TRUE))
  expect_false(grepl('id="municipio-', html, fixed = TRUE))

  expect_error(
    gerar_relatorio_consistencia(
      base_padronizada = cenario$base,
      resultado_consistencia = cenario$resultado,
      diretorio_referencias = dir_ref,
      diretorio_saida = file.path(diretorio, "saida"),
      gerar_pdf = FALSE
    ),
    "Selecione ao menos uma abrangência"
  )
})

test_that("HTML permite contrair grupos e expandir todas as regras", {
  cenario <- criar_cenario_relatorio_consistencia()
  diretorio <- tempfile("relatorio-consistencia-html-")
  dir.create(diretorio)
  dir_ref <- file.path(diretorio, "reference")
  dir.create(dir_ref)
  utils::write.csv(
    cenario$referencias$ufs, file.path(dir_ref, "ufs.csv"), row.names = FALSE
  )
  utils::write.csv(
    cenario$referencias$municipios,
    file.path(dir_ref, "municipios.csv"), row.names = FALSE
  )
  artefato <- gerar_relatorio_consistencia(
    cenario$base, cenario$resultado,
    regioes = "3", ufs = "35",
    diretorio_referencias = dir_ref,
    diretorio_saida = file.path(diretorio, "saida"),
    gerar_pdf = FALSE,
    gerado_em = as.POSIXct("2026-09-22 12:00:00", tz = "America/Sao_Paulo"),
    escala_matriz = "transformada"
  )
  html <- paste(readLines(artefato$caminho_html, encoding = "UTF-8"), collapse = "\n")

  expect_true(file.exists(artefato$caminho_html))
  expect_false("dados_agregados" %in% names(artefato))
  expect_match(html, "QualificaVIVA — Relatório de Consistência", fixed = TRUE)
  expect_match(html, "Ver apenas grupos", fixed = TRUE)
  expect_match(html, "Expandir todas as regras", fixed = TRUE)
  expect_match(html, "detalhes-regras-consistencia", fixed = TRUE)
  expect_match(html, "Integridade cadastral, temporal e territorial", fixed = TRUE)
  expect_match(html, "Regra 27A", fixed = TRUE)
  expect_match(html, "1. Região Sudeste", fixed = TRUE)
  expect_match(html, "2. São Paulo (SP)", fixed = TRUE)
  expect_false(grepl('href="#brasil"', html, fixed = TRUE))
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
  expect_equal(artefato$config_relatorio$periodo, "2021")
  expect_equal(artefato$config_relatorio$escala_matriz_prioridade, "transformada")
  expect_match(html, '"escala_inicial":"transformada"', fixed = TRUE)
})

test_that("gráficos de consistência usam grupos e regras separadamente", {
  cenario <- criar_cenario_relatorio_consistencia()
  dados <- pre_agregar_relatorio_consistencia(
    cenario$base, cenario$resultado, cenario$referencias
  )
  grafico_grupos <- grafico_grupos_consistencia_relatorio(
    dados$grupos_brasil, "Grupos"
  )
  grafico_regras <- grafico_regras_grupo_consistencia_relatorio(
    dados$regras_brasil,
    "Integridade cadastral, temporal e territorial", "Regras"
  )

  expect_equal(nrow(ggplot2::ggplot_build(grafico_grupos)$data[[1]]), 3L)
  expect_equal(nrow(ggplot2::ggplot_build(grafico_regras)$data[[1]]), 2L)
})

test_that("expansão das regras mantém tabelas e omite gráficos por regra", {
  cenario <- criar_cenario_relatorio_consistencia()
  dados <- pre_agregar_relatorio_consistencia(
    cenario$base, cenario$resultado, cenario$referencias
  )
  bloco <- as.character(bloco_resultados_consistencia_relatorio(
    dados$grupos_brasil, dados$regras_brasil, dados, "Brasil"
  ))
  imagens <- gregexpr('class="grafico-relatorio"', bloco, fixed = TRUE)[[1]]

  expect_match(bloco, "Expandir regras individuais", fixed = TRUE)
  expect_match(bloco, "<th>Regra</th>", fixed = TRUE)
  expect_equal(sum(imagens > 0L), 1L)
  expect_false(grepl('alt="Regras do grupo', bloco, fixed = TRUE))
})

test_that("regra não executável permanece visível sem percentual artificial", {
  cenario <- criar_cenario_relatorio_consistencia()
  cenario$resultado$por_regra$status[2] <- "Não executável"
  cenario$resultado$por_regra$detalhe[2] <- "Ausente na base original"
  cenario$resultado$por_registro$regra_2 <- NA
  dados <- pre_agregar_relatorio_consistencia(
    cenario$base, cenario$resultado, cenario$referencias
  )
  bloco <- as.character(bloco_resultados_consistencia_relatorio(
    dados$grupos_brasil, dados$regras_brasil, dados, "Brasil"
  ))

  expect_match(bloco, "Não executável", fixed = TRUE)
  expect_match(bloco, "Ausente na base original", fixed = TRUE)
  expect_false(grepl("NA%", bloco, fixed = TRUE))
})
