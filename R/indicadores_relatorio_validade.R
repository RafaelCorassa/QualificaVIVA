# Indicadores do relatório de validade ----------------------------------------
#
# Funções puras de agregação: recebem registros já relacionados às referências
# e devolvem tabelas prontas para visualização. Nenhuma delas escreve arquivos.

# Calcula percentuais sem produzir divisão por zero.
percentual_seguro <- function(numerador, denominador) {
  tamanho <- max(length(numerador), length(denominador))
  numerador <- rep_len(numerador, tamanho)
  denominador <- rep_len(denominador, tamanho)
  resultado <- 100 * numerador / denominador
  resultado[is.na(denominador) | denominador == 0] <- 0
  resultado
}

# Prepara uma vez os grupos territoriais sem criar listas com um índice para
# cada registro. O vetor inteiro resultante pode ser reutilizado por totais,
# regras e grupos e é descartado assim que o nível territorial termina.
criar_plano_agregacao_territorial <- function(registros, grupos = character()) {
  numero_registros <- nrow(registros)
  if (!length(grupos)) {
    return(list(
      grupos = grupos,
      indice = rep.int(1L, numero_registros),
      valores = data.frame(matrix(nrow = 1L, ncol = 0L)),
      notificacoes = as.integer(numero_registros)
    ))
  }
  if (!numero_registros) {
    return(list(
      grupos = grupos,
      indice = integer(),
      valores = registros[FALSE, grupos, drop = FALSE],
      notificacoes = integer()
    ))
  }

  chaves <- lapply(registros[grupos], function(x) {
    valor <- as.character(x)
    ausente <- is.na(valor) | !nzchar(valor)
    if (any(ausente)) valor[ausente] <- "<NA>"
    valor
  })
  indice_fator <- interaction(chaves, drop = TRUE, lex.order = TRUE)
  indice <- as.integer(indice_fator)
  numero_grupos <- nlevels(indice_fator)
  primeiros <- match(seq_len(numero_grupos), indice)
  valores <- registros[primeiros, grupos, drop = FALSE]
  rownames(valores) <- NULL

  list(
    grupos = grupos,
    indice = indice,
    valores = valores,
    notificacoes = as.integer(tabulate(indice, nbins = numero_grupos))
  )
}

# Soma dois vetores usando um plano existente. Somente uma matriz inteira de
# duas colunas vive durante a operação, independentemente do número de regras.
agregar_vetores_no_plano <- function(problema, ocorrencias, plano) {
  saida <- plano$valores
  saida$notificacoes <- plano$notificacoes
  if (!length(plano$indice)) {
    saida$registros_invalidos <- integer(nrow(saida))
    saida$ocorrencias_invalidas <- integer(nrow(saida))
  } else {
    valores <- cbind(
      registros_invalidos = as.integer(!is.na(problema) & problema),
      ocorrencias_invalidas = as.integer(ocorrencias)
    )
    valores[is.na(valores)] <- 0L
    somas <- rowsum(valores, plano$indice, reorder = TRUE, na.rm = TRUE)
    saida$registros_invalidos <- as.integer(somas[, "registros_invalidos"])
    saida$ocorrencias_invalidas <- as.integer(somas[, "ocorrencias_invalidas"])
  }
  saida$percentual_invalidos <- percentual_seguro(
    saida$registros_invalidos, saida$notificacoes
  )
  saida
}

# Consolida notificações, registros problemáticos e ocorrências pelos campos de
# agrupamento informados (Brasil quando `grupos` está vazio).
agregar_indicadores_validade <- function(registros, grupos = character(),
                                          plano = NULL) {
  if (is.null(plano)) plano <- criar_plano_agregacao_territorial(registros, grupos)
  agregar_vetores_no_plano(
    registros$problema_validade, registros$numero_problemas_validade, plano
  )
}

# Acrescenta a participação de cada linha no total de problemas do nível pai.
adicionar_contribuicao <- function(tabela, total_invalidos, grupos_pai = character()) {
  if (!length(grupos_pai)) {
    denominador <- rep(total_invalidos, nrow(tabela))
  } else {
    chave <- do.call(paste, c(tabela[grupos_pai], sep = "\r"))
    totais <- tapply(tabela$registros_invalidos, chave, sum, na.rm = TRUE)
    denominador <- unname(totais[chave])
  }
  tabela$contribuicao_invalidos <- percentual_seguro(tabela$registros_invalidos, denominador)
  tabela
}

# Agrega cada indicador de regra pelos níveis territoriais solicitados.
agregar_regras_validade <- function(registros, por_regra, grupos = character(),
                                     plano = NULL,
                                     fonte_indicadores = registros) {
  numeros_regras <- gsub("[^0-9]", "", as.character(por_regra$regra))
  executadas <- numeros_regras[por_regra$status == "Executada"]
  if (!length(executadas)) return(data.frame())
  nomes_colunas <- paste0("regra_", executadas)
  nomes_colunas <- nomes_colunas[nomes_colunas %in% names(fonte_indicadores)]
  if (!length(nomes_colunas)) return(data.frame())

  if (is.null(plano)) plano <- criar_plano_agregacao_territorial(registros, grupos)
  partes <- lapply(nomes_colunas, function(coluna) {
    indicador <- as.logical(fonte_indicadores[[coluna]])
    tabela <- agregar_vetores_no_plano(indicador, as.integer(indicador), plano)
    numero <- sub("regra_", "", coluna)
    tabela$regra <- rep(paste("Regra", numero), nrow(tabela))
    descricao <- por_regra$descricao[match(numero, numeros_regras)]
    tabela$descricao_regra <- rep(descricao, nrow(tabela))
    tabela
  })
  saida <- do.call(rbind, partes)
  rownames(saida) <- NULL
  saida
}

# Aplica uma ordem estável e semanticamente útil em cada nível territorial.
ordenar_tabela_territorial <- function(tabela, nivel) {
  if (!nrow(tabela)) return(tabela)
  if (nivel == "regiao") {
    tabela <- tabela[order(match(tabela$codigo_regiao, as.character(1:5))), , drop = FALSE]
  } else if (nivel == "uf") {
    tabela <- tabela[order(as.integer(tabela$codigo_uf), na.last = TRUE), , drop = FALSE]
  } else if (nivel == "municipio") {
    tabela <- tabela[order(as.integer(tabela$codigo_uf), tabela$nome_municipio,
                           tabela$codigo_sinan_6, na.last = TRUE), , drop = FALSE]
  } else if (nivel == "cnes") {
    tabela <- tabela[order(as.integer(tabela$codigo_uf), tabela$nome_unidade,
                           tabela$id_unidade, na.last = TRUE), , drop = FALSE]
  }
  rownames(tabela) <- NULL
  tabela
}

# Calcula frequência, posição, contribuição e percentual acumulado de Pareto.
calcular_pareto <- function(tabela, coluna_nome, limite = Inf) {
  if (!nrow(tabela)) return(tabela)
  ordem <- order(-tabela$registros_invalidos, -tabela$notificacoes,
                 as.character(tabela[[coluna_nome]]), na.last = TRUE)
  tabela <- tabela[ordem, , drop = FALSE]
  total <- sum(tabela$registros_invalidos, na.rm = TRUE)
  tabela$contribuicao_pareto <- percentual_seguro(tabela$registros_invalidos, total)
  tabela$contribuicao_acumulada <- cumsum(tabela$contribuicao_pareto)
  tabela$ordem_prioridade <- seq_len(nrow(tabela))
  if (is.finite(limite)) tabela <- utils::head(tabela, limite)
  rownames(tabela) <- NULL
  tabela
}

# Resume quantos itens concentram até 50%, 80% e 100% dos problemas.
resumir_faixas_pareto <- function(tabela_pareto) {
  total <- sum(tabela_pareto$registros_invalidos, na.rm = TRUE)
  if (!nrow(tabela_pareto) || total == 0) {
    return(data.frame(
      faixa = c("50%", "80%"), unidades_necessarias = 0L,
      registros_invalidos = 0L, percentual_alcancado = 0,
      stringsAsFactors = FALSE
    ))
  }
  obter <- function(limiar) {
    posicao <- which(tabela_pareto$contribuicao_acumulada >= limiar)[1]
    data.frame(
      faixa = paste0(limiar, "%"),
      unidades_necessarias = posicao,
      registros_invalidos = sum(tabela_pareto$registros_invalidos[seq_len(posicao)]),
      percentual_alcancado = tabela_pareto$contribuicao_acumulada[posicao],
      stringsAsFactors = FALSE
    )
  }
  rbind(obter(50), obter(80))
}

# Identifica, em cada território, a regra com maior número de ocorrências.
identificar_regras_dominantes <- function(tabela_regras, grupos, coluna_nome, limite = 20L) {
  if (!nrow(tabela_regras)) return(data.frame())
  chave <- do.call(paste, c(tabela_regras[c(grupos, coluna_nome)], sep = "\r"))
  partes <- split(seq_len(nrow(tabela_regras)), chave)
  indices <- vapply(partes, function(i) i[[1]], integer(1))
  saida <- tabela_regras[indices, c(grupos, coluna_nome), drop = FALSE]
  dominantes <- lapply(partes, function(i) {
    trecho <- tabela_regras[i, , drop = FALSE]
    maximo <- max(trecho$registros_invalidos, na.rm = TRUE)
    regras <- trecho$regra[trecho$registros_invalidos == maximo & maximo > 0]
    list(
      regra = if (length(regras)) paste(regras, collapse = ", ") else "Sem ocorrência",
      ocorrencias = if (is.finite(maximo)) as.integer(maximo) else 0L
    )
  })
  saida$regra_dominante <- vapply(dominantes, `[[`, character(1), "regra")
  saida$ocorrencias_regra_dominante <- vapply(dominantes, `[[`, integer(1), "ocorrencias")
  rownames(saida) <- NULL
  saida <- saida[order(-saida$ocorrencias_regra_dominante), , drop = FALSE]
  utils::head(saida, limite)
}

# Infere o período coberto pela base a partir das datas de notificação válidas.
inferir_periodo_relatorio <- function(base_padronizada) {
  if ("nu_ano" %in% names(base_padronizada)) {
    anos <- sort(unique(normalizar_codigo_territorial(base_padronizada$nu_ano, 4L)))
    anos <- anos[!is.na(anos)]
    if (length(anos) == 1L) return(anos)
    if (length(anos) > 1L) return(paste0(min(anos), "-", max(anos)))
  }
  "periodo_nao_identificado"
}

# Reúne metadados da execução que aparecerão no cabeçalho e na metodologia.
criar_config_relatorio <- function(base_padronizada, referencias,
                                   incluir_brasil = FALSE,
                                   regioes = character(), ufs = character(),
                                   municipios = character(),
                                   escala_matriz = configuracao_priorizacao()$escala_padrao,
                                   gerado_em = Sys.time()) {
  # A escala é uma escolha de apresentação. Guardá-la nos metadados garante
  # que o HTML seja aberto nessa opção e que o PDF use a mesma visualização.
  escala_matriz <- validar_escala_matriz_prioridade(escala_matriz)
  regioes <- unique(normalizar_codigo_territorial(regioes, 1L))
  regioes <- regioes[regioes %in% referencias$ufs$codigo_regiao]
  regioes <- as.character(1:5)[as.character(1:5) %in% regioes]
  ufs <- unique(normalizar_codigo_territorial(ufs, 2L))
  ufs <- referencias$ufs$codigo_uf[referencias$ufs$codigo_uf %in% ufs]
  municipios_normalizados <- unique(normalizar_codigo_territorial(municipios, 6L))
  municipios_normalizados <- municipios_normalizados[!is.na(municipios_normalizados)]
  municipios_presentes <- if ("id_municip" %in% names(base_padronizada)) {
    unique(normalizar_codigo_territorial(base_padronizada$id_municip, 6L))
  } else character()
  municipios_validos <- referencias$municipios$codigo_sinan_6[
    referencias$municipios$codigo_sinan_6 %in% municipios_normalizados &
      referencias$municipios$codigo_sinan_6 %in% municipios_presentes
  ]
  municipios_descartados <- setdiff(municipios_normalizados, municipios_validos)
  incluir_brasil <- isTRUE(incluir_brasil)
  if (!incluir_brasil && !length(regioes) && !length(ufs) &&
      !length(municipios_validos)) {
    stop(
      paste(
        "Selecione ao menos uma abrangência para gerar o relatório:",
        "Brasil, região, UF ou município."
      ),
      call. = FALSE
    )
  }
  nomes_regioes <- referencias$ufs$nome_regiao[
    match(regioes, referencias$ufs$codigo_regiao)
  ]
  linhas_municipios <- match(
    municipios_validos, referencias$municipios$codigo_sinan_6
  )
  abrangencias <- character()
  if (incluir_brasil) abrangencias <- c(abrangencias, "Brasil")
  if (length(regioes)) {
    abrangencias <- c(abrangencias, paste("Região", nomes_regioes))
  }
  if (length(ufs)) {
    abrangencias <- c(abrangencias, paste0(
      referencias$ufs$nome_uf[match(ufs, referencias$ufs$codigo_uf)],
      " (", referencias$ufs$sigla_uf[match(ufs, referencias$ufs$codigo_uf)], ")"
    ))
  }
  if (length(municipios_validos)) {
    abrangencias <- c(abrangencias, paste0(
      referencias$municipios$nome_municipio[linhas_municipios], " (",
      referencias$municipios$sigla_uf[linhas_municipios], ")"
    ))
  }
  list(
    incluir_brasil = incluir_brasil,
    regioes_selecionadas = regioes,
    ufs_selecionadas = ufs,
    municipios_selecionados = municipios_validos,
    municipios_descartados = municipios_descartados,
    descricao_abrangencias = paste(abrangencias, collapse = "; "),
    escala_matriz_prioridade = escala_matriz,
    gerado_em = as.POSIXct(gerado_em, tz = "America/Sao_Paulo"),
    periodo = inferir_periodo_relatorio(base_padronizada)
  )
}

# Ponto central de pré-agregação. Produz uma estrutura única com resumos do
# Brasil, regiões, UFs, municípios, CNES, regras e diagnósticos de relacionamento.
pre_agregar_relatorio_validade <- function(base_padronizada, resultado_validade,
                                           referencias,
                                           ufs_detalhadas = NULL,
                                           municipios_detalhados = character()) {
  registros <- preparar_registros_territoriais(
    base_padronizada, resultado_validade, referencias, incluir_regras = FALSE
  )
  relacionamentos <- resumir_relacionamentos_territoriais(registros, referencias)
  filtro_detalhe <- if (is.null(ufs_detalhadas)) {
    rep(TRUE, nrow(registros))
  } else {
    registros$codigo_uf %in% unique(as.character(ufs_detalhadas)) |
      registros$codigo_sinan_6 %in% unique(as.character(municipios_detalhados))
  }
  registros_detalhe <- registros[filtro_detalhe, , drop = FALSE]
  indicadores <- resultado_validade$por_registro
  indicadores_detalhe <- indicadores[filtro_detalhe, , drop = FALSE]

  # Cada nível é agregado e liberado antes do seguinte. Isso evita conservar
  # simultaneamente cinco vetores de agrupamento do tamanho da base.
  processar_nivel <- function(registros_nivel, indicadores_nivel, grupos) {
    plano <- criar_plano_agregacao_territorial(registros_nivel, grupos)
    totais <- agregar_indicadores_validade(
      registros_nivel, grupos, plano = plano
    )
    regras <- agregar_regras_validade(
      registros_nivel, resultado_validade$por_regra, grupos,
      plano = plano, fonte_indicadores = indicadores_nivel
    )
    rm(plano)
    invisible(gc(verbose = FALSE, full = FALSE))
    list(totais = totais, regras = regras)
  }

  nivel_brasil <- processar_nivel(registros, indicadores, character())
  brasil <- adicionar_contribuicao(
    nivel_brasil$totais, sum(registros$problema_validade)
  )
  total_invalidos <- brasil$registros_invalidos[[1]]

  nivel_regiao <- processar_nivel(
    registros, indicadores, c("codigo_regiao", "nome_regiao")
  )
  regioes <- nivel_regiao$totais
  regioes <- adicionar_contribuicao(regioes, total_invalidos)
  regioes <- ordenar_tabela_territorial(regioes, "regiao")

  grupos_uf <- c(
    "codigo_regiao", "nome_regiao", "codigo_uf", "sigla_uf", "nome_uf"
  )
  nivel_uf <- processar_nivel(registros, indicadores, grupos_uf)
  ufs <- nivel_uf$totais
  ufs <- adicionar_contribuicao(ufs, total_invalidos)
  ufs <- ordenar_tabela_territorial(ufs, "uf")

  grupos_municipio <- c(
    "codigo_regiao", "nome_regiao", "codigo_uf", "sigla_uf", "nome_uf",
    "codigo_sinan_6", "codigo_ibge_7", "nome_municipio"
  )
  nivel_municipio <- processar_nivel(
    registros_detalhe, indicadores_detalhe, grupos_municipio
  )
  municipios <- nivel_municipio$totais
  municipios <- adicionar_contribuicao(municipios, total_invalidos, c("codigo_uf"))
  municipios <- ordenar_tabela_territorial(municipios, "municipio")

  grupos_cnes <- c(
    "codigo_regiao", "nome_regiao", "codigo_uf", "sigla_uf", "nome_uf",
    "id_unidade", "nome_unidade"
  )
  nivel_cnes <- processar_nivel(
    registros_detalhe, indicadores_detalhe, grupos_cnes
  )
  cnes <- nivel_cnes$totais
  cnes <- adicionar_contribuicao(cnes, total_invalidos, c("codigo_uf"))
  cnes <- ordenar_tabela_territorial(cnes, "cnes")

  # A granularidade municipal inclui o município na chave para impedir que o
  # mesmo CNES, usado em localidades diferentes, misture seus indicadores.
  grupos_cnes_municipio <- c(
    "codigo_regiao", "nome_regiao", "codigo_uf", "sigla_uf", "nome_uf",
    "codigo_sinan_6", "codigo_ibge_7", "nome_municipio",
    "id_unidade", "nome_unidade"
  )
  nivel_cnes_municipio <- processar_nivel(
    registros_detalhe, indicadores_detalhe, grupos_cnes_municipio
  )
  cnes_municipio <- nivel_cnes_municipio$totais
  cnes_municipio <- adicionar_contribuicao(
    cnes_municipio, total_invalidos, c("codigo_sinan_6")
  )
  cnes_municipio <- ordenar_tabela_territorial(cnes_municipio, "cnes")

  list(
    brasil = brasil,
    regioes = regioes,
    ufs = ufs,
    municipios = municipios,
    cnes = cnes,
    cnes_municipio = cnes_municipio,
    regras_brasil = nivel_brasil$regras,
    regras_regiao = nivel_regiao$regras,
    regras_uf = nivel_uf$regras,
    regras_municipio = nivel_municipio$regras,
    regras_cnes = nivel_cnes$regras,
    regras_cnes_municipio = nivel_cnes_municipio$regras,
    regras_metodologicas = resultado_validade$por_regra,
    relacionamentos = relacionamentos,
    geografia_ufs = referencias$geografia_ufs,
    referencia_ufs = referencias$ufs,
    possui_referencia_cnes = referencia_cnes_disponivel(referencias),
    arquivo_referencia_cnes = if (!is.null(referencias$arquivo_cnes)) {
      basename(referencias$arquivo_cnes)
    } else NULL
  )
}
