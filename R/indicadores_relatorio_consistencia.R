# Indicadores do relatório de consistência ------------------------------------
#
# Reaproveita a estrutura territorial da validade e acrescenta os níveis de
# regra e grupo temático próprios da consistência.

# Adapta a preparação territorial para os indicadores de consistência.
preparar_registros_territoriais_consistencia <- function(base_padronizada,
                                                          resultado_consistencia,
                                                          referencias,
                                                          incluir_regras = TRUE) {
  origem <- resultado_consistencia$por_registro
  por_registro <- data.frame(
    numero_problemas_validade = origem$numero_problemas_consistencia,
    problema_validade = origem$problema_consistencia
  )
  if (isTRUE(incluir_regras)) {
    regras <- grep("^regra_[0-9]+[a-z]?$", names(origem), value = TRUE)
    for (regra in regras) por_registro[[regra]] <- origem[[regra]]
  }
  preparar_registros_territoriais(
    base_padronizada, list(por_registro = por_registro), referencias,
    incluir_regras = incluir_regras
  )
}

# Prepara uma única vez a divisão dos registros em territórios. Antes desta
# estrutura existir, cada regra reconstruía `interaction()` e `split()` para a
# mesma dimensão, o que multiplicava o custo nas bases grandes.
criar_plano_agregacao_consistencia <- function(registros, grupos = character()) {
  criar_plano_agregacao_territorial(registros, grupos)
}

# Seleciona os indicadores executáveis sem convertê-los para uma matriz. As
# colunas do data.frame permanecem compartilhadas e cada regra é agregada
# isoladamente, limitando o pico de memória a poucos vetores do tamanho da base.
preparar_cache_regras_consistencia <- function(fonte_indicadores, por_regra) {
  executadas <- por_regra[
    por_regra$status == "Executada" &
      por_regra$indicador %in% names(fonte_indicadores), ,
    drop = FALSE
  ]
  list(
    metadados = executadas,
    indicadores = fonte_indicadores[executadas$indicador]
  )
}

# Soma várias colunas em todos os territórios do plano numa única operação.
somar_matriz_plano_consistencia <- function(matriz, plano) {
  if (!is.matrix(matriz)) matriz <- as.matrix(matriz)
  if (nrow(matriz) != length(plano$indice)) {
    stop("O plano territorial não corresponde à matriz de indicadores.", call. = FALSE)
  }
  if (!ncol(matriz)) {
    return(matrix(integer(), nrow = length(plano$notificacoes), ncol = 0L))
  }
  if (!nrow(matriz)) {
    return(matrix(0L, nrow = length(plano$notificacoes), ncol = ncol(matriz)))
  }
  storage.mode(matriz) <- "integer"
  somas <- rowsum(matriz, plano$indice, reorder = TRUE, na.rm = TRUE)
  matrix(
    as.integer(somas), nrow = nrow(somas), ncol = ncol(somas),
    dimnames = dimnames(somas)
  )
}

# Cria as colunas territoriais e o denominador comum das tabelas agregadas.
iniciar_tabela_plano_consistencia <- function(plano) {
  tabela <- plano$valores
  tabela$notificacoes <- plano$notificacoes
  tabela
}

# Agrega ocorrências de cada regra pelos campos territoriais informados.
agregar_regras_consistencia <- function(registros, por_regra, grupos = character(),
                                         plano = NULL, cache_regras = NULL) {
  if (is.null(plano)) plano <- criar_plano_agregacao_consistencia(registros, grupos)
  if (is.null(cache_regras)) {
    cache_regras <- preparar_cache_regras_consistencia(registros, por_regra)
  }
  executadas <- cache_regras$metadados
  if (!nrow(executadas)) return(data.frame())

  partes <- lapply(seq_len(nrow(executadas)), function(i) {
    regra <- executadas[i, ]
    indicador <- as.logical(cache_regras$indicadores[[regra$indicador]])
    tabela <- agregar_vetores_no_plano(
      indicador, as.integer(indicador), plano
    )
    tabela$regra_id <- rep(as.character(regra$regra), nrow(tabela))
    tabela$regra <- rep(paste("Regra", regra$regra), nrow(tabela))
    tabela$descricao_regra <- rep(regra$descricao, nrow(tabela))
    tabela$grupo_id <- rep(as.integer(regra$grupo_id), nrow(tabela))
    tabela$grupo <- rep(regra$grupo, nrow(tabela))
    tabela
  })
  saida <- do.call(rbind, partes)
  rownames(saida) <- NULL
  saida
}

# Agrega grupos, contando uma notificação somente uma vez por grupo, ainda que
# ela tenha sido marcada em várias regras do mesmo tema.
agregar_grupos_consistencia <- function(registros, por_regra, por_grupo,
                                         grupos = character(), plano = NULL,
                                         cache_regras = NULL) {
  if (is.null(plano)) plano <- criar_plano_agregacao_consistencia(registros, grupos)
  if (is.null(cache_regras)) {
    cache_regras <- preparar_cache_regras_consistencia(registros, por_regra)
  }
  metadados_regras <- cache_regras$metadados
  partes <- lapply(seq_len(nrow(por_grupo)), function(i) {
    grupo_id_atual <- por_grupo$grupo_id[[i]]
    indices <- which(metadados_regras$grupo_id == grupo_id_atual)
    if (!length(indices)) return(NULL)
    ocorrencias <- integer(length(plano$indice))
    for (indice_regra in indices) {
      indicador <- cache_regras$indicadores[[
        metadados_regras$indicador[[indice_regra]]
      ]]
      ocorrencias <- ocorrencias + as.integer(!is.na(indicador) & indicador)
    }
    grupo <- por_grupo[i, ]
    tabela <- agregar_vetores_no_plano(
      ocorrencias > 0L, ocorrencias, plano
    )
    tabela$grupo_id <- rep(as.integer(grupo$grupo_id), nrow(tabela))
    tabela$grupo <- rep(grupo$grupo, nrow(tabela))
    tabela$status_grupo <- rep(grupo$status, nrow(tabela))
    tabela
  })
  partes <- partes[!vapply(partes, is.null, logical(1))]
  if (!length(partes)) return(data.frame())
  saida <- do.call(rbind, partes)
  rownames(saida) <- NULL
  saida
}

# Identifica o grupo ou a regra que mais contribui para cada entidade territorial.
identificar_itens_dominantes_consistencia <- function(tabela, grupos, coluna_nome,
                                                       coluna_item, nome_saida,
                                                       limite = 20L) {
  if (!nrow(tabela)) return(data.frame())
  chave <- do.call(paste, c(tabela[c(grupos, coluna_nome)], sep = "\r"))
  partes <- split(seq_len(nrow(tabela)), chave)
  indices <- vapply(partes, function(i) i[[1]], integer(1))
  saida <- tabela[indices, c(grupos, coluna_nome), drop = FALSE]
  dominantes <- lapply(partes, function(i) {
    trecho <- tabela[i, , drop = FALSE]
    maximo <- max(trecho$registros_invalidos, na.rm = TRUE)
    itens <- trecho[[coluna_item]][trecho$registros_invalidos == maximo & maximo > 0]
    list(
      item = if (length(itens)) paste(unique(itens), collapse = ", ") else "Sem ocorrência",
      ocorrencias = if (is.finite(maximo)) as.integer(maximo) else 0L
    )
  })
  saida[[nome_saida]] <- vapply(dominantes, `[[`, character(1), "item")
  saida$ocorrencias_dominantes <- vapply(dominantes, `[[`, integer(1), "ocorrencias")
  rownames(saida) <- NULL
  saida <- saida[order(-saida$ocorrencias_dominantes), , drop = FALSE]
  utils::head(saida, limite)
}

# Constrói todas as tabelas necessárias ao relatório de consistência, incluindo
# resultados nacionais, regionais, estaduais, municipais e por estabelecimento.
pre_agregar_relatorio_consistencia <- function(base_padronizada,
                                                resultado_consistencia,
                                                referencias,
                                                ufs_detalhadas = NULL,
                                                municipios_detalhados = character()) {
  registros <- preparar_registros_territoriais_consistencia(
    base_padronizada, resultado_consistencia, referencias,
    incluir_regras = FALSE
  )
  relacionamentos <- resumir_relacionamentos_territoriais(registros, referencias)
  filtro_detalhe <- if (is.null(ufs_detalhadas)) {
    rep(TRUE, nrow(registros))
  } else {
    registros$codigo_uf %in% unique(as.character(ufs_detalhadas)) |
      registros$codigo_sinan_6 %in% unique(as.character(municipios_detalhados))
  }
  registros_detalhe <- registros[filtro_detalhe, , drop = FALSE]
  indicadores <- resultado_consistencia$por_registro
  indicadores_detalhe <- indicadores[filtro_detalhe, , drop = FALSE]

  # O plano, os vetores temporários de grupo e as somas de cada nível são
  # descartados antes de iniciar o nível seguinte.
  processar_nivel <- function(registros_nivel, indicadores_nivel, grupos) {
    plano <- criar_plano_agregacao_consistencia(registros_nivel, grupos)
    cache <- preparar_cache_regras_consistencia(
      indicadores_nivel, resultado_consistencia$por_regra
    )
    totais <- agregar_indicadores_validade(
      registros_nivel, grupos, plano = plano
    )
    regras <- agregar_regras_consistencia(
      registros_nivel, resultado_consistencia$por_regra, grupos,
      plano = plano, cache_regras = cache
    )
    grupos_agregados <- agregar_grupos_consistencia(
      registros_nivel, resultado_consistencia$por_regra,
      resultado_consistencia$por_grupo, grupos,
      plano = plano, cache_regras = cache
    )
    rm(plano, cache)
    invisible(gc(verbose = FALSE, full = FALSE))
    list(totais = totais, regras = regras, grupos = grupos_agregados)
  }

  nivel_brasil <- processar_nivel(registros, indicadores, character())
  brasil <- adicionar_contribuicao(
    nivel_brasil$totais, sum(registros$problema_validade)
  )
  total_inconsistentes <- brasil$registros_invalidos[[1]]

  grupos_regiao <- c("codigo_regiao", "nome_regiao")
  nivel_regiao <- processar_nivel(registros, indicadores, grupos_regiao)
  regioes <- nivel_regiao$totais
  regioes <- adicionar_contribuicao(regioes, total_inconsistentes)
  regioes <- ordenar_tabela_territorial(regioes, "regiao")

  grupos_uf <- c(
    "codigo_regiao", "nome_regiao", "codigo_uf", "sigla_uf", "nome_uf"
  )
  nivel_uf <- processar_nivel(registros, indicadores, grupos_uf)
  ufs <- nivel_uf$totais
  ufs <- adicionar_contribuicao(ufs, total_inconsistentes)
  ufs <- ordenar_tabela_territorial(ufs, "uf")

  grupos_municipio <- c(
    "codigo_regiao", "nome_regiao", "codigo_uf", "sigla_uf", "nome_uf",
    "codigo_sinan_6", "codigo_ibge_7", "nome_municipio"
  )
  nivel_municipio <- processar_nivel(
    registros_detalhe, indicadores_detalhe, grupos_municipio
  )
  municipios <- nivel_municipio$totais
  municipios <- adicionar_contribuicao(municipios, total_inconsistentes, c("codigo_uf"))
  municipios <- ordenar_tabela_territorial(municipios, "municipio")

  grupos_cnes <- c(
    "codigo_regiao", "nome_regiao", "codigo_uf", "sigla_uf", "nome_uf",
    "id_unidade", "nome_unidade"
  )
  nivel_cnes <- processar_nivel(
    registros_detalhe, indicadores_detalhe, grupos_cnes
  )
  cnes <- nivel_cnes$totais
  cnes <- adicionar_contribuicao(cnes, total_inconsistentes, c("codigo_uf"))
  cnes <- ordenar_tabela_territorial(cnes, "cnes")

  # Esta chave mantém o município junto ao CNES. A estrutura estadual acima é
  # preservada sem mudanças para não alterar os relatórios de UF existentes.
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
    cnes_municipio, total_inconsistentes, c("codigo_sinan_6")
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
    grupos_brasil = nivel_brasil$grupos,
    grupos_regiao = nivel_regiao$grupos,
    grupos_uf = nivel_uf$grupos,
    grupos_municipio = nivel_municipio$grupos,
    grupos_cnes = nivel_cnes$grupos,
    grupos_cnes_municipio = nivel_cnes_municipio$grupos,
    regras_metodologicas = resultado_consistencia$por_regra,
    grupos_metodologicos = resultado_consistencia$por_grupo,
    relacionamentos = relacionamentos,
    geografia_ufs = referencias$geografia_ufs,
    referencia_ufs = referencias$ufs,
    possui_referencia_cnes = referencia_cnes_disponivel(referencias),
    arquivo_referencia_cnes = if (!is.null(referencias$arquivo_cnes)) {
      basename(referencias$arquivo_cnes)
    } else NULL
  )
}
