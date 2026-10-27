# Geração do relatório de consistência ----------------------------------------
#
# Segue a mesma arquitetura visual do relatório de validade, acrescentando a
# hierarquia grupo > regra. Blocos `<details>` permitem expandir ou contrair os
# resultados sem duplicar cálculos ou dados no documento.

# Reaproveita os rótulos territoriais e os expõe com nomes específicos do módulo.
preparar_rotulos_relatorio_consistencia <- function(dados) {
  dados <- preparar_rotulos_relatorio(dados)
  dados$cnes$nome_exibicao <- gsub("—", "-", dados$cnes$nome_exibicao, fixed = TRUE)
  if (!is.null(dados$cnes_municipio)) {
    dados$cnes_municipio$nome_exibicao <- gsub(
      "—", "-", dados$cnes_municipio$nome_exibicao, fixed = TRUE
    )
  }
  dados
}

# Comparação vetorial protegida contra valores ausentes.
igual_relatorio_consistencia <- function(x, valor) !is.na(x) & x == valor

# Cartões de totais da seção atual.
cards_resumo_consistencia_relatorio <- function(resumo) {
  valores <- list(
    "Total de Notificações" = formatar_inteiro_relatorio(resumo$notificacoes),
    "Total de Registros inconsistentes" = formatar_inteiro_relatorio(resumo$registros_invalidos),
    "Percentual de registros inconsistentes" = formatar_percentual_relatorio(resumo$percentual_invalidos),
    "Total de inconsistências" = formatar_inteiro_relatorio(resumo$ocorrencias_invalidas)
  )
  htmltools::tags$div(
    class = "cards-resumo",
    lapply(names(valores), function(nome) htmltools::tags$div(
      class = "card-resumo",
      htmltools::tags$span(nome),
      htmltools::tags$strong(valores[[nome]])
    ))
  )
}

# Completa a tabela de grupos com itens metodológicos sem resultado executável.
consolidar_grupos_consistencia_relatorio <- function(tabela, grupos_metodologicos) {
  consolidada <- grupos_metodologicos[
    , c("grupo_id", "grupo", "status"), drop = FALSE
  ]
  indice <- match(consolidada$grupo_id, tabela$grupo_id)
  consolidada$registros_invalidos <- tabela$registros_invalidos[indice]
  consolidada$percentual_invalidos <- tabela$percentual_invalidos[indice]
  consolidada
}

# Faz o mesmo para regras e preserva sua vinculação ao grupo temático.
consolidar_regras_consistencia_relatorio <- function(tabela, regras_metodologicas,
                                                      grupo_id) {
  consolidada <- regras_metodologicas[
    regras_metodologicas$grupo_id == grupo_id,
    c("regra", "descricao", "status", "detalhe"), drop = FALSE
  ]
  consolidada$regra_id <- as.character(consolidada$regra)
  consolidada$regra <- paste("Regra", consolidada$regra)
  indice <- match(consolidada$regra_id, tabela$regra_id)
  consolidada$registros_invalidos <- tabela$registros_invalidos[indice]
  consolidada$percentual_invalidos <- tabela$percentual_invalidos[indice]
  consolidada
}

# Monta o núcleo expansível grupo > regras. A síntese geral mantém o gráfico
# por grupos; dentro da expansão, cada grupo apresenta somente sua tabela.
bloco_resultados_consistencia_relatorio <- function(tabela_grupos, tabela_regras,
                                                     dados, titulo) {
  dimensoes_grupos <- dimensoes_figura_relatorio("grupos")
  controles <- configuracao_de_relatorio("consistencia")$controles
  grupos <- consolidar_grupos_consistencia_relatorio(
    tabela_grupos, dados$grupos_metodologicos
  )
  detalhes <- lapply(seq_len(nrow(dados$grupos_metodologicos)), function(i) {
    grupo <- dados$grupos_metodologicos[i, ]
    regras <- consolidar_regras_consistencia_relatorio(
      tabela_regras, dados$regras_metodologicas, grupo$grupo_id
    )
    htmltools::tags$div(
      class = "grupo-regras-relatorio",
      htmltools::tags$h4(grupo$grupo),
      tabela_html_relatorio(
        regras,
        c("regra", "descricao", "status", "detalhe",
          "registros_invalidos", "percentual_invalidos"),
        c("Regra", "Descrição", "Status", "Detalhe",
          "Registros sinalizados", "% das notificações")
      )
    )
  })

  htmltools::tagList(
    htmltools::tags$div(
      class = "subtitulo-resultados",
      htmltools::tags$h4(titulo_de_relatorio("consistencia", "sintese_grupos")),
      htmltools::tags$p(
        class = "observacao",
        "Cada registro é contado uma única vez dentro de cada grupo, mesmo quando atende a várias regras do grupo."
      )
    ),
    tabela_html_relatorio(
      grupos,
      c("grupo", "status", "registros_invalidos", "percentual_invalidos"),
      c("Grupo", "Status", "Registros sinalizados", "% das notificações")
    ),
    imagem_grafico_relatorio(
      grafico_grupos_consistencia_relatorio(tabela_grupos, paste(titulo, "- grupos")),
      paste("Resultados por grupos de consistência em", titulo),
      altura = dimensoes_grupos$altura
    ),
    htmltools::tags$details(
      class = "detalhes-regras-consistencia",
      htmltools::tags$summary(controles$abrir_regras),
      htmltools::tags$div(class = "conteudo-regras-consistencia", detalhes)
    )
  )
}

# Exibe o mapa territorial para um indicador de consistência selecionado.
bloco_mapa_consistencia_relatorio <- function(dados, tabela_ufs,
                                               codigo_regiao = NULL,
                                               prefixo = "Brasil") {
  if (is.null(dados$geografia_ufs)) {
    return(htmltools::tags$p(
      class = "aviso",
      "Malha geográfica indisponível; os totais permanecem nas tabelas."
    ))
  }
  # Dimensões e disposição são editáveis em `configuracao_aplicativo.R`.
  dimensoes <- dimensoes_figura_relatorio("mapas")
  dados_mapa <- preparar_dados_mapa_interativo_relatorio(
    dados$geografia_ufs, tabela_ufs, codigo_regiao
  )
  htmltools::tags$div(
    class = paste("grade-graficos", classe_layout_mapas()),
    bloco_mapa_indicador_relatorio(
      dados_mapa,
      mapa_ufs_consistencia(
        dados$geografia_ufs, tabela_ufs, "registros_invalidos",
        paste(prefixo, "- Número de registros inconsistentes"), codigo_regiao
      ),
      "registros_invalidos", paste(prefixo, "- Número de registros inconsistentes"),
      paste("Mapa", prefixo, "por volume de registros inconsistentes"),
      "consistencia", dimensoes
    ),
    bloco_mapa_indicador_relatorio(
      dados_mapa,
      mapa_ufs_consistencia(
        dados$geografia_ufs, tabela_ufs, "percentual_invalidos",
        paste(prefixo, "- Percentual de registros inconsistentes"), codigo_regiao
      ),
      "percentual_invalidos", paste(prefixo, "- Percentual de registros inconsistentes"),
      paste("Mapa", prefixo, "por percentual de registros inconsistentes"),
      "consistencia", dimensoes
    )
  )
}

# Seções hierárquicas: Brasil, região e UF; cada nível reutiliza o mesmo bloco
# de grupos e regras com os filtros territoriais correspondentes.
secao_brasil_consistencia_relatorio <- function(dados, numero = 1L) {
  ufs <- calcular_pareto(dados$ufs, "nome_exibicao")
  htmltools::tags$section(
    id = "brasil", class = "secao-relatorio",
    htmltools::tags$h2(titulo_numerado_relatorio(
      numero, titulo_de_relatorio("consistencia", "brasil")
    )),
    htmltools::tags$h3(titulo_numerado_relatorio(
      numero, titulo_de_relatorio("consistencia", "sintese_nacional"), 1L
    )),
    cards_resumo_consistencia_relatorio(dados$brasil[1, ]),
    htmltools::tags$h3(titulo_numerado_relatorio(
      numero, titulo_de_relatorio("consistencia", "resultados_nacionais"), 2L
    )),
    bloco_resultados_consistencia_relatorio(
      dados$grupos_brasil, dados$regras_brasil, dados, "Brasil"
    ),
    htmltools::tags$h3(titulo_numerado_relatorio(
      numero, titulo_de_relatorio("consistencia", "distribuicao_uf"), 3L
    )),
    tabela_html_relatorio(
      ufs,
      c("nome_exibicao", "notificacoes", "registros_invalidos",
        "percentual_invalidos", "contribuicao_pareto"),
      c("UF", "Notificações", "Inconsistentes", "% inconsistente",
        "Contribuição")
    ),
    htmltools::tags$h3(titulo_numerado_relatorio(
      numero, titulo_de_relatorio("consistencia", "mapas"), 4L
    )),
    bloco_mapa_consistencia_relatorio(dados, dados$ufs)
  )
}

# Monta uma seção regional usando somente a região selecionada pelo usuário.
secao_regiao_consistencia_relatorio <- function(dados, codigo_regiao, numero) {
  resumo <- dados$regioes[
    igual_relatorio_consistencia(dados$regioes$codigo_regiao, codigo_regiao), ,
    drop = FALSE
  ]
  ufs <- dados$ufs[
    igual_relatorio_consistencia(dados$ufs$codigo_regiao, codigo_regiao), ,
    drop = FALSE
  ]
  regras <- dados$regras_regiao[
    igual_relatorio_consistencia(dados$regras_regiao$codigo_regiao, codigo_regiao), ,
    drop = FALSE
  ]
  grupos <- dados$grupos_regiao[
    igual_relatorio_consistencia(dados$grupos_regiao$codigo_regiao, codigo_regiao), ,
    drop = FALSE
  ]
  nome <- if (nrow(resumo)) resumo$nome_regiao[[1]] else paste("Região", codigo_regiao)
  htmltools::tags$section(
    id = paste0("regiao-", codigo_regiao), class = "secao-relatorio quebra-pagina",
    htmltools::tags$h2(titulo_numerado_relatorio(
      numero, sprintf(titulo_de_relatorio("consistencia", "regiao"), nome)
    )),
    cards_resumo_consistencia_relatorio(resumo[1, ]),
    htmltools::tags$h3(titulo_numerado_relatorio(
      numero, titulo_de_relatorio("consistencia", "resultados_territorio"), 1L
    )),
    bloco_resultados_consistencia_relatorio(grupos, regras, dados, paste("Região", nome)),
    htmltools::tags$h3(titulo_numerado_relatorio(
      numero, titulo_de_relatorio("consistencia", "comparacao_ufs"), 2L
    )),
    tabela_html_relatorio(
      calcular_pareto(ufs, "nome_exibicao"),
      c("nome_exibicao", "notificacoes", "registros_invalidos",
        "percentual_invalidos", "contribuicao_pareto"),
      c("UF", "Notificações", "Inconsistentes", "% inconsistente",
        "Contribuição")
    ),
    bloco_mapa_consistencia_relatorio(
      dados, dados$ufs, codigo_regiao, paste("Região", nome)
    )
  )
}

# Acrescenta nomes/códigos da entidade ao resultado de itens dominantes.
adicionar_entidade_dominante_consistencia <- function(tabela, entidades, codigos,
                                                       coluna_codigo, prefixo) {
  if (!nrow(tabela)) return(tabela)
  tabela$entidade <- rotular_entidade(
    entidades[match(tabela[[coluna_codigo]], codigos)],
    tabela[[coluna_codigo]], prefixo
  )
  tabela
}

# Apresenta, lado a lado, grupo e regra predominantes por entidade.
bloco_dominantes_consistencia_relatorio <- function(grupos, regras, titulo_entidade) {
  htmltools::tagList(
    htmltools::tags$h4(paste("Grupos dominantes nos principais", titulo_entidade)),
    tabela_html_relatorio(
      grupos,
      c("entidade", "grupo_dominante", "ocorrencias_dominantes"),
      c("Unidade", "Grupo dominante", "Ocorrências")
    ),
    htmltools::tags$details(
      class = "detalhes-regras-consistencia",
      htmltools::tags$summary(
        configuracao_de_relatorio("consistencia")$controles$abrir_dominantes
      ),
      tabela_html_relatorio(
        regras,
        c("entidade", "regra_dominante", "ocorrencias_dominantes"),
        c("Unidade", "Grupo e regra dominante", "Ocorrências")
      )
    )
  )
}

# Monta o detalhamento estadual, incluindo municípios e estabelecimentos.
secao_uf_consistencia_relatorio <- function(
    dados, codigo_uf, numero,
    escala_matriz = configuracao_priorizacao()$escala_padrao) {
  resumo <- dados$ufs[
    igual_relatorio_consistencia(dados$ufs$codigo_uf, codigo_uf), , drop = FALSE
  ]
  if (!nrow(resumo)) return(NULL)
  nome <- paste0(resumo$nome_uf[[1]], " (", resumo$sigla_uf[[1]], ")")
  municipios <- dados$municipios[
    igual_relatorio_consistencia(dados$municipios$codigo_uf, codigo_uf), , drop = FALSE
  ]
  unidades <- dados$cnes[
    igual_relatorio_consistencia(dados$cnes$codigo_uf, codigo_uf), , drop = FALSE
  ]
  regras <- dados$regras_uf[
    igual_relatorio_consistencia(dados$regras_uf$codigo_uf, codigo_uf), , drop = FALSE
  ]
  grupos <- dados$grupos_uf[
    igual_relatorio_consistencia(dados$grupos_uf$codigo_uf, codigo_uf), , drop = FALSE
  ]
  regras_municipio <- dados$regras_municipio[
    igual_relatorio_consistencia(dados$regras_municipio$codigo_uf, codigo_uf), ,
    drop = FALSE
  ]
  grupos_municipio <- dados$grupos_municipio[
    igual_relatorio_consistencia(dados$grupos_municipio$codigo_uf, codigo_uf), ,
    drop = FALSE
  ]
  regras_cnes <- dados$regras_cnes[
    igual_relatorio_consistencia(dados$regras_cnes$codigo_uf, codigo_uf), ,
    drop = FALSE
  ]
  grupos_cnes <- dados$grupos_cnes[
    igual_relatorio_consistencia(dados$grupos_cnes$codigo_uf, codigo_uf), ,
    drop = FALSE
  ]
  pareto_municipios <- calcular_pareto(municipios, "nome_exibicao")
  pareto_cnes <- calcular_pareto(unidades, "nome_exibicao")
  limite_ranking <- configuracao_geral_relatorios()$limites$ranking
  dimensoes_pareto <- dimensoes_figura_relatorio("pareto")

  regras_municipio$regra_aninhada <- paste(
    regras_municipio$grupo, ">", regras_municipio$regra
  )
  regras_cnes$regra_aninhada <- paste(regras_cnes$grupo, ">", regras_cnes$regra)
  dominantes_grupos_municipios <- identificar_itens_dominantes_consistencia(
    grupos_municipio, "codigo_uf", "codigo_sinan_6", "grupo", "grupo_dominante"
  )
  dominantes_regras_municipios <- identificar_itens_dominantes_consistencia(
    regras_municipio, "codigo_uf", "codigo_sinan_6", "regra_aninhada", "regra_dominante"
  )
  dominantes_grupos_cnes <- identificar_itens_dominantes_consistencia(
    grupos_cnes, "codigo_uf", "id_unidade", "grupo", "grupo_dominante"
  )
  dominantes_regras_cnes <- identificar_itens_dominantes_consistencia(
    regras_cnes, "codigo_uf", "id_unidade", "regra_aninhada", "regra_dominante"
  )

  dominantes_grupos_municipios <- adicionar_entidade_dominante_consistencia(
    dominantes_grupos_municipios, municipios$nome_municipio,
    municipios$codigo_sinan_6, "codigo_sinan_6", "Município"
  )
  dominantes_regras_municipios <- adicionar_entidade_dominante_consistencia(
    dominantes_regras_municipios, municipios$nome_municipio,
    municipios$codigo_sinan_6, "codigo_sinan_6", "Município"
  )
  nomes_cnes <- if (isTRUE(dados$possui_referencia_cnes)) {
    unidades$nome_unidade
  } else {
    paste("CNES", unidades$id_unidade)
  }
  dominantes_grupos_cnes <- adicionar_entidade_dominante_consistencia(
    dominantes_grupos_cnes, nomes_cnes, unidades$id_unidade,
    "id_unidade", "CNES"
  )
  dominantes_regras_cnes <- adicionar_entidade_dominante_consistencia(
    dominantes_regras_cnes, nomes_cnes, unidades$id_unidade,
    "id_unidade", "CNES"
  )

  htmltools::tags$section(
    id = paste0("uf-", tolower(resumo$sigla_uf[[1]])),
    class = "secao-relatorio quebra-pagina",
    htmltools::tags$h2(titulo_numerado_relatorio(
      numero, sprintf(titulo_de_relatorio("consistencia", "uf"), nome)
    )),
    cards_resumo_consistencia_relatorio(resumo[1, ]),
    htmltools::tags$h3(titulo_numerado_relatorio(
      numero, titulo_de_relatorio("consistencia", "resultados_territorio"), 1L
    )),
    bloco_resultados_consistencia_relatorio(grupos, regras, dados, nome),
    htmltools::tags$h3(titulo_numerado_relatorio(
      numero, titulo_de_relatorio("consistencia", "municipios"), 2L
    )),
    bloco_pareto_relatorio(
      municipios, "nome_exibicao", paste(nome, "- Pareto dos municípios"),
      grafico_pareto_consistencia(
        municipios, "nome_exibicao", paste(nome, "- Pareto dos municípios")
      ),
      paste("Pareto de municípios de", nome), "consistencia", dimensoes_pareto
    ),
    tabela_html_relatorio(
      pareto_municipios,
      c("nome_exibicao", "notificacoes", "registros_invalidos",
        "percentual_invalidos", "contribuicao_pareto"),
      c("Município", "Notificações", "Inconsistentes", "% inconsistente",
        "Contribuição"), limite = limite_ranking
    ),
    bloco_matriz_prioridade_relatorio(
      municipios, "nome_exibicao", "codigo_sinan_6",
      paste(nome, "- matriz de prioridade municipal"), "consistencia",
      regras_dominantes = dominantes_regras_municipios,
      grupos_dominantes = dominantes_grupos_municipios,
      escala_matriz = escala_matriz
    ),
    bloco_dominantes_consistencia_relatorio(
      dominantes_grupos_municipios, dominantes_regras_municipios, "municípios"
    ),
    htmltools::tags$h3(titulo_numerado_relatorio(
      numero, titulo_de_relatorio("consistencia", "estabelecimentos"), 3L
    )),
    if (!isTRUE(dados$possui_referencia_cnes)) htmltools::tags$p(
      class = "aviso",
      if (!is.null(dados$arquivo_referencia_cnes)) {
        paste0(dados$arquivo_referencia_cnes,
               " ainda não contém registros; os estabelecimentos são exibidos somente pelo código CNES.")
      } else {
        "cnes.xlsx ou cnes.csv não está disponível; os estabelecimentos são exibidos somente pelo código CNES."
      }
    ),
    bloco_pareto_relatorio(
      unidades, "nome_exibicao", paste(nome, "- Pareto dos estabelecimentos"),
      grafico_pareto_consistencia(
        unidades, "nome_exibicao", paste(nome, "- Pareto dos estabelecimentos")
      ),
      paste("Pareto de estabelecimentos de", nome), "consistencia", dimensoes_pareto
    ),
    tabela_html_relatorio(
      pareto_cnes,
      c("nome_exibicao", "notificacoes", "registros_invalidos",
        "percentual_invalidos", "contribuicao_pareto"),
      c("Estabelecimento", "Notificações", "Inconsistentes", "% inconsistente",
        "Contribuição"), limite = limite_ranking
    ),
    bloco_matriz_prioridade_relatorio(
      unidades, "nome_exibicao", "id_unidade",
      paste(nome, "- matriz de prioridade dos estabelecimentos"), "consistencia",
      regras_dominantes = dominantes_regras_cnes,
      grupos_dominantes = dominantes_grupos_cnes,
      escala_matriz = escala_matriz
    ),
    bloco_dominantes_consistencia_relatorio(
      dominantes_grupos_cnes, dominantes_regras_cnes, "estabelecimentos"
    )
  )
}

# Monta uma seção municipal sem rankings de outros municípios. Os grupos e as
# regras já calculados são exibidos para o território, e as unidades daquele
# município formam o único nível interno de priorização.
secao_municipio_consistencia_relatorio <- function(
    dados, codigo_municipio, numero,
    escala_matriz = configuracao_priorizacao()$escala_padrao) {
  no_municipio <- function(tabela) {
    tabela[
      !is.na(tabela$codigo_sinan_6) &
        tabela$codigo_sinan_6 == codigo_municipio, , drop = FALSE
    ]
  }
  resumo <- no_municipio(dados$municipios)
  if (!nrow(resumo)) return(NULL)
  nome <- paste0(resumo$nome_municipio[[1]], " (", resumo$sigla_uf[[1]], ")")
  unidades <- no_municipio(dados$cnes_municipio)
  regras <- no_municipio(dados$regras_municipio)
  grupos <- no_municipio(dados$grupos_municipio)
  regras_cnes <- no_municipio(dados$regras_cnes_municipio)
  grupos_cnes <- no_municipio(dados$grupos_cnes_municipio)
  regras_cnes$regra_aninhada <- paste(regras_cnes$grupo, ">", regras_cnes$regra)
  pareto_cnes <- calcular_pareto(unidades, "nome_exibicao")
  limite_ranking <- configuracao_geral_relatorios()$limites$ranking
  dimensoes_pareto <- dimensoes_figura_relatorio("pareto")
  dominantes_grupos_cnes <- identificar_itens_dominantes_consistencia(
    grupos_cnes, "codigo_sinan_6", "id_unidade", "grupo", "grupo_dominante"
  )
  dominantes_regras_cnes <- identificar_itens_dominantes_consistencia(
    regras_cnes, "codigo_sinan_6", "id_unidade", "regra_aninhada", "regra_dominante"
  )
  nomes_cnes <- if (isTRUE(dados$possui_referencia_cnes)) {
    unidades$nome_unidade
  } else {
    paste("CNES", unidades$id_unidade)
  }
  dominantes_grupos_cnes <- adicionar_entidade_dominante_consistencia(
    dominantes_grupos_cnes, nomes_cnes, unidades$id_unidade,
    "id_unidade", "CNES"
  )
  dominantes_regras_cnes <- adicionar_entidade_dominante_consistencia(
    dominantes_regras_cnes, nomes_cnes, unidades$id_unidade,
    "id_unidade", "CNES"
  )

  htmltools::tags$section(
    id = paste0("municipio-", codigo_municipio),
    class = "secao-relatorio quebra-pagina secao-municipio",
    htmltools::tags$h2(titulo_numerado_relatorio(
      numero, sprintf(titulo_de_relatorio("consistencia", "municipio"), nome)
    )),
    cards_resumo_consistencia_relatorio(resumo[1, ]),
    htmltools::tags$h3(titulo_numerado_relatorio(
      numero, titulo_de_relatorio("consistencia", "resultados_territorio"), 1L
    )),
    bloco_resultados_consistencia_relatorio(grupos, regras, dados, nome),
    htmltools::tags$h3(titulo_numerado_relatorio(
      numero, titulo_de_relatorio("consistencia", "estabelecimentos"), 2L
    )),
    if (!isTRUE(dados$possui_referencia_cnes)) htmltools::tags$p(
      class = "aviso",
      if (!is.null(dados$arquivo_referencia_cnes)) {
        paste0(
          dados$arquivo_referencia_cnes,
          " ainda não contém registros; os estabelecimentos são exibidos somente pelo código CNES."
        )
      } else {
        "cnes.xlsx ou cnes.csv não está disponível; os estabelecimentos são exibidos somente pelo código CNES."
      }
    ),
    bloco_pareto_relatorio(
      unidades, "nome_exibicao", paste(nome, "- Pareto dos estabelecimentos"),
      grafico_pareto_consistencia(
        unidades, "nome_exibicao", paste(nome, "- Pareto dos estabelecimentos")
      ),
      paste("Pareto de estabelecimentos de", nome), "consistencia", dimensoes_pareto
    ),
    tabela_html_relatorio(
      pareto_cnes,
      c("nome_exibicao", "notificacoes", "registros_invalidos",
        "percentual_invalidos", "contribuicao_pareto"),
      c("Estabelecimento", "Notificações", "Inconsistentes", "% inconsistente",
        "Contribuição"), limite = limite_ranking
    ),
    bloco_matriz_prioridade_relatorio(
      unidades, "nome_exibicao", "id_unidade",
      paste(nome, "- matriz de prioridade dos estabelecimentos"), "consistencia",
      regras_dominantes = dominantes_regras_cnes,
      grupos_dominantes = dominantes_grupos_cnes,
      escala_matriz = escala_matriz
    ),
    bloco_dominantes_consistencia_relatorio(
      dominantes_grupos_cnes, dominantes_regras_cnes, "estabelecimentos"
    )
  )
}

# Explica os grupos, as regras e as referências empregadas no processamento.
secao_metodologia_consistencia_relatorio <- function(dados, numero) {
  htmltools::tags$section(
    id = "metodologia", class = "secao-relatorio quebra-pagina",
    htmltools::tags$h2(titulo_numerado_relatorio(
      numero, titulo_de_relatorio("consistencia", "metodologia")
    )),
    htmltools::tags$p(
      "O relatório utiliza exclusivamente a base padronizada e os indicadores já produzidos pela Análise de Consistência. As regras não são recalculadas durante a emissão."
    ),
    htmltools::tags$p(
      "A territorialização representa o local de notificação: sg_uf_not para UF, id_municip para município e id_unidade para estabelecimento CNES. Endereço de residência e local de ocorrência não são utilizados."
    ),
    htmltools::tags$p(
      "As abrangências exibidas são escolhidas pelo usuário e a seção nacional é opcional. Nas análises municipais, id_municip identifica o município de notificação e as unidades notificadoras constituem o nível interno de priorização. A ausência de uma abrangência no documento não remove seus registros da base."
    ),
    htmltools::tags$p(
      "Um registro pode atender a várias regras e a vários grupos. Nos totais de cada grupo, cada registro é contado uma única vez; ocorrências correspondem à soma dos indicadores individuais."
    ),
    htmltools::tags$p(
      "Volume é o número absoluto de registros inconsistentes; qualidade relativa é o percentual de inconsistentes no total do território; contribuição é a participação do território no total de inconsistentes."
    ),
    bloco_metodologia_priorizacao(),
    htmltools::tags$h3(titulo_numerado_relatorio(
      numero, titulo_de_relatorio("consistencia", "relacionamentos"), 1L
    )),
    tabela_html_relatorio(
      dados$relacionamentos, c("relacionamento", "registros", "observacao"),
      c("Verificação", "Registros", "Observação")
    ),
    htmltools::tags$p(
      class = "observacao",
      "Registros com código ausente ou não localizado permanecem nos totais gerais e são explicitados nesta auditoria; nenhuma linha é descartada silenciosamente."
    )
  )
}

# Estilos adicionais sobre a base visual compartilhada com validade.
css_relatorio_consistencia <- function() {
  paste0(
    css_relatorio_validade(),
    "
    .controles-regras { position:sticky; top:45px; z-index:2; display:flex; gap:8px;
      flex-wrap:wrap; background:var(--superficie-marca); border-bottom:1px solid var(--borda); padding:10px 5vw; }
    .controles-regras button { min-height:36px; border:1px solid var(--principal); border-radius:8px;
      color:var(--principal); background:white; cursor:pointer; font-weight:bold; padding:7px 11px; }
    .controles-regras button:hover,.controles-regras button:focus { color:white;
      background:var(--principal); outline:none; box-shadow:0 0 0 3px rgba(144,8,152,.12); }
    .detalhes-regras-consistencia { border:1px solid var(--borda); border-radius:9px;
      margin:18px 0 26px; overflow:hidden; }
    .detalhes-regras-consistencia > summary { background:var(--superficie-marca); color:var(--principal);
      cursor:pointer; font-weight:bold; padding:12px 15px; }
    .conteudo-regras-consistencia { padding:4px 16px 16px; }
    .grupo-regras-relatorio { border-bottom:2px solid var(--borda); padding:8px 0 18px; }
    .grupo-regras-relatorio:last-child { border-bottom:0; }
    @media print {
      .controles-regras { display:none; }
      details.detalhes-regras-consistencia > summary { display:none; }
      details.detalhes-regras-consistencia > *:not(summary) { display:block !important; }
      .grupo-regras-relatorio { break-inside:auto; }
      /* A metodologia já vem depois de uma seção estadual de extensão variável.
         Deixar o navegador escolher a próxima página evita que uma quebra forçada
         acrescente uma folha vazia quando a seção anterior termina no limite da página. */
      #metodologia { break-before:auto; page-break-before:auto; }
    }
    "
  )
}

# Controles de expandir/contrair todos os grupos no HTML interativo.
javascript_relatorio_consistencia <- function() {
  "
  function alternarRegrasConsistencia(aberto) {
    document.querySelectorAll('details.detalhes-regras-consistencia').forEach(function(item) {
      item.open = aberto;
    });
  }
  document.addEventListener('DOMContentLoaded', function() {
    var parametros = new URLSearchParams(window.location.search);
    if (parametros.get('expandir_regras') === '1') alternarRegrasConsistencia(true);
  });
  "
}

# Compõe o HTML completo do relatório.
montar_documento_relatorio_consistencia <- function(config_relatorio, dados) {
  configuracao_documento <- configuracao_de_relatorio("consistencia")
  controles <- configuracao_documento$controles
  dados <- preparar_rotulos_relatorio_consistencia(dados)
  regioes <- config_relatorio$regioes_selecionadas
  ufs <- config_relatorio$ufs_selecionadas
  municipios <- config_relatorio$municipios_selecionados
  nomes_regioes <- dados$referencia_ufs$nome_regiao[
    match(regioes, dados$referencia_ufs$codigo_regiao)
  ]
  nomes_ufs <- dados$referencia_ufs$sigla_uf[
    match(ufs, dados$referencia_ufs$codigo_uf)
  ]
  linhas_municipios <- match(municipios, dados$municipios$codigo_sinan_6)
  nomes_municipios <- paste0(
    dados$municipios$nome_municipio[linhas_municipios], " (",
    dados$municipios$sigla_uf[linhas_municipios], ")"
  )
  proximo_numero <- 0L
  numeros <- function(quantidade) {
    if (!quantidade) return(integer())
    saida <- seq.int(proximo_numero + 1L, length.out = quantidade)
    proximo_numero <<- proximo_numero + quantidade
    saida
  }
  numero_brasil <- if (isTRUE(config_relatorio$incluir_brasil)) numeros(1L) else integer()
  numeros_regioes <- numeros(length(regioes))
  numeros_ufs <- numeros(length(ufs))
  numeros_municipios <- numeros(length(municipios))
  numero_metodologia <- numeros(1L)
  navegacao <- htmltools::tags$nav(
    if (isTRUE(config_relatorio$incluir_brasil)) {
      htmltools::tags$a(href = "#brasil", "Brasil")
    },
    lapply(seq_along(regioes), function(i) htmltools::tags$a(
      href = paste0("#regiao-", regioes[[i]]), nomes_regioes[[i]]
    )),
    lapply(seq_along(ufs), function(i) htmltools::tags$a(
      href = paste0("#uf-", tolower(nomes_ufs[[i]])), nomes_ufs[[i]]
    )),
    lapply(seq_along(municipios), function(i) htmltools::tags$a(
      href = paste0("#municipio-", municipios[[i]]), nomes_municipios[[i]]
    )),
    htmltools::tags$a(href = "#metodologia", "Metodologia")
  )
  config_json <- jsonlite::toJSON(
    config_relatorio, auto_unbox = TRUE, null = "null",
    POSIXt = "ISO8601", pretty = TRUE
  )
  htmltools::tags$html(
    lang = "pt-BR",
    htmltools::tags$head(
      htmltools::tags$meta(charset = "UTF-8"),
      htmltools::tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
      htmltools::tags$title(configuracao_documento$titulo_documento),
      htmltools::tags$style(htmltools::HTML(css_relatorio_consistencia())),
      htmltools::tags$script(type = "application/json", id = "config-relatorio",
                             htmltools::HTML(config_json)),
      htmltools::tags$script(htmltools::HTML(javascript_matrizes_prioridade())),
      htmltools::tags$script(htmltools::HTML(javascript_mapas_interativos())),
      htmltools::tags$script(htmltools::HTML(javascript_paretos_interativos())),
      htmltools::tags$script(htmltools::HTML(javascript_relatorio_consistencia()))
    ),
    htmltools::tags$body(
      cabecalho_identidade_relatorio(
        configuracao_documento$titulo_documento,
        config_relatorio$periodo,
        config_relatorio$gerado_em
      ),
      navegacao,
      htmltools::tags$div(
        class = "controles-regras",
        htmltools::tags$button(
          type = "button", onclick = "alternarRegrasConsistencia(false)",
          controles$apenas_grupos
        ),
        htmltools::tags$button(
          type = "button", onclick = "alternarRegrasConsistencia(true)",
          controles$expandir_regras
        )
      ),
      htmltools::tags$main(
        if (isTRUE(config_relatorio$incluir_brasil)) {
          secao_brasil_consistencia_relatorio(dados, numero_brasil[[1]])
        },
        lapply(seq_along(regioes), function(i) {
          secao_regiao_consistencia_relatorio(
            dados, regioes[[i]], numeros_regioes[[i]]
          )
        }),
        lapply(seq_along(ufs), function(i) {
          secao_uf_consistencia_relatorio(
            dados, ufs[[i]], numeros_ufs[[i]],
            config_relatorio$escala_matriz_prioridade
          )
        }),
        lapply(seq_along(municipios), function(i) {
          secao_municipio_consistencia_relatorio(
            dados, municipios[[i]], numeros_municipios[[i]],
            config_relatorio$escala_matriz_prioridade
          )
        }),
        secao_metodologia_consistencia_relatorio(dados, numero_metodologia[[1]])
      ),
      bloco_autoria_relatorio()
    )
  )
}

# Orquestra pré-agregação, montagem, gravação e eventual conversão para PDF.
gerar_relatorio_consistencia <- function(base_padronizada, resultado_consistencia,
                                          incluir_brasil = FALSE,
                                          regioes = character(), ufs = character(),
                                          municipios = character(),
                                          diretorio_referencias = file.path("data", "reference"),
                                          diretorio_saida = tempfile("relatorio-consistencia-"),
                                          gerar_pdf = TRUE, gerado_em = Sys.time(),
                                          escala_matriz = configuracao_priorizacao()$escala_padrao) {
  referencias <- carregar_referencias_territoriais(diretorio_referencias)
  config <- criar_config_relatorio(
    base_padronizada = base_padronizada,
    referencias = referencias,
    incluir_brasil = incluir_brasil,
    regioes = regioes,
    ufs = ufs,
    municipios = municipios,
    escala_matriz = escala_matriz,
    gerado_em = gerado_em
  )
  dados <- pre_agregar_relatorio_consistencia(
    base_padronizada, resultado_consistencia, referencias,
    ufs_detalhadas = config$ufs_selecionadas,
    municipios_detalhados = config$municipios_selecionados
  )
  rm(referencias)
  liberar_memoria_ram()
  dir.create(diretorio_saida, recursive = TRUE, showWarnings = FALSE)
  periodo_arquivo <- gsub("[^0-9A-Za-z_-]+", "_", config$periodo)
  base_nome <- paste0(
    configuracao_de_relatorio("consistencia")$nome_arquivo, "_", periodo_arquivo
  )
  caminho_html <- file.path(diretorio_saida, paste0(base_nome, ".html"))
  caminho_pdf <- file.path(diretorio_saida, paste0(base_nome, ".pdf"))
  concluido <- FALSE
  on.exit({
    if (!concluido) remover_arquivos_temporarios_relatorio(list(
      caminho_html = caminho_html, caminho_pdf = caminho_pdf
    ))
  }, add = TRUE)
  documento <- montar_documento_relatorio_consistencia(config, dados)
  rm(dados)
  liberar_memoria_ram()
  htmltools::save_html(documento, caminho_html, background = "white")
  rm(documento)
  liberar_memoria_ram(completa = TRUE)
  if (isTRUE(gerar_pdf)) {
    converter_html_para_pdf(
      caminho_html, caminho_pdf, expandir_detalhes = TRUE
    )
  }
  concluido <- TRUE
  list(
    config_relatorio = config,
    caminho_html = normalizePath(caminho_html, winslash = "/", mustWork = TRUE),
    caminho_pdf = if (file.exists(caminho_pdf)) {
      normalizePath(caminho_pdf, winslash = "/", mustWork = TRUE)
    } else NULL
  )
}
