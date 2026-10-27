# Visualizações específicas de consistência -----------------------------------
#
# Os gráficos territoriais delegam às funções de validade para manter identidade
# visual e comportamento iguais. As funções finais representam grupos e regras.

# Ranking territorial de registros inconsistentes.
grafico_ranking_consistencia <- function(
    tabela, coluna_nome, titulo,
    limite = configuracao_geral_relatorios()$limites$ranking) {
  if (!nrow(tabela)) return(grafico_sem_dados(titulo))
  dados <- calcular_pareto(tabela, coluna_nome, limite)
  dados$rotulo <- dados[[coluna_nome]]
  dados$rotulo[is.na(dados$rotulo) | !nzchar(dados$rotulo)] <- "Código não localizado"
  dados$posicao_eixo <- factor(seq_len(nrow(dados)), levels = rev(seq_len(nrow(dados))))
  ggplot2::ggplot(dados, ggplot2::aes(x = posicao_eixo, y = registros_invalidos)) +
    ggplot2::geom_col(fill = cor_viva_quali("principal"), width = 0.72) +
    ggplot2::coord_flip() +
    ggplot2::scale_x_discrete(labels = function(x) dados$rotulo[as.integer(x)]) +
    ggplot2::labs(title = titulo, x = NULL, y = "Registros inconsistentes") +
    tema_viva_quali()
}

# Pareto territorial de inconsistências.
grafico_pareto_consistencia <- function(
    tabela, coluna_nome, titulo,
    limite = configuracao_geral_relatorios()$limites$pareto) {
  grafico <- grafico_pareto_validade(tabela, coluna_nome, titulo, limite)
  grafico + ggplot2::labs(y = "Registros inconsistentes")
}

# Matriz territorial de volume e percentual.
grafico_matriz_prioridade_consistencia <- function(
    tabela, coluna_nome, titulo,
    escala = configuracao_priorizacao()$escala_padrao) {
  grafico_matriz_prioridade(
    tabela, coluna_nome, titulo, modulo = "consistencia", escala = escala
  )
}

# Mapa de UFs usando os indicadores de consistência.
mapa_ufs_consistencia <- function(geografia, tabela_ufs, indicador,
                                  titulo, codigos_regiao = NULL) {
  grafico <- mapa_ufs_validade(geografia, tabela_ufs, indicador, titulo, codigos_regiao)
  legenda <- if (identical(indicador, "percentual_invalidos")) {
    "Inconsistentes (%)"
  } else {
    "Registros inconsistentes"
  }
  grafico + ggplot2::labs(fill = legenda)
}

# Compara os oito grupos temáticos no nível territorial em análise.
grafico_grupos_consistencia_relatorio <- function(tabela, titulo) {
  if (!nrow(tabela)) return(grafico_sem_dados(titulo))
  dados <- tabela[order(tabela$registros_invalidos, tabela$grupo), , drop = FALSE]
  dados$rotulo <- factor(dados$grupo, levels = dados$grupo)
  ggplot2::ggplot(dados, ggplot2::aes(x = rotulo, y = registros_invalidos)) +
    ggplot2::geom_col(fill = cor_viva_quali("principal"), width = 0.72) +
    ggplot2::geom_text(
      ggplot2::aes(label = registros_invalidos), hjust = -0.18, size = 3.2,
      color = cor_viva_quali("texto")
    ) +
    ggplot2::coord_flip() +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, .15))) +
    ggplot2::labs(title = titulo, x = NULL, y = "Registros inconsistentes") +
    tema_viva_quali()
}

# Detalha as regras pertencentes a um grupo selecionado.
grafico_regras_grupo_consistencia_relatorio <- function(tabela, grupo, titulo) {
  dados <- tabela[tabela$grupo == grupo, , drop = FALSE]
  if (!nrow(dados)) return(grafico_sem_dados(titulo))
  dados <- dados[order(dados$registros_invalidos, dados$regra), , drop = FALSE]
  dados$rotulo <- factor(dados$regra, levels = dados$regra)
  ggplot2::ggplot(dados, ggplot2::aes(x = rotulo, y = registros_invalidos)) +
    ggplot2::geom_col(fill = cor_viva_quali("principal_clara"), width = 0.72) +
    ggplot2::geom_text(
      ggplot2::aes(label = registros_invalidos), hjust = -0.18, size = 3.2,
      color = cor_viva_quali("texto")
    ) +
    ggplot2::coord_flip() +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, .15))) +
    ggplot2::labs(title = titulo, x = NULL, y = "Registros inconsistentes") +
    tema_viva_quali()
}
