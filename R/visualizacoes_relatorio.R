# Visualizações compartilhadas do relatório -----------------------------------
#
# Todas as funções retornam objetos `ggplot`, permitindo que o gerador escolha
# o tamanho e o formato de saída. A ordenação dos dados é feita antes daqui.

# Tema visual comum, com tipografia e margens adequadas para relatório.
tema_viva_quali <- function() {
  ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        face = "bold", color = cor_viva_quali("principal")
      ),
      plot.subtitle = ggplot2::element_text(
        color = cor_viva_quali("texto_secundario")
      ),
      panel.grid.minor = ggplot2::element_blank(),
      legend.position = "bottom"
    )
}

# Gráfico substituto que mantém a estrutura quando não há dados para exibir.
grafico_sem_dados <- function(titulo) {
  ggplot2::ggplot() +
    ggplot2::annotate(
      "text", x = 0, y = 0, label = "Sem dados para exibir.",
      color = cor_viva_quali("texto_secundario")
    ) +
    ggplot2::labs(title = titulo) +
    ggplot2::xlim(-1, 1) + ggplot2::ylim(-1, 1) + tema_viva_quali() +
    ggplot2::theme(axis.text = ggplot2::element_blank(), axis.title = ggplot2::element_blank(),
                   panel.grid = ggplot2::element_blank())
}

# Barras horizontais para comparar as entidades com mais problemas.
grafico_ranking_validade <- function(
    tabela, coluna_nome, titulo,
    limite = configuracao_geral_relatorios()$limites$ranking) {
  if (!nrow(tabela)) return(grafico_sem_dados(titulo))
  dados <- calcular_pareto(tabela, coluna_nome, limite)
  dados$rotulo <- dados[[coluna_nome]]
  dados$rotulo[is.na(dados$rotulo) | !nzchar(dados$rotulo)] <- "Código não localizado"
  dados$posicao_eixo <- factor(
    seq_len(nrow(dados)), levels = rev(seq_len(nrow(dados)))
  )
  ggplot2::ggplot(dados, ggplot2::aes(x = posicao_eixo, y = registros_invalidos)) +
    ggplot2::geom_col(fill = cor_viva_quali("principal"), width = 0.72) +
    ggplot2::coord_flip() +
    ggplot2::scale_x_discrete(labels = function(x) dados$rotulo[as.integer(x)]) +
    ggplot2::labs(title = titulo, x = NULL, y = "Registros inválidos") +
    tema_viva_quali()
}

# Pareto com barras de ocorrências e linha de percentual acumulado.
# Os nomes ficam na vertical para reduzir sobreposição em listas extensas.
grafico_pareto_validade <- function(
    tabela, coluna_nome, titulo,
    limite = configuracao_geral_relatorios()$limites$pareto) {
  if (!nrow(tabela)) return(grafico_sem_dados(titulo))
  dados <- calcular_pareto(tabela, coluna_nome, limite)
  dados$rotulo <- dados[[coluna_nome]]
  dados$rotulo[is.na(dados$rotulo) | !nzchar(dados$rotulo)] <- "Não localizado"
  dados$posicao_eixo <- factor(seq_len(nrow(dados)), levels = seq_len(nrow(dados)))
  ggplot2::ggplot(dados, ggplot2::aes(x = posicao_eixo)) +
    ggplot2::geom_col(
      ggplot2::aes(y = registros_invalidos), fill = cor_viva_quali("principal")
    ) +
    ggplot2::geom_line(
      ggplot2::aes(y = contribuicao_acumulada / 100 * max(registros_invalidos, 1), group = 1),
      color = cor_viva_quali("pareto_linha"), linewidth = 1
    ) +
    ggplot2::geom_hline(
      yintercept = c(.5, .8) * max(dados$registros_invalidos, 1),
      linetype = "dashed",
      color = c(cor_viva_quali("pareto_50"), cor_viva_quali("pareto_80"))
    ) +
    ggplot2::scale_y_continuous(
      name = "Registros inválidos",
      sec.axis = ggplot2::sec_axis(
        ~ . / max(dados$registros_invalidos, 1) * 100,
        name = "Contribuição acumulada (%)"
      )
    ) +
    ggplot2::scale_x_discrete(
      labels = function(x) dados$rotulo[as.integer(x)],
      expand = ggplot2::expansion(add = c(.6, .6))
    ) +
    ggplot2::labs(title = titulo, x = NULL) +
    tema_viva_quali() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, vjust = .5, size = 7),
      plot.margin = ggplot2::margin(t = 10, r = 10, b = 150, l = 10, unit = "pt")
    )
}

# Traduz a escolha amigável da configuração para uma transformação reversível
# do pacote `scales`. Pseudo-log foi escolhido porque preserva o zero e separa
# melhor valores pequenos sem permitir que grandes extremos comprimam o painel.
transformacao_eixo_prioridade <- function(tipo, sigma = 1) {
  switch(
    tipo,
    pseudo_log = scales::pseudo_log_trans(sigma = sigma, base = 10),
    log1p = scales::trans_new(
      "log1p", transform = log1p, inverse = expm1,
      breaks = scales::breaks_pretty()
    ),
    sqrt = "sqrt",
    linear = "identity",
    stop(sprintf("Transformação de prioridade desconhecida: %s.", tipo), call. = FALSE)
  )
}

# Matriz estática usada no PDF. A tabela pode chegar já classificada pelo bloco
# HTML; chamadas isoladas também são aceitas e passam pela mesma função central.
grafico_matriz_prioridade <- function(
    tabela, coluna_nome, titulo, modulo = c("validade", "consistencia"),
    escala = configuracao_priorizacao()$escala_padrao) {
  modulo <- match.arg(modulo)
  escala <- validar_escala_matriz_prioridade(escala)
  if (!nrow(tabela)) return(grafico_sem_dados(titulo))
  dados <- if ("prioridade_id" %in% names(tabela)) {
    tabela
  } else {
    preparar_tabela_prioridades(tabela, coluna_nome, coluna_nome)
  }
  dados <- dados[dados$registros_invalidos > 0, , drop = FALSE]
  if (!nrow(dados)) return(grafico_sem_dados(titulo))

  config <- configuracao_priorizacao()
  # "Sem problemas" permanece na tabela, porém não possui ponto na matriz;
  # removê-lo desta legenda evita uma categoria visual sem representação.
  ids <- setdiff(names(config$categorias), "sem_problemas")
  categorias_visuais <- config$categorias[ids]
  nomes <- vapply(categorias_visuais, `[[`, character(1), "nome")
  cores <- vapply(categorias_visuais, `[[`, character(1), "cor")
  dados$prioridade_fator <- factor(dados$prioridade_id, levels = ids, labels = nomes)
  dados$alpha_prioridade <- ifelse(
    dados$prioridade_id == "monitoramento",
    config$pontos$transparencia_monitoramento,
    config$pontos$transparencia_padrao
  )
  criticos <- dados[dados$prioridade_id == "prioridade_1", , drop = FALSE]
  criticos <- utils::head(
    criticos[order(-criticos$registros_invalidos), , drop = FALSE],
    config$rotulos_prioridade_1
  )
  rotulo_y <- if (identical(modulo, "consistencia")) {
    "Registros inconsistentes (%)"
  } else "Registros inválidos (%)"
  rotulo_tamanho <- if (identical(modulo, "consistencia")) {
    "Inconsistentes"
  } else "Inválidos"
  # O limite acompanha os dados de cada território, mas nunca ultrapassa 100%.
  # Assim RR não fica comprimido e nenhum eixo percentual exibe 120% ou 125%.
  limite_y_exibicao <- min(
    100,
    1.08 * max(
      c(dados$percentual_invalidos, dados$limiar_relativo_p95[[1]], 1),
      na.rm = TRUE
    )
  )
  transformacao_x <- if (identical(escala, "original")) {
    "identity"
  } else {
    transformacao_eixo_prioridade(config$transformacao_x, config$sigma_x)
  }
  transformacao_y <- if (identical(escala, "original")) {
    "identity"
  } else {
    transformacao_eixo_prioridade(config$transformacao_y, config$sigma_y)
  }
  # Posiciona os rótulos críticos para dentro do painel. A decisão considera a
  # posição já transformada, portanto funciona tanto na escala original quanto
  # na pseudo-logarítmica sem alterar os valores exibidos.
  transformar_posicao <- function(valores, transformacao) {
    if (is.character(transformacao) && identical(transformacao, "identity")) {
      return(valores)
    }
    transformacao$transform(valores)
  }
  x_visual <- transformar_posicao(dados$registros_invalidos, transformacao_x)
  y_visual <- transformar_posicao(dados$percentual_invalidos, transformacao_y)
  limite_x_visual <- max(x_visual, na.rm = TRUE)
  limite_y_visual <- max(y_visual, na.rm = TRUE)
  criticos$rotulo_exibicao <- ifelse(
    nchar(criticos$nome_prioridade) > 34,
    paste0(substr(criticos$nome_prioridade, 1, 31), "…"),
    criticos$nome_prioridade
  )
  criticos_x <- transformar_posicao(criticos$registros_invalidos, transformacao_x)
  criticos_y <- transformar_posicao(criticos$percentual_invalidos, transformacao_y)
  criticos$rotulo_hjust <- ifelse(
    criticos_x >= 0.70 * limite_x_visual, 1.08, -0.08
  )
  criticos$rotulo_vjust <- ifelse(
    criticos_y >= 0.88 * limite_y_visual, 1.25, -0.85
  )

  ggplot2::ggplot(dados, ggplot2::aes(
    x = registros_invalidos, y = percentual_invalidos,
    size = registros_invalidos, color = prioridade_fator, alpha = alpha_prioridade
  )) +
    # Forma circular única: apenas a cor comunica a categoria de prioridade.
    # A área continua proporcional ao número absoluto de registros problemáticos.
    ggplot2::geom_point(shape = 16) +
    ggplot2::geom_text(
      data = criticos,
      ggplot2::aes(
        x = registros_invalidos, y = percentual_invalidos,
        label = rotulo_exibicao, hjust = rotulo_hjust, vjust = rotulo_vjust
      ),
      inherit.aes = FALSE,
      color = cor_viva_quali("texto"), size = 3,
      check_overlap = TRUE
    ) +
    ggplot2::scale_x_continuous(
      trans = transformacao_x,
      breaks = if (identical(escala, "original")) {
        scales::breaks_pretty(n = 6)
      } else ggplot2::waiver(),
      labels = scales::label_number(big.mark = ".", decimal.mark = ",")
    ) +
    ggplot2::scale_y_continuous(
      trans = transformacao_y,
      limits = c(0, limite_y_exibicao), oob = scales::squish,
      breaks = function(limites) {
        candidatos <- scales::breaks_pretty(n = 6)(limites)
        candidatos[candidatos >= 0 & candidatos <= 100]
      },
      labels = scales::label_number(suffix = "%", decimal.mark = ",")
    ) +
    ggplot2::scale_color_manual(values = stats::setNames(cores, nomes), drop = FALSE) +
    ggplot2::scale_alpha_identity() +
    ggplot2::scale_size_area(
      max_size = config$pontos$tamanho_maximo,
      breaks = scales::breaks_pretty(n = 4)
    ) +
    ggplot2::labs(
      title = titulo,
      subtitle = if (identical(escala, "original")) {
        "Eixos na escala original"
      } else {
        "Eixos na escala transformada; valores originais preservados"
      },
      x = "Registros problemáticos", y = rotulo_y,
      color = "Prioridade", size = rotulo_tamanho
    ) +
    tema_viva_quali() +
    # A legenda de tamanho ocupa uma linha própria e as categorias podem usar
    # duas linhas. Isso evita cortes laterais quando o HTML é impresso em PDF.
    ggplot2::guides(
      alpha = "none",
      size = ggplot2::guide_legend(order = 1, nrow = 1),
      color = ggplot2::guide_legend(
        order = 2, nrow = 2, byrow = TRUE,
        override.aes = list(
          size = 4, alpha = 1, shape = 16, color = unname(cores)
        )
      )
    ) +
    ggplot2::theme(
      legend.box = "vertical",
      legend.justification = "center",
      legend.spacing.y = grid::unit(.08, "cm"),
      plot.margin = ggplot2::margin(8, 12, 8, 12)
    )
}

# Mapa coroplético das UFs; exige a malha `sf` carregada nas referências.
mapa_ufs_validade <- function(geografia, tabela_ufs, indicador,
                              titulo, codigos_regiao = NULL) {
  if (is.null(geografia) || !inherits(geografia, "sf")) {
    stop("A malha geográfica de UFs não está disponível.", call. = FALSE)
  }
  dados <- geografia
  indice <- match(dados$codigo_uf, tabela_ufs$codigo_uf)
  dados$valor <- tabela_ufs[[indicador]][indice]
  dados$sigla_uf <- tabela_ufs$sigla_uf[indice]
  if (!is.null(codigos_regiao)) {
    codigos_ufs <- tabela_ufs$codigo_uf[tabela_ufs$codigo_regiao %in% codigos_regiao]
    dados <- dados[dados$codigo_uf %in% codigos_ufs, , drop = FALSE]
  }
  dados <- sf::st_transform(dados, 5880)
  legenda <- if (identical(indicador, "percentual_invalidos")) {
    "Inválidos (%)"
  } else {
    "Registros inválidos"
  }
  ggplot2::ggplot(dados) +
    ggplot2::geom_sf(ggplot2::aes(fill = valor), color = "white", linewidth = .25) +
    ggplot2::geom_sf_text(
      ggplot2::aes(label = sigla_uf), size = 2.5,
      color = cor_viva_quali("texto"), na.rm = TRUE
    ) +
    ggplot2::scale_fill_gradient(
      low = cor_viva_quali("mapa_baixo"), high = cor_viva_quali("mapa_alto"),
      na.value = cor_viva_quali("mapa_ausente"),
      # Mantém a escala no lado direito também na versão estática usada no PDF.
      guide = ggplot2::guide_colorbar(
        direction = "vertical",
        barheight = grid::unit(4.5, "cm"),
        barwidth = grid::unit(.45, "cm")
      )
    ) +
    ggplot2::labs(title = titulo, fill = legenda) +
    ggplot2::coord_sf(datum = NA) +
    tema_viva_quali() +
    ggplot2::theme(
      axis.text = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      legend.position = "right"
    )
}
