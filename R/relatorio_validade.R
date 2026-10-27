# Geração do relatório de validade --------------------------------------------
#
# Converte as tabelas pré-agregadas em um documento HTML autossuficiente e,
# opcionalmente, em PDF. As funções de seção mantêm separadas apresentação,
# indicadores e dados, facilitando mudanças pontuais no relatório.

# Formatação brasileira de totais inteiros.
formatar_inteiro_relatorio <- function(x) {
  saida <- rep(NA_character_, length(x))
  validos <- !is.na(x)
  saida[validos] <- format(as.integer(x[validos]), big.mark = ".", decimal.mark = ",",
                           scientific = FALSE)
  saida
}

# Formatação brasileira de percentuais, preservando ausências.
formatar_percentual_relatorio <- function(x) {
  saida <- rep(NA_character_, length(x))
  validos <- !is.na(x)
  saida[validos] <- paste0(
    format(round(as.numeric(x[validos]), 2), nsmall = 2, decimal.mark = ","), "%"
  )
  saida
}

# Incorpora a identidade visual no próprio HTML para que o relatório permaneça
# autossuficiente quando for enviado, arquivado ou convertido em PDF.
logo_relatorio_data_uri <- function(tipo = c("logo", "simbolo")) {
  tipo <- match.arg(tipo)
  identidade <- configuracao_viva_quali$aplicativo$identidade
  chave <- if (identical(tipo, "logo")) "logo_arquivo" else "simbolo_arquivo"
  caminho_relativo <- identidade[[chave]]
  raiz <- if (exists("diretorio_projeto_aplicativo", inherits = TRUE)) {
    get("diretorio_projeto_aplicativo", inherits = TRUE)
  } else {
    getwd()
  }
  diretorios <- unique(c(raiz, getwd()))
  atual <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
  for (i in seq_len(8L)) {
    diretorios <- unique(c(diretorios, atual))
    pai <- dirname(atual)
    if (identical(pai, atual)) break
    atual <- pai
  }
  candidatos <- unique(c(file.path(diretorios, caminho_relativo), caminho_relativo))
  candidatos <- candidatos[file.exists(candidatos)]
  if (!length(candidatos)) return(NULL)
  base64enc::dataURI(file = candidatos[[1]], mime = "image/png")
}

# Cabeçalho comum aos relatórios de validade e consistência.
cabecalho_identidade_relatorio <- function(titulo, periodo, gerado_em) {
  logo <- logo_relatorio_data_uri("logo")
  htmltools::tags$header(
    class = "cabecalho-relatorio",
    htmltools::tags$div(
      class = "marca-relatorio",
      if (!is.null(logo)) htmltools::tags$img(
        class = "logo-relatorio", src = logo,
        alt = paste("Logomarca", configuracao_viva_quali$aplicativo$nome)
      )
    ),
    htmltools::tags$div(
      class = "identificacao-relatorio",
      htmltools::tags$span(class = "rotulo-relatorio", "Relatório analítico"),
      htmltools::tags$h1(titulo),
      htmltools::tags$div(
        class = "metadados-relatorio",
        htmltools::tags$span(htmltools::tags$strong("Período: "), periodo),
        htmltools::tags$span(
          htmltools::tags$strong("Gerado em: "),
          format(gerado_em, "%d/%m/%Y %H:%M:%S %Z")
        )
      )
    )
  )
}

# Registra autoria humana e a ferramenta de apoio sem atribuir ao Codex a
# responsabilidade metodológica ou institucional pelo aplicativo.
bloco_autoria_relatorio <- function() {
  autoria <- configuracao_autoria()
  simbolo <- logo_relatorio_data_uri("simbolo")
  htmltools::tags$footer(
    class = "rodape-relatorio",
    if (!is.null(simbolo)) htmltools::tags$img(
      class = "simbolo-rodape-relatorio", src = simbolo,
      alt = "", `aria-hidden` = "true"
    ),
    htmltools::tags$div(
      class = "texto-rodape-relatorio",
      htmltools::tags$p(
        htmltools::tags$strong("Autoria: "), valor_autoria("autores"),
        " — ", valor_autoria("instituicao")
      ),
      htmltools::tags$p(
        paste("Contato:", valor_autoria("contato")),
        " · ", paste("Versão", valor_autoria("versao")),
        " · ", valor_autoria("ano")
      ),
      htmltools::tags$p(autoria$declaracao_apoio),
      htmltools::tags$p(class = "observacao", autoria$declaracao_responsabilidade)
    )
  )
}

# Escolhe um rótulo legível e usa o código como alternativa quando falta o nome.
rotular_entidade <- function(nome, codigo, prefixo = "Código") {
  nome <- as.character(nome)
  codigo <- as.character(codigo)
  vazio <- is.na(nome) | !nzchar(nome)
  nome[vazio] <- ifelse(
    is.na(codigo[vazio]) | !nzchar(codigo[vazio]),
    paste(prefixo, "ausente"), paste(prefixo, codigo[vazio], "não localizado")
  )
  nome
}

# Cria rótulos finais de região, UF, município e estabelecimento.
preparar_rotulos_relatorio <- function(dados) {
  dados$ufs$nome_exibicao <- rotular_entidade(
    ifelse(is.na(dados$ufs$sigla_uf), dados$ufs$nome_uf,
           paste0(dados$ufs$nome_uf, " (", dados$ufs$sigla_uf, ")")),
    dados$ufs$codigo_uf, "UF"
  )
  dados$municipios$nome_exibicao <- rotular_entidade(
    dados$municipios$nome_municipio, dados$municipios$codigo_sinan_6, "Município"
  )
  dados$cnes$nome_exibicao <- if (isTRUE(dados$possui_referencia_cnes)) {
    nome <- as.character(dados$cnes$nome_unidade)
    sem_nome <- is.na(nome) | !nzchar(trimws(nome))
    nome[sem_nome] <- ifelse(
      is.na(dados$cnes$id_unidade[sem_nome]) | !nzchar(dados$cnes$id_unidade[sem_nome]),
      "CNES ausente",
      paste("CNES", dados$cnes$id_unidade[sem_nome], "— nome fantasia não disponível")
    )
    nome
  } else {
    ifelse(is.na(dados$cnes$id_unidade), "CNES ausente", paste("CNES", dados$cnes$id_unidade))
  }
  if (!is.null(dados$cnes_municipio)) {
    dados$cnes_municipio$nome_exibicao <- if (isTRUE(dados$possui_referencia_cnes)) {
      nome <- as.character(dados$cnes_municipio$nome_unidade)
      sem_nome <- is.na(nome) | !nzchar(trimws(nome))
      nome[sem_nome] <- ifelse(
        is.na(dados$cnes_municipio$id_unidade[sem_nome]) |
          !nzchar(dados$cnes_municipio$id_unidade[sem_nome]),
        "CNES ausente",
        paste(
          "CNES", dados$cnes_municipio$id_unidade[sem_nome],
          "— nome fantasia não disponível"
        )
      )
      nome
    } else {
      ifelse(
        is.na(dados$cnes_municipio$id_unidade), "CNES ausente",
        paste("CNES", dados$cnes_municipio$id_unidade)
      )
    }
  }
  dados
}

# Acrescenta numeração contínua aos títulos configuráveis. Subseções usam o
# mesmo número principal do território e um índice local coerente.
titulo_numerado_relatorio <- function(numero, titulo, subindice = NULL) {
  prefixo <- if (is.null(subindice)) as.character(numero) else {
    paste(numero, subindice, sep = ".")
  }
  paste0(prefixo, ". ", titulo)
}

# Constrói uma tabela HTML com seleção de colunas, rótulos e limite de linhas.
tabela_html_relatorio <- function(tabela, colunas, rotulos, limite = Inf,
                                  classes_percentuais = character()) {
  if (!nrow(tabela)) return(htmltools::tags$p(class = "sem-dados", "Sem dados para exibir."))
  tabela <- tabela[, colunas, drop = FALSE]
  if (is.finite(limite)) tabela <- utils::head(tabela, limite)
  for (coluna in intersect(c("notificacoes", "registros_invalidos", "ocorrencias_invalidas",
                              "unidades_necessarias", "ocorrencias_regra_dominante",
                              "ocorrencias_dominantes"), names(tabela))) {
    tabela[[coluna]] <- formatar_inteiro_relatorio(tabela[[coluna]])
  }
  for (coluna in intersect(c("percentual_invalidos", "contribuicao_invalidos",
                              "contribuicao_pareto", "contribuicao_acumulada",
                              "percentual_alcancado"), names(tabela))) {
    tabela[[coluna]] <- formatar_percentual_relatorio(tabela[[coluna]])
  }
  cabecalho <- htmltools::tags$tr(lapply(rotulos, htmltools::tags$th))
  linhas <- lapply(seq_len(nrow(tabela)), function(i) {
    htmltools::tags$tr(lapply(tabela[i, , drop = FALSE], function(x) {
      valor <- if (is.na(x) || !nzchar(as.character(x))) "—" else as.character(x)
      htmltools::tags$td(valor)
    }))
  })
  htmltools::tags$div(
    class = "tabela-rolagem",
    htmltools::tags$table(
      class = "tabela-relatorio",
      htmltools::tags$thead(cabecalho), htmltools::tags$tbody(linhas)
    )
  )
}

# Converte um gráfico ggplot em imagem incorporada diretamente no HTML. Os
# valores padrão vêm do arquivo central de configuração, mas cada chamada pode
# fornecer dimensões próprias quando necessário.
imagem_grafico_relatorio <- function(
    grafico, alt,
    largura = dimensoes_figura_relatorio("padrao")$largura,
    altura = dimensoes_figura_relatorio("padrao")$altura,
    dpi = dimensoes_figura_relatorio("padrao")$dpi,
    classes = "grafico-relatorio") {
  caminho <- tempfile(fileext = ".png")
  on.exit(unlink(caminho), add = TRUE)
  ggplot2::ggsave(caminho, grafico, width = largura, height = altura,
                  units = "in", dpi = dpi, bg = "white")
  htmltools::tags$img(
    class = classes, alt = alt,
    src = base64enc::dataURI(file = caminho, mime = "image/png")
  )
}

# Monta os cartões com os principais totais da seção.
cards_resumo_relatorio <- function(resumo) {
  valores <- list(
    "Notificações" = formatar_inteiro_relatorio(resumo$notificacoes),
    "Registros inválidos" = formatar_inteiro_relatorio(resumo$registros_invalidos),
    "Percentual inválido" = formatar_percentual_relatorio(resumo$percentual_invalidos),
    "Ocorrências de problemas" = formatar_inteiro_relatorio(resumo$ocorrencias_invalidas)
  )
  htmltools::tags$div(
    class = "cards-resumo",
    lapply(names(valores), function(nome) htmltools::tags$div(
      class = "card-resumo", htmltools::tags$span(nome), htmltools::tags$strong(valores[[nome]])
    ))
  )
}

# Monta a tabela analítica das regras executadas e não executáveis.
bloco_regras_relatorio <- function(tabela, regras_metodologicas) {
  numeros <- gsub("[^0-9]", "", as.character(regras_metodologicas$regra))
  consolidada <- data.frame(
    regra = paste("Regra", numeros),
    descricao = regras_metodologicas$descricao,
    status = regras_metodologicas$status,
    detalhe = regras_metodologicas$detalhe,
    stringsAsFactors = FALSE
  )
  indice <- match(consolidada$regra, tabela$regra)
  consolidada$registros_invalidos <- tabela$registros_invalidos[indice]
  consolidada$percentual_invalidos <- tabela$percentual_invalidos[indice]
  tabela_html_relatorio(
    consolidada,
    c("regra", "descricao", "status", "detalhe", "registros_invalidos", "percentual_invalidos"),
    c("Regra", "Descrição", "Status", "Detalhe", "Registros sinalizados", "% das notificações")
  )
}

# Calcula uma transformação simples das coordenadas projetadas para o sistema
# de coordenadas do SVG. Essa etapa altera somente a forma de desenhar o mapa
# no HTML; os indicadores continuam vindo das tabelas já agregadas.
parametros_svg_mapa_relatorio <- function(bbox, largura = 720, altura = 520,
                                          margem = 18) {
  amplitude_x <- max(as.numeric(bbox[["xmax"]] - bbox[["xmin"]]), 1)
  amplitude_y <- max(as.numeric(bbox[["ymax"]] - bbox[["ymin"]]), 1)
  escala <- min(
    (largura - 2 * margem) / amplitude_x,
    (altura - 2 * margem) / amplitude_y
  )
  largura_desenho <- amplitude_x * escala
  altura_desenho <- amplitude_y * escala
  list(
    largura = largura,
    altura = altura,
    xmin = as.numeric(bbox[["xmin"]]),
    ymax = as.numeric(bbox[["ymax"]]),
    escala = escala,
    deslocamento_x = (largura - largura_desenho) / 2,
    deslocamento_y = (altura - altura_desenho) / 2
  )
}

# Converte pares X/Y para a orientação usada pelo SVG, cujo eixo vertical
# cresce de cima para baixo.
coordenadas_svg_mapa_relatorio <- function(coordenadas, parametros) {
  coordenadas <- as.matrix(coordenadas)
  cbind(
    x = parametros$deslocamento_x +
      (coordenadas[, 1] - parametros$xmin) * parametros$escala,
    y = parametros$deslocamento_y +
      (parametros$ymax - coordenadas[, 2]) * parametros$escala
  )
}

# Transforma POLYGON e MULTIPOLYGON em um único atributo `d` do SVG. Os anéis
# internos são preservados por `fill-rule=evenodd`, portanto ilhas e vazios da
# malha mantêm a geometria original.
caminho_svg_geometria_relatorio <- function(geometria, parametros) {
  poligonos <- if (inherits(geometria, "POLYGON")) {
    list(geometria)
  } else if (inherits(geometria, "MULTIPOLYGON")) {
    geometria
  } else {
    stop("A malha das UFs contém uma geometria não suportada pelo mapa HTML.",
         call. = FALSE)
  }
  partes <- unlist(lapply(poligonos, function(poligono) {
    lapply(poligono, function(anel) {
      pontos <- coordenadas_svg_mapa_relatorio(anel, parametros)
      pares <- paste0(
        formatC(pontos[, "x"], format = "f", digits = 2), ",",
        formatC(pontos[, "y"], format = "f", digits = 2)
      )
      paste0("M ", paste(pares, collapse = " L "), " Z")
    })
  }), use.names = FALSE)
  paste(partes, collapse = " ")
}

# Aplica aos valores do indicador a mesma paleta baixo/alto usada no mapa
# estático. A cor ausente é usada somente quando não há indicador para a UF.
cores_mapa_interativo_relatorio <- function(valores) {
  cores <- configuracao_geral_relatorios()$cores
  resultado <- rep(cores$mapa_ausente, length(valores))
  validos <- is.finite(valores)
  if (!any(validos)) return(resultado)
  intervalo <- range(valores[validos], na.rm = TRUE)
  posicao <- if (diff(intervalo) == 0) {
    rep(.5, sum(validos))
  } else {
    (valores[validos] - intervalo[[1]]) / diff(intervalo)
  }
  rgb <- grDevices::colorRamp(c(cores$mapa_baixo, cores$mapa_alto))(posicao)
  resultado[validos] <- grDevices::rgb(
    rgb[, 1], rgb[, 2], rgb[, 3], maxColorValue = 255
  )
  resultado
}

# Reúne geometria e indicadores já calculados para o mapa HTML. O filtro
# regional replica exatamente o usado pelo mapa estático e não refaz nenhuma
# regra de validade ou consistência.
preparar_dados_mapa_interativo_relatorio <- function(
    geografia, tabela_ufs, codigo_regiao = NULL) {
  if (is.null(geografia) || !inherits(geografia, "sf")) {
    stop("A malha geográfica de UFs não está disponível.", call. = FALSE)
  }
  dados <- geografia
  indice <- match(dados$codigo_uf, tabela_ufs$codigo_uf)
  dados$nome_uf <- tabela_ufs$nome_uf[indice]
  dados$sigla_uf <- tabela_ufs$sigla_uf[indice]
  dados$registros_invalidos <- tabela_ufs$registros_invalidos[indice]
  dados$percentual_invalidos <- tabela_ufs$percentual_invalidos[indice]
  if (!is.null(codigo_regiao)) {
    codigos_ufs <- tabela_ufs$codigo_uf[tabela_ufs$codigo_regiao %in% codigo_regiao]
    dados <- dados[dados$codigo_uf %in% codigos_ufs, , drop = FALSE]
  }
  dados <- sf::st_transform(dados, 5880)
  parametros <- parametros_svg_mapa_relatorio(sf::st_bbox(dados))
  geometrias <- sf::st_geometry(dados)
  dados$caminho_svg <- vapply(
    geometrias, caminho_svg_geometria_relatorio, character(1),
    parametros = parametros
  )
  # `st_point_on_surface` mantém os rótulos dentro de cada UF, inclusive em
  # geometrias côncavas nas quais o centroide poderia ficar fora do polígono.
  pontos_rotulo <- suppressWarnings(sf::st_point_on_surface(geometrias))
  coordenadas_rotulo <- sf::st_coordinates(pontos_rotulo)
  posicoes_rotulo <- coordenadas_svg_mapa_relatorio(
    coordenadas_rotulo[, c("X", "Y"), drop = FALSE], parametros
  )
  dados$x_rotulo_svg <- posicoes_rotulo[, "x"]
  dados$y_rotulo_svg <- posicoes_rotulo[, "y"]
  attr(dados, "parametros_svg") <- parametros
  dados
}

# Monta uma versão SVG interativa de um mapa. O preenchimento representa o
# indicador escolhido, mas a caixa de detalhes sempre mostra conjuntamente o
# número e o percentual de registros problemáticos.
mapa_interativo_ufs_relatorio <- function(dados_mapa, indicador, titulo,
                                          modulo = c("validade", "consistencia")) {
  modulo <- match.arg(modulo)
  parametros <- attr(dados_mapa, "parametros_svg")
  valores <- as.numeric(dados_mapa[[indicador]])
  preenchimentos <- cores_mapa_interativo_relatorio(valores)
  nome_absoluto <- if (identical(modulo, "consistencia")) {
    "Registros inconsistentes"
  } else {
    "Registros inválidos"
  }
  nome_percentual <- if (identical(modulo, "consistencia")) {
    "Percentual inconsistente"
  } else {
    "Percentual inválido"
  }
  titulo_legenda <- if (identical(indicador, "percentual_invalidos")) {
    paste0(nome_percentual, " (%)")
  } else {
    nome_absoluto
  }
  validos <- valores[is.finite(valores)]
  limites <- if (length(validos)) range(validos) else c(NA_real_, NA_real_)
  formatar_limite <- if (identical(indicador, "percentual_invalidos")) {
    formatar_percentual_relatorio
  } else {
    formatar_inteiro_relatorio
  }
  caminhos <- lapply(seq_len(nrow(dados_mapa)), function(i) {
    nome <- dados_mapa$nome_uf[[i]]
    sigla <- dados_mapa$sigla_uf[[i]]
    if (is.na(nome) || !nzchar(nome)) nome <- paste("UF", dados_mapa$codigo_uf[[i]])
    if (is.na(sigla)) sigla <- ""
    absoluto <- dados_mapa$registros_invalidos[[i]]
    percentual <- dados_mapa$percentual_invalidos[[i]]
    htmltools::tags$path(
      class = "estado-mapa-interativo",
      d = dados_mapa$caminho_svg[[i]], fill = preenchimentos[[i]],
      `fill-rule` = "evenodd", tabindex = "0",
      `data-codigo-uf` = as.character(dados_mapa$codigo_uf[[i]]),
      `data-nome-uf` = nome, `data-sigla-uf` = sigla,
      `data-absoluto` = if (is.finite(absoluto)) as.character(absoluto) else "",
      `data-percentual` = if (is.finite(percentual)) as.character(percentual) else "",
      `data-rotulo-absoluto` = nome_absoluto,
      `data-rotulo-percentual` = nome_percentual,
      `aria-label` = paste(nome, sigla)
    )
  })
  rotulos <- lapply(seq_len(nrow(dados_mapa)), function(i) {
    htmltools::tags$text(
      class = "rotulo-uf-mapa",
      x = formatC(dados_mapa$x_rotulo_svg[[i]], format = "f", digits = 2),
      y = formatC(dados_mapa$y_rotulo_svg[[i]], format = "f", digits = 2),
      `data-codigo-uf` = as.character(dados_mapa$codigo_uf[[i]]),
      dados_mapa$sigla_uf[[i]]
    )
  })
  htmltools::tags$div(
    class = "mapa-interativo somente-tela",
    htmltools::tags$h4(titulo),
    htmltools::tags$div(
      class = "area-svg-mapa",
      htmltools::tags$svg(
        class = "svg-mapa-interativo",
        viewBox = sprintf("0 0 %s %s", parametros$largura, parametros$altura),
        role = "img", `aria-label` = titulo,
        caminhos, rotulos
      ),
      htmltools::tags$div(class = "tooltip-mapa-interativo", role = "status")
    ),
    htmltools::tags$div(
      class = "legenda-mapa-interativo",
      htmltools::tags$span(class = "titulo-legenda-mapa", titulo_legenda),
      # Na escala vertical, o maior valor fica no topo e o menor na base.
      htmltools::tags$span(class = "limite-legenda-mapa", formatar_limite(limites[[2]])),
      htmltools::tags$span(class = "gradiente-legenda-mapa"),
      htmltools::tags$span(class = "limite-legenda-mapa", formatar_limite(limites[[1]]))
    )
  )
}

# Agrupa a versão interativa, usada na tela, e a imagem estática já existente,
# usada na impressão. Isso impede que a interatividade altere o PDF.
bloco_mapa_indicador_relatorio <- function(
    dados_mapa, grafico_estatico, indicador, titulo, alt,
    modulo = c("validade", "consistencia"), dimensoes) {
  modulo <- match.arg(modulo)
  htmltools::tags$div(
    class = "bloco-mapa-indicador",
    mapa_interativo_ufs_relatorio(dados_mapa, indicador, titulo, modulo),
    htmltools::tags$div(
      class = "mapa-estatico-impressao",
      imagem_grafico_relatorio(
        grafico_estatico, alt,
        largura = dimensoes$largura, altura = dimensoes$altura
      )
    )
  )
}

# Exibe o mapa de UFs quando a malha geográfica está disponível.
bloco_mapa_relatorio <- function(dados, tabela_ufs, codigo_regiao = NULL, prefixo = "Brasil") {
  if (is.null(dados$geografia_ufs)) {
    return(htmltools::tags$p(class = "aviso", "Malha geográfica indisponível; os totais permanecem nas tabelas."))
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
      mapa_ufs_validade(dados$geografia_ufs, tabela_ufs, "registros_invalidos",
                        paste(prefixo, "— número de registros inválidos"), codigo_regiao),
      "registros_invalidos", paste(prefixo, "— número de registros inválidos"),
      paste("Mapa", prefixo, "por número de registros inválidos"),
      "validade", dimensoes
    ),
    bloco_mapa_indicador_relatorio(
      dados_mapa,
      mapa_ufs_validade(dados$geografia_ufs, tabela_ufs, "percentual_invalidos",
                        paste(prefixo, "— percentual de registros inválidos"), codigo_regiao),
      "percentual_invalidos", paste(prefixo, "— percentual de registros inválidos"),
      paste("Mapa", prefixo, "por percentual de registros inválidos"),
      "validade", dimensoes
    )
  )
}

# Cria o Pareto interativo usado no HTML. Os valores são retirados da mesma
# tabela previamente ordenada pela função calcular_pareto; nada é recalculado
# a partir da base original durante a construção da visualização.
pareto_interativo_relatorio <- function(
    tabela, coluna_nome, titulo,
    modulo = c("validade", "consistencia"),
    limite = configuracao_geral_relatorios()$limites$pareto) {
  modulo <- match.arg(modulo)
  if (!nrow(tabela)) {
    return(htmltools::tags$div(
      class = "pareto-interativo somente-tela",
      htmltools::tags$p(class = "sem-dados", "Sem dados para exibir.")
    ))
  }
  dados <- calcular_pareto(tabela, coluna_nome, limite)
  dados$rotulo <- as.character(dados[[coluna_nome]])
  dados$rotulo[is.na(dados$rotulo) | !nzchar(dados$rotulo)] <- "Não localizado"
  if (!nrow(dados)) {
    return(htmltools::tags$div(
      class = "pareto-interativo somente-tela",
      htmltools::tags$p(class = "sem-dados", "Sem dados para exibir.")
    ))
  }

  dimensoes <- dimensoes_figura_relatorio("pareto")
  largura <- 1200
  # A razão altura/largura vem da configuração central. Assim, o mesmo valor
  # aumenta ou reduz tanto a figura impressa quanto o SVG interativo.
  altura <- max(700, round(largura * dimensoes$altura / dimensoes$largura))
  margens <- list(esquerda = 86, direita = 92, topo = 52, inferior = 255)
  largura_painel <- largura - margens$esquerda - margens$direita
  altura_painel <- altura - margens$topo - margens$inferior
  base_y <- margens$topo + altura_painel
  n <- nrow(dados)
  passo_x <- largura_painel / n
  largura_barra <- max(3, passo_x * .72)
  x <- margens$esquerda + (seq_len(n) - .5) * passo_x
  maximo_absoluto <- max(c(dados$registros_invalidos, 1), na.rm = TRUE)
  y_absoluto <- function(valor) {
    margens$topo + altura_painel * (1 - valor / maximo_absoluto)
  }
  y_percentual <- function(valor) {
    margens$topo + altura_painel * (1 - valor / 100)
  }
  y_barras <- y_absoluto(dados$registros_invalidos)
  y_linha <- y_percentual(dados$contribuicao_acumulada)
  cores <- configuracao_geral_relatorios()$cores
  nome_absoluto <- if (identical(modulo, "consistencia")) {
    "Registros inconsistentes"
  } else {
    "Registros inválidos"
  }
  nome_percentual <- if (identical(modulo, "consistencia")) {
    "Percentual inconsistente"
  } else {
    "Percentual inválido"
  }

  # Atributos comuns alimentam a caixa de detalhes das barras e da linha.
  atributos_pareto <- function(i, classe, tag) {
    atributos <- list(
      classe, "0", as.character(i), dados$rotulo[[i]],
      as.character(dados$registros_invalidos[[i]]),
      as.character(dados$percentual_invalidos[[i]]),
      as.character(dados$contribuicao_acumulada[[i]]),
      nome_absoluto, nome_percentual
    )
    names(atributos) <- c(
      "class", "tabindex", "data-indice-pareto", "data-nome",
      "data-absoluto", "data-percentual", "data-acumulado",
      "data-rotulo-absoluto", "data-rotulo-percentual"
    )
    do.call(htmltools::tagAppendAttributes, c(list(tag), atributos))
  }

  barras <- lapply(seq_len(n), function(i) {
    atributos_pareto(
      i, "barra-pareto-interativa",
      htmltools::tags$rect(
        x = formatC(x[[i]] - largura_barra / 2, format = "f", digits = 2),
        y = formatC(y_barras[[i]], format = "f", digits = 2),
        width = formatC(largura_barra, format = "f", digits = 2),
        height = formatC(base_y - y_barras[[i]], format = "f", digits = 2),
        fill = cores$principal
      )
    )
  })
  pontos_linha <- lapply(seq_len(n), function(i) {
    atributos_pareto(
      i, "ponto-linha-pareto-interativo",
      htmltools::tags$circle(
        cx = formatC(x[[i]], format = "f", digits = 2),
        cy = formatC(y_linha[[i]], format = "f", digits = 2),
        r = "4.5", fill = cores$pareto_linha
      )
    )
  })
  # Segmentos transparentes e largos tornam toda a linha fácil de alcançar com
  # o cursor. Cada segmento apresenta os dados de sua extremidade direita.
  segmentos_interacao <- if (n > 1) {
    lapply(2:n, function(i) {
      atributos_pareto(
        i, "segmento-interacao-pareto",
        htmltools::tags$line(
          x1 = formatC(x[[i - 1]], format = "f", digits = 2),
          y1 = formatC(y_linha[[i - 1]], format = "f", digits = 2),
          x2 = formatC(x[[i]], format = "f", digits = 2),
          y2 = formatC(y_linha[[i]], format = "f", digits = 2)
        )
      )
    })
  } else {
    list()
  }
  pontos_polyline <- paste0(
    formatC(x, format = "f", digits = 2), ",",
    formatC(y_linha, format = "f", digits = 2)
  )
  rotulos_x <- lapply(seq_len(n), function(i) {
    htmltools::tags$text(
      class = "rotulo-eixo-x-pareto",
      x = formatC(x[[i]], format = "f", digits = 2),
      y = formatC(base_y + 13, format = "f", digits = 2),
      transform = sprintf("rotate(-90 %.2f %.2f)", x[[i]], base_y + 13),
      dados$rotulo[[i]]
    )
  })
  quebras_absolutas <- pretty(c(0, maximo_absoluto), n = 5)
  quebras_absolutas <- quebras_absolutas[
    quebras_absolutas >= 0 & quebras_absolutas <= maximo_absoluto
  ]
  grades_absolutas <- lapply(quebras_absolutas, function(valor) {
    y <- y_absoluto(valor)
    htmltools::tagList(
      htmltools::tags$line(
        class = "grade-pareto",
        x1 = margens$esquerda, y1 = y,
        x2 = largura - margens$direita, y2 = y
      ),
      htmltools::tags$text(
        class = "rotulo-eixo-pareto rotulo-eixo-esquerdo",
        x = margens$esquerda - 10, y = y + 4,
        formatar_inteiro_relatorio(valor)
      )
    )
  })
  rotulos_percentuais <- lapply(c(0, 25, 50, 75, 100), function(valor) {
    y <- y_percentual(valor)
    htmltools::tags$text(
      class = "rotulo-eixo-pareto rotulo-eixo-direito",
      x = largura - margens$direita + 10, y = y + 4,
      paste0(valor, "%")
    )
  })
  linhas_referencia <- lapply(c(50, 80), function(valor) {
    htmltools::tags$line(
      class = paste0("linha-referencia-pareto referencia-", valor),
      x1 = margens$esquerda, y1 = y_percentual(valor),
      x2 = largura - margens$direita, y2 = y_percentual(valor)
    )
  })

  htmltools::tags$div(
    class = "pareto-interativo somente-tela",
    htmltools::tags$h4(titulo),
    htmltools::tags$div(
      class = "area-svg-pareto",
      htmltools::tags$svg(
        class = "svg-pareto-interativo",
        viewBox = sprintf("0 0 %d %d", largura, altura),
        role = "img",
        grades_absolutas,
        linhas_referencia,
        htmltools::tags$line(
          class = "eixo-pareto",
          x1 = margens$esquerda, y1 = margens$topo,
          x2 = margens$esquerda, y2 = base_y
        ),
        htmltools::tags$line(
          class = "eixo-pareto",
          x1 = margens$esquerda, y1 = base_y,
          x2 = largura - margens$direita, y2 = base_y
        ),
        barras,
        htmltools::tags$polyline(
          class = "linha-pareto-interativa",
          points = paste(pontos_polyline, collapse = " "),
          fill = "none", stroke = cores$pareto_linha
        ),
        segmentos_interacao,
        pontos_linha,
        rotulos_x,
        rotulos_percentuais,
        htmltools::tags$text(
          class = "titulo-eixo-pareto titulo-eixo-esquerdo",
          x = 20, y = margens$topo + altura_painel / 2,
          transform = sprintf(
            "rotate(-90 20 %.2f)", margens$topo + altura_painel / 2
          ),
          nome_absoluto
        ),
        htmltools::tags$text(
          class = "titulo-eixo-pareto titulo-eixo-direito",
          x = largura - 18, y = margens$topo + altura_painel / 2,
          transform = sprintf(
            "rotate(90 %d %.2f)", largura - 18,
            margens$topo + altura_painel / 2
          ),
          "Contribuição acumulada (%)"
        )
      ),
      htmltools::tags$div(class = "tooltip-pareto-interativo", role = "status")
    ),
    htmltools::tags$div(
      class = "legenda-pareto-interativo",
      htmltools::tags$span(
        htmltools::tags$i(class = "amostra-barra-pareto"),
        nome_absoluto
      ),
      htmltools::tags$span(
        htmltools::tags$i(class = "amostra-linha-pareto"),
        "Contribuição acumulada"
      )
    )
  )
}

# Mantém duas representações do mesmo Pareto: SVG interativo no HTML e PNG
# estático na impressão/PDF. Ambas recebem os indicadores já agregados.
bloco_pareto_relatorio <- function(
    tabela, coluna_nome, titulo, grafico_estatico, alt,
    modulo = c("validade", "consistencia"), dimensoes) {
  modulo <- match.arg(modulo)
  htmltools::tags$div(
    class = "bloco-pareto-relatorio",
    pareto_interativo_relatorio(tabela, coluna_nome, titulo, modulo),
    htmltools::tags$div(
      class = "pareto-estatico-impressao",
      imagem_grafico_relatorio(
        grafico_estatico, alt,
        largura = dimensoes$largura, altura = dimensoes$altura
      )
    )
  )
}

# Seções hierárquicas do relatório: Brasil, região e UF.
secao_brasil_relatorio <- function(dados, numero = 1L) {
  ufs <- calcular_pareto(dados$ufs, "nome_exibicao")
  htmltools::tagList(
    htmltools::tags$section(
      id = "brasil", class = "secao-relatorio",
      htmltools::tags$h2(titulo_numerado_relatorio(
        numero, titulo_de_relatorio("validade", "brasil")
      )),
      htmltools::tags$h3(titulo_numerado_relatorio(
        numero, titulo_de_relatorio("validade", "sintese_nacional"), 1L
      )),
      cards_resumo_relatorio(dados$brasil[1, ]),
      htmltools::tags$h3(titulo_numerado_relatorio(
        numero, titulo_de_relatorio("validade", "resultados_nacionais"), 2L
      )),
      bloco_regras_relatorio(dados$regras_brasil, dados$regras_metodologicas),
      htmltools::tags$h3(titulo_numerado_relatorio(
        numero, titulo_de_relatorio("validade", "distribuicao_uf"), 3L
      )),
      tabela_html_relatorio(
        ufs,
        c("nome_exibicao", "notificacoes", "registros_invalidos", "percentual_invalidos",
          "contribuicao_pareto"),
        c("UF", "Notificações", "Inválidos", "% inválido", "Contribuição")
      ),
      htmltools::tags$h3(titulo_numerado_relatorio(
        numero, titulo_de_relatorio("validade", "mapas"), 4L
      )),
      bloco_mapa_relatorio(dados, dados$ufs)
    )
  )
}

# Monta uma seção regional usando somente a região selecionada pelo usuário.
secao_regiao_relatorio <- function(dados, codigo_regiao, numero) {
  resumo <- dados$regioes[dados$regioes$codigo_regiao == codigo_regiao, , drop = FALSE]
  ufs <- dados$ufs[dados$ufs$codigo_regiao == codigo_regiao, , drop = FALSE]
  regras <- dados$regras_regiao[dados$regras_regiao$codigo_regiao == codigo_regiao, , drop = FALSE]
  nome <- if (nrow(resumo)) resumo$nome_regiao[[1]] else paste("Região", codigo_regiao)
  htmltools::tags$section(
    id = paste0("regiao-", codigo_regiao), class = "secao-relatorio quebra-pagina",
    htmltools::tags$h2(titulo_numerado_relatorio(
      numero, sprintf(titulo_de_relatorio("validade", "regiao"), nome)
    )),
    cards_resumo_relatorio(resumo[1, ]),
    htmltools::tags$h3(titulo_numerado_relatorio(
      numero, titulo_de_relatorio("validade", "resultados_territorio"), 1L
    )),
    bloco_regras_relatorio(regras, dados$regras_metodologicas),
    htmltools::tags$h3(titulo_numerado_relatorio(
      numero, titulo_de_relatorio("validade", "comparacao_ufs"), 2L
    )),
    tabela_html_relatorio(
      calcular_pareto(ufs, "nome_exibicao"),
      c("nome_exibicao", "notificacoes", "registros_invalidos", "percentual_invalidos",
        "contribuicao_pareto"),
      c("UF", "Notificações", "Inválidos", "% inválido", "Contribuição")
    ),
    bloco_mapa_relatorio(dados, dados$ufs, codigo_regiao, paste("Região", nome))
  )
}

# Monta o detalhamento estadual, incluindo municípios e estabelecimentos.
secao_uf_relatorio <- function(
    dados, codigo_uf, numero,
    escala_matriz = configuracao_priorizacao()$escala_padrao) {
  resumo <- dados$ufs[dados$ufs$codigo_uf == codigo_uf, , drop = FALSE]
  if (!nrow(resumo)) return(NULL)
  nome <- paste0(resumo$nome_uf[[1]], " (", resumo$sigla_uf[[1]], ")")
  municipios <- dados$municipios[dados$municipios$codigo_uf == codigo_uf, , drop = FALSE]
  unidades <- dados$cnes[dados$cnes$codigo_uf == codigo_uf, , drop = FALSE]
  regras <- dados$regras_uf[dados$regras_uf$codigo_uf == codigo_uf, , drop = FALSE]
  regras_municipio <- dados$regras_municipio[dados$regras_municipio$codigo_uf == codigo_uf, , drop = FALSE]
  regras_cnes <- dados$regras_cnes[dados$regras_cnes$codigo_uf == codigo_uf, , drop = FALSE]
  pareto_municipios <- calcular_pareto(municipios, "nome_exibicao")
  pareto_cnes <- calcular_pareto(unidades, "nome_exibicao")
  limite_ranking <- configuracao_geral_relatorios()$limites$ranking
  dimensoes_pareto <- dimensoes_figura_relatorio("pareto")
  dominantes_municipios <- identificar_regras_dominantes(
    regras_municipio, "codigo_uf", "codigo_sinan_6", limite_ranking
  )
  dominantes_cnes <- identificar_regras_dominantes(
    regras_cnes, "codigo_uf", "id_unidade", limite_ranking
  )
  if (nrow(dominantes_municipios)) {
    dominantes_municipios$entidade <- rotular_entidade(
      municipios$nome_municipio[match(dominantes_municipios$codigo_sinan_6,
                                      municipios$codigo_sinan_6)],
      dominantes_municipios$codigo_sinan_6, "Município"
    )
  }
  if (nrow(dominantes_cnes)) {
    dominantes_cnes$entidade <- if (isTRUE(dados$possui_referencia_cnes)) {
      rotular_entidade(
        unidades$nome_unidade[match(dominantes_cnes$id_unidade, unidades$id_unidade)],
        dominantes_cnes$id_unidade, "CNES"
      )
    } else paste("CNES", dominantes_cnes$id_unidade)
  }

  htmltools::tags$section(
    id = paste0("uf-", tolower(resumo$sigla_uf[[1]])), class = "secao-relatorio quebra-pagina",
    htmltools::tags$h2(titulo_numerado_relatorio(
      numero, sprintf(titulo_de_relatorio("validade", "uf"), nome)
    )),
    cards_resumo_relatorio(resumo[1, ]),
    htmltools::tags$h3(titulo_numerado_relatorio(
      numero, titulo_de_relatorio("validade", "resultados_territorio"), 1L
    )),
    bloco_regras_relatorio(regras, dados$regras_metodologicas),
    htmltools::tags$h3(titulo_numerado_relatorio(
      numero, titulo_de_relatorio("validade", "municipios"), 2L
    )),
    bloco_pareto_relatorio(
      municipios, "nome_exibicao", paste(nome, "— Pareto dos municípios"),
      grafico_pareto_validade(
        municipios, "nome_exibicao", paste(nome, "— Pareto dos municípios")
      ),
      paste("Pareto de municípios de", nome), "validade", dimensoes_pareto
    ),
    tabela_html_relatorio(
      pareto_municipios,
      c("nome_exibicao", "notificacoes", "registros_invalidos", "percentual_invalidos",
        "contribuicao_pareto"),
      c("Município", "Notificações", "Inválidos", "% inválido", "Contribuição"),
      limite = limite_ranking
    ),
    bloco_matriz_prioridade_relatorio(
      municipios, "nome_exibicao", "codigo_sinan_6",
      paste(nome, "— matriz de prioridade municipal"), "validade",
      regras_dominantes = dominantes_municipios,
      escala_matriz = escala_matriz
    ),
    htmltools::tags$h4("Regras dominantes nos principais municípios"),
    tabela_html_relatorio(
      dominantes_municipios,
      c("entidade", "regra_dominante", "ocorrencias_regra_dominante"),
      c("Município", "Regra dominante", "Ocorrências")
    ),
    htmltools::tags$h3(titulo_numerado_relatorio(
      numero, titulo_de_relatorio("validade", "estabelecimentos"), 3L
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
      unidades, "nome_exibicao", paste(nome, "— Pareto dos estabelecimentos"),
      grafico_pareto_validade(
        unidades, "nome_exibicao", paste(nome, "— Pareto dos estabelecimentos")
      ),
      paste("Pareto de estabelecimentos de", nome), "validade", dimensoes_pareto
    ),
    tabela_html_relatorio(
      pareto_cnes,
      c("nome_exibicao", "notificacoes", "registros_invalidos", "percentual_invalidos",
        "contribuicao_pareto"),
      c("Estabelecimento", "Notificações", "Inválidos", "% inválido", "Contribuição"),
      limite = limite_ranking
    ),
    bloco_matriz_prioridade_relatorio(
      unidades, "nome_exibicao", "id_unidade",
      paste(nome, "— matriz de prioridade dos estabelecimentos"), "validade",
      regras_dominantes = dominantes_cnes,
      escala_matriz = escala_matriz
    ),
    htmltools::tags$h4("Regras dominantes nos principais estabelecimentos"),
    tabela_html_relatorio(
      dominantes_cnes,
      c("entidade", "regra_dominante", "ocorrencias_regra_dominante"),
      c("Estabelecimento", "Regra dominante", "Ocorrências")
    )
  )
}

# Monta o território municipal. As unidades notificadoras, e não outros
# municípios, são o nível interno de priorização desta seção.
secao_municipio_relatorio <- function(
    dados, codigo_municipio, numero,
    escala_matriz = configuracao_priorizacao()$escala_padrao) {
  resumo <- dados$municipios[
    !is.na(dados$municipios$codigo_sinan_6) &
      dados$municipios$codigo_sinan_6 == codigo_municipio, , drop = FALSE
  ]
  if (!nrow(resumo)) return(NULL)
  nome <- paste0(resumo$nome_municipio[[1]], " (", resumo$sigla_uf[[1]], ")")
  unidades <- dados$cnes_municipio[
    !is.na(dados$cnes_municipio$codigo_sinan_6) &
      dados$cnes_municipio$codigo_sinan_6 == codigo_municipio, , drop = FALSE
  ]
  regras <- dados$regras_municipio[
    !is.na(dados$regras_municipio$codigo_sinan_6) &
      dados$regras_municipio$codigo_sinan_6 == codigo_municipio, , drop = FALSE
  ]
  regras_cnes <- dados$regras_cnes_municipio[
    !is.na(dados$regras_cnes_municipio$codigo_sinan_6) &
      dados$regras_cnes_municipio$codigo_sinan_6 == codigo_municipio, , drop = FALSE
  ]
  pareto_cnes <- calcular_pareto(unidades, "nome_exibicao")
  limite_ranking <- configuracao_geral_relatorios()$limites$ranking
  dimensoes_pareto <- dimensoes_figura_relatorio("pareto")
  dominantes_cnes <- identificar_regras_dominantes(
    regras_cnes, "codigo_sinan_6", "id_unidade", limite_ranking
  )
  if (nrow(dominantes_cnes)) {
    dominantes_cnes$entidade <- if (isTRUE(dados$possui_referencia_cnes)) {
      rotular_entidade(
        unidades$nome_unidade[match(dominantes_cnes$id_unidade, unidades$id_unidade)],
        dominantes_cnes$id_unidade, "CNES"
      )
    } else paste("CNES", dominantes_cnes$id_unidade)
  }

  htmltools::tags$section(
    id = paste0("municipio-", codigo_municipio),
    class = "secao-relatorio quebra-pagina secao-municipio",
    htmltools::tags$h2(titulo_numerado_relatorio(
      numero, sprintf(titulo_de_relatorio("validade", "municipio"), nome)
    )),
    cards_resumo_relatorio(resumo[1, ]),
    htmltools::tags$h3(titulo_numerado_relatorio(
      numero, titulo_de_relatorio("validade", "resultados_territorio"), 1L
    )),
    bloco_regras_relatorio(regras, dados$regras_metodologicas),
    htmltools::tags$h3(titulo_numerado_relatorio(
      numero, titulo_de_relatorio("validade", "estabelecimentos"), 2L
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
      unidades, "nome_exibicao", paste(nome, "— Pareto dos estabelecimentos"),
      grafico_pareto_validade(
        unidades, "nome_exibicao", paste(nome, "— Pareto dos estabelecimentos")
      ),
      paste("Pareto de estabelecimentos de", nome), "validade", dimensoes_pareto
    ),
    tabela_html_relatorio(
      pareto_cnes,
      c("nome_exibicao", "notificacoes", "registros_invalidos",
        "percentual_invalidos", "contribuicao_pareto"),
      c("Estabelecimento", "Notificações", "Inválidos", "% inválido", "Contribuição"),
      limite = limite_ranking
    ),
    bloco_matriz_prioridade_relatorio(
      unidades, "nome_exibicao", "id_unidade",
      paste(nome, "— matriz de prioridade dos estabelecimentos"), "validade",
      regras_dominantes = dominantes_cnes,
      escala_matriz = escala_matriz
    ),
    htmltools::tags$h4("Regras dominantes nos principais estabelecimentos"),
    tabela_html_relatorio(
      dominantes_cnes,
      c("entidade", "regra_dominante", "ocorrencias_regra_dominante"),
      c("Estabelecimento", "Regra dominante", "Ocorrências")
    )
  )
}

# Documenta regras, fontes auxiliares e qualidade dos relacionamentos.
secao_metodologia_relatorio <- function(dados, numero) {
  htmltools::tags$section(
    id = "metodologia", class = "secao-relatorio quebra-pagina",
    htmltools::tags$h2(titulo_numerado_relatorio(
      numero, titulo_de_relatorio("validade", "metodologia")
    )),
    htmltools::tags$p(
      "O relatório utiliza exclusivamente a base padronizada e os indicadores já produzidos pela Análise de Validade. As regras não são recalculadas durante a emissão."
    ),
    htmltools::tags$p(
      "A territorialização representa o local de notificação: sg_uf_not para UF, id_municip para município e id_unidade para estabelecimento CNES. Endereço de residência e local de ocorrência não são utilizados."
    ),
    htmltools::tags$p(
      "As abrangências exibidas são escolhidas pelo usuário e a seção nacional é opcional. Nas análises municipais, id_municip identifica o município de notificação e as unidades notificadoras constituem o nível interno de priorização. A ausência de uma abrangência no documento não remove seus registros da base."
    ),
    htmltools::tags$p(
      "Volume é o número absoluto de registros inválidos; qualidade relativa é o percentual de inválidos no total do território; contribuição é a participação do território no total de inválidos. As curvas de Pareto ordenam as unidades pelo número absoluto de inválidos."
    ),
    bloco_metodologia_priorizacao(),
    htmltools::tags$h3(titulo_numerado_relatorio(
      numero, titulo_de_relatorio("validade", "relacionamentos"), 1L
    )),
    tabela_html_relatorio(
      dados$relacionamentos, c("relacionamento", "registros", "observacao"),
      c("Verificação", "Registros", "Observação")
    ),
    htmltools::tags$p(class = "observacao",
      "Registros com código ausente ou não localizado permanecem nos totais gerais e são explicitados nesta auditoria; nenhuma linha é descartada silenciosamente."
    )
  )
}

# Ativa realce, caixa de detalhes e seleção persistente nos mapas SVG. Todo o
# comportamento ocorre no navegador; nenhum indicador é recalculado ao mover
# ou clicar sobre uma UF.
javascript_mapas_interativos <- function() {
  "
  document.addEventListener('DOMContentLoaded', function() {
    var formatarInteiroMapa = new Intl.NumberFormat('pt-BR', {
      maximumFractionDigits: 0
    });
    var formatarPercentualMapa = new Intl.NumberFormat('pt-BR', {
      minimumFractionDigits: 2, maximumFractionDigits: 2
    });

    document.querySelectorAll('.mapa-interativo').forEach(function(mapa) {
      var area = mapa.querySelector('.area-svg-mapa');
      var svg = mapa.querySelector('.svg-mapa-interativo');
      var estados = Array.from(mapa.querySelectorAll('.estado-mapa-interativo'));
      var rotulos = Array.from(mapa.querySelectorAll('.rotulo-uf-mapa'));
      var tooltip = mapa.querySelector('.tooltip-mapa-interativo');
      var codigoFixado = null;

      function aplicarDestaque(codigo) {
        estados.forEach(function(estado) {
          var corresponde = estado.dataset.codigoUf === codigo;
          estado.classList.toggle('em-destaque', corresponde);
          estado.classList.toggle('esmaecido', Boolean(codigo) && !corresponde);
        });
        rotulos.forEach(function(rotulo) {
          var corresponde = rotulo.dataset.codigoUf === codigo;
          rotulo.classList.toggle('em-destaque', corresponde);
          rotulo.classList.toggle('esmaecido', Boolean(codigo) && !corresponde);
        });
      }

      function conteudoTooltip(estado) {
        var absoluto = estado.dataset.absoluto;
        var percentual = estado.dataset.percentual;
        var nomeCompleto = estado.dataset.nomeUf;
        if (estado.dataset.siglaUf) {
          nomeCompleto += ' (' + estado.dataset.siglaUf + ')';
        }
        return '<strong>' + nomeCompleto + '</strong><br>' +
          estado.dataset.rotuloAbsoluto + ': <strong>' +
          (absoluto === '' ? 'não disponível' : formatarInteiroMapa.format(Number(absoluto))) +
          '</strong><br>' + estado.dataset.rotuloPercentual + ': <strong>' +
          (percentual === '' ? 'não disponível' :
            formatarPercentualMapa.format(Number(percentual)) + '%') + '</strong>';
      }

      function posicionarTooltip(evento) {
        var caixaArea = area.getBoundingClientRect();
        var larguraTooltip = tooltip.offsetWidth || 260;
        var alturaTooltip = tooltip.offsetHeight || 90;
        var x = evento.clientX - caixaArea.left + 14;
        var y = evento.clientY - caixaArea.top + 14;
        if (x + larguraTooltip > caixaArea.width - 8) {
          x = evento.clientX - caixaArea.left - larguraTooltip - 14;
        }
        if (y + alturaTooltip > caixaArea.height - 8) {
          y = evento.clientY - caixaArea.top - alturaTooltip - 14;
        }
        tooltip.style.left = Math.max(8, x) + 'px';
        tooltip.style.top = Math.max(8, y) + 'px';
      }

      function exibirTooltip(estado, evento) {
        tooltip.innerHTML = conteudoTooltip(estado);
        tooltip.style.display = 'block';
        posicionarTooltip(evento);
      }

      estados.forEach(function(estado) {
        estado.addEventListener('pointerenter', function(evento) {
          aplicarDestaque(estado.dataset.codigoUf);
          exibirTooltip(estado, evento);
        });
        estado.addEventListener('pointermove', posicionarTooltip);
        estado.addEventListener('pointerleave', function() {
          tooltip.style.display = 'none';
          aplicarDestaque(codigoFixado);
        });
        estado.addEventListener('click', function(evento) {
          evento.stopPropagation();
          codigoFixado = codigoFixado === estado.dataset.codigoUf ?
            null : estado.dataset.codigoUf;
          aplicarDestaque(codigoFixado || estado.dataset.codigoUf);
          exibirTooltip(estado, evento);
        });
        estado.addEventListener('focus', function() {
          aplicarDestaque(estado.dataset.codigoUf);
          var caixaEstado = estado.getBoundingClientRect();
          exibirTooltip(estado, {
            clientX: caixaEstado.left + caixaEstado.width / 2,
            clientY: caixaEstado.top + caixaEstado.height / 2
          });
        });
        estado.addEventListener('blur', function() {
          tooltip.style.display = 'none';
          aplicarDestaque(codigoFixado);
        });
        estado.addEventListener('keydown', function(evento) {
          if (evento.key === 'Enter' || evento.key === ' ') {
            evento.preventDefault();
            estado.click();
          }
        });
      });

      svg.addEventListener('click', function(evento) {
        if (evento.target === svg) {
          codigoFixado = null;
          aplicarDestaque(null);
          tooltip.style.display = 'none';
        }
      });
    });
  });
  "
}

# Controla o realce e a caixa de informações dos Paretos em SVG. Barras,
# pontos e segmentos da curva usam os mesmos atributos, portanto apresentam
# exatamente os mesmos indicadores sem executar cálculos no navegador.
javascript_paretos_interativos <- function() {
  "
  document.addEventListener('DOMContentLoaded', function() {
    var formatarInteiroPareto = new Intl.NumberFormat('pt-BR', {
      maximumFractionDigits: 0
    });
    var formatarPercentualPareto = new Intl.NumberFormat('pt-BR', {
      minimumFractionDigits: 2, maximumFractionDigits: 2
    });

    document.querySelectorAll('.pareto-interativo').forEach(function(pareto) {
      var area = pareto.querySelector('.area-svg-pareto');
      if (!area) return;
      var svg = pareto.querySelector('.svg-pareto-interativo');
      var tooltip = pareto.querySelector('.tooltip-pareto-interativo');
      var alvos = Array.from(pareto.querySelectorAll(
        '.barra-pareto-interativa,.ponto-linha-pareto-interativo,' +
        '.segmento-interacao-pareto'
      ));
      var elementosVisuais = Array.from(pareto.querySelectorAll(
        '.barra-pareto-interativa,.ponto-linha-pareto-interativo'
      ));
      var indiceFixado = null;

      function aplicarDestaque(indice) {
        elementosVisuais.forEach(function(elemento) {
          var corresponde = elemento.dataset.indicePareto === indice;
          elemento.classList.toggle('em-destaque', corresponde);
          elemento.classList.toggle('esmaecido', Boolean(indice) && !corresponde);
        });
      }

      function conteudoTooltip(alvo) {
        return '<strong>' + alvo.dataset.nome + '</strong><br>' +
          alvo.dataset.rotuloAbsoluto + ': <strong>' +
          formatarInteiroPareto.format(Number(alvo.dataset.absoluto)) +
          '</strong><br>' + alvo.dataset.rotuloPercentual + ': <strong>' +
          formatarPercentualPareto.format(Number(alvo.dataset.percentual)) +
          '%</strong><br>Percentual acumulado: <strong>' +
          formatarPercentualPareto.format(Number(alvo.dataset.acumulado)) +
          '%</strong>';
      }

      function posicionarTooltip(evento) {
        var caixaArea = area.getBoundingClientRect();
        var larguraTooltip = tooltip.offsetWidth || 285;
        var alturaTooltip = tooltip.offsetHeight || 110;
        var x = evento.clientX - caixaArea.left + 14;
        var y = evento.clientY - caixaArea.top + 14;
        if (x + larguraTooltip > caixaArea.width - 8) {
          x = evento.clientX - caixaArea.left - larguraTooltip - 14;
        }
        if (y + alturaTooltip > caixaArea.height - 8) {
          y = evento.clientY - caixaArea.top - alturaTooltip - 14;
        }
        tooltip.style.left = Math.max(8, x) + 'px';
        tooltip.style.top = Math.max(8, y) + 'px';
      }

      function exibirTooltip(alvo, evento) {
        tooltip.innerHTML = conteudoTooltip(alvo);
        tooltip.style.display = 'block';
        posicionarTooltip(evento);
      }

      alvos.forEach(function(alvo) {
        alvo.addEventListener('pointerenter', function(evento) {
          aplicarDestaque(alvo.dataset.indicePareto);
          exibirTooltip(alvo, evento);
        });
        alvo.addEventListener('pointermove', posicionarTooltip);
        alvo.addEventListener('pointerleave', function() {
          tooltip.style.display = 'none';
          aplicarDestaque(indiceFixado);
        });
        alvo.addEventListener('click', function(evento) {
          evento.stopPropagation();
          indiceFixado = indiceFixado === alvo.dataset.indicePareto ?
            null : alvo.dataset.indicePareto;
          aplicarDestaque(indiceFixado || alvo.dataset.indicePareto);
          exibirTooltip(alvo, evento);
        });
        alvo.addEventListener('focus', function() {
          aplicarDestaque(alvo.dataset.indicePareto);
          var caixaAlvo = alvo.getBoundingClientRect();
          exibirTooltip(alvo, {
            clientX: caixaAlvo.left + caixaAlvo.width / 2,
            clientY: caixaAlvo.top + caixaAlvo.height / 2
          });
        });
        alvo.addEventListener('blur', function() {
          tooltip.style.display = 'none';
          aplicarDestaque(indiceFixado);
        });
        alvo.addEventListener('keydown', function(evento) {
          if (evento.key === 'Enter' || evento.key === ' ') {
            evento.preventDefault();
            alvo.click();
          }
        });
      });

      svg.addEventListener('click', function(evento) {
        if (evento.target === svg) {
          indiceFixado = null;
          aplicarDestaque(null);
          tooltip.style.display = 'none';
        }
      });
    });
  });
  "
}

# Folha de estilos incorporada ao arquivo para dispensar recursos externos.
css_relatorio_validade <- function() {
  geral <- configuracao_geral_relatorios()
  cores <- geral$cores
  layout <- geral$layout
  # `paste0` permite inserir as escolhas do arquivo de configuração sem manter
  # uma segunda paleta escondida dentro da folha de estilos.
  paste0("
  :root { --verde:", cores$principal, "; --principal:", cores$principal,
    "; --principal-escura:", cores$principal_escura, "; --marca:", cores$marca,
    "; --validacao:", cores$validacao, "; --validacao-escura:", cores$validacao_escura,
    "; --superficie:", cores$superficie_suave, "; --superficie-marca:", cores$superficie_marca,
    "; --texto:", cores$texto, "; --cinza:", cores$texto_secundario,
    "; --borda:", cores$borda, "; }
  * { box-sizing:border-box; }
  body { margin:0; color:var(--texto); font-family:'Segoe UI',Arial,sans-serif;
    line-height:1.5; background:", cores$fundo, "; }
  .cabecalho-relatorio { position:relative; display:grid; grid-template-columns:minmax(230px,330px) 1fr;
    align-items:center; gap:38px; color:var(--texto); background:white;
    border-bottom:6px solid var(--marca); padding:30px max(5vw,32px) 28px; }
  .cabecalho-relatorio::after { content:''; position:absolute; right:0; bottom:-6px; width:22%; height:6px;
    background:var(--validacao); }
  .marca-relatorio { display:flex; align-items:center; min-height:82px; }
  .logo-relatorio { display:block; width:100%; max-width:320px; max-height:96px; object-fit:contain;
    object-position:left center; }
  .identificacao-relatorio { min-width:0; padding-left:34px; border-left:1px solid var(--borda); }
  .rotulo-relatorio { display:block; color:var(--marca); font-size:11px; font-weight:800;
    letter-spacing:.12em; margin-bottom:8px; text-transform:uppercase; }
  .cabecalho-relatorio h1 { color:var(--principal); margin:0 0 12px; font-size:30px; line-height:1.18; }
  .metadados-relatorio { display:flex; flex-wrap:wrap; gap:8px 18px; color:var(--cinza); font-size:13px; }
  .cabecalho-relatorio p { margin:5px 0; }
  nav { position:sticky; top:0; z-index:2; background:rgba(255,255,255,.97);
    border-bottom:1px solid var(--borda); box-shadow:0 3px 12px rgba(54,35,74,.06); padding:11px 5vw; }
  nav a { display:inline-block; color:var(--principal); margin:3px 18px 3px 0;
    text-decoration:none; font-weight:700; }
  nav a:hover,nav a:focus { color:var(--marca); text-decoration:underline; text-underline-offset:4px; }
  main { max-width:", layout$largura_conteudo_px, "px; margin:24px auto; padding:0 24px 60px; }
  .rodape-relatorio { display:flex; align-items:flex-start; gap:15px; max-width:", layout$largura_conteudo_px, "px;
    margin:0 auto 30px; padding:22px 24px; border-top:1px solid var(--borda);
    color:var(--cinza); font-size:12px; }
  .simbolo-rodape-relatorio { width:42px; height:42px; flex:0 0 42px; object-fit:contain; }
  .texto-rodape-relatorio { min-width:0; }
  .rodape-relatorio p { margin:4px 0; }
  .secao-relatorio { background:white; border:1px solid var(--borda); border-radius:14px;
    margin:24px 0; padding:30px; box-shadow:0 7px 24px rgba(54,35,74,.055); }
  h2,h3,h4 { color:var(--principal); }
  h2 { margin-top:0; border-left:5px solid var(--marca); border-bottom:1px solid var(--borda);
    padding:4px 0 10px 14px; }
  h3 { margin-top:26px; }
  .cards-resumo { display:grid; grid-template-columns:repeat(auto-fit,minmax(180px,1fr)); gap:12px; margin:18px 0; }
  .card-resumo { background:var(--superficie); border:1px solid var(--borda);
    border-top:4px solid var(--marca); border-radius:9px; padding:15px; }
  .card-resumo span { display:block; color:var(--cinza); font-size:11px; font-weight:700;
    letter-spacing:.05em; text-transform:uppercase; }
  .card-resumo strong { display:block; color:var(--texto); font-size:22px; margin-top:4px; }
  .tabela-rolagem { overflow-x:auto; margin:13px 0 24px; }
  .tabela-relatorio { width:100%; border-collapse:collapse; font-size:13px; }
  .tabela-relatorio th { color:white; background:var(--principal); text-align:left; }
  .tabela-relatorio th,.tabela-relatorio td { border:1px solid var(--borda); padding:7px 9px; }
  .tabela-relatorio tr:nth-child(even) { background:#FBF9FC; }
  .tabela-relatorio tbody tr:hover { background:var(--superficie-marca); }
  .grafico-relatorio { display:block; width:100%; height:auto; margin:18px auto 28px; }
  .somente-tela { display:block; }
  .matriz-estatica-impressao,.quadro-matriz-estatica-impressao { display:none; }
  .bloco-matriz-prioridade { margin:18px 0 30px; }
  .controles-matriz-prioridade,.legenda-interativa-prioridade,.filtros-tabela-prioridade {
    display:flex; flex-wrap:wrap; gap:8px; align-items:center; margin:9px 0; }
  .controles-matriz-prioridade button,.legenda-interativa-prioridade button,
  .filtros-tabela-prioridade select,.filtros-tabela-prioridade input {
    border:1px solid var(--borda); border-radius:8px; background:white; color:var(--texto);
    cursor:pointer; min-height:36px; padding:7px 10px; }
  .controles-matriz-prioridade button:hover,.controles-matriz-prioridade button:focus,
  .legenda-interativa-prioridade button:hover,.legenda-interativa-prioridade button:focus {
    border-color:var(--marca); box-shadow:0 0 0 3px rgba(144,8,152,.10); outline:none; }
  .controles-matriz-prioridade .separador-controles-matriz { color:var(--cinza); margin-left:6px; }
  .controles-matriz-prioridade .selecionar-escala-matriz.ativa {
    background:var(--verde); border-color:var(--verde); color:white; font-weight:bold; }
  .legenda-interativa-prioridade button { display:inline-flex; gap:6px; align-items:center; }
  .legenda-interativa-prioridade button span { width:12px; height:12px; border-radius:50%; display:inline-block; }
  .legenda-interativa-prioridade button.inativa { opacity:.38; text-decoration:line-through; }
  .area-svg-prioridade { position:relative; width:100%; overflow:hidden; border:1px solid var(--borda);
    border-radius:9px; background:white; }
  .svg-matriz-prioridade { display:block; width:100%; height:auto; min-height:430px; touch-action:none; cursor:grab; }
  .svg-matriz-prioridade:active { cursor:grabbing; }
  .ponto-prioridade { cursor:pointer; transition:opacity .14s ease,filter .14s ease,stroke-width .14s ease; }
  .ponto-prioridade:focus { outline:none; }
  .ponto-prioridade.em-destaque { opacity:1 !important; filter:saturate(1.75) contrast(1.18);
    stroke:#1f2937; stroke-width:2.2; }
  .ponto-prioridade.esmaecida { opacity:.14 !important; filter:saturate(.35); }
  .tooltip-prioridade { display:none; position:absolute; z-index:5; width:min(320px,85%);
    padding:10px 12px; border-radius:7px; background:rgba(17,24,39,.96); color:white;
    font-size:12px; line-height:1.45; pointer-events:none; box-shadow:0 4px 14px rgba(0,0,0,.25); }
  .nota-matriz-prioridade { margin-top:9px; }
  .detalhes-tabela-prioridade { border:1px solid var(--borda); border-radius:9px; margin-top:14px; overflow:hidden; }
  .detalhes-tabela-prioridade > summary { cursor:pointer; color:var(--principal);
    background:var(--superficie-marca); font-weight:bold; padding:12px 14px; }
  .detalhes-tabela-prioridade .tabela-rolagem { max-height:520px; margin:0; }
  .tabela-prioridades th { position:sticky; top:0; z-index:1; }
  .grade-graficos { display:grid; grid-template-columns:repeat(auto-fit,minmax(380px,1fr)); gap:15px; }
  .grade-graficos.mapas-verticais { grid-template-columns:1fr; }
  .bloco-mapa-indicador { min-width:0; }
  .mapa-estatico-impressao { display:none; }
  .mapa-interativo { display:grid; grid-template-columns:minmax(0,1fr) auto;
    align-items:center; column-gap:8px; margin:18px 0 28px; border:1px solid var(--borda);
    border-radius:9px; background:white; padding:12px; }
  .mapa-interativo h4 { grid-column:1 / -1; margin:0 0 8px; text-align:center;
    font-size:15px; }
  .area-svg-mapa { position:relative; width:100%; }
  .svg-mapa-interativo { display:block; width:100%; height:auto; min-height:360px; }
  .estado-mapa-interativo { cursor:pointer; stroke:white; stroke-width:1.1;
    vector-effect:non-scaling-stroke;
    transition:opacity .14s ease,filter .14s ease,stroke .14s ease,stroke-width .14s ease; }
  .estado-mapa-interativo:focus { outline:none; }
  .estado-mapa-interativo.em-destaque { opacity:1 !important;
    filter:saturate(1.8) contrast(1.15); stroke:var(--texto); stroke-width:2.7; }
  .estado-mapa-interativo.esmaecido { opacity:.22 !important; filter:saturate(.35); }
  .rotulo-uf-mapa { fill:var(--texto); font-size:11px; font-weight:600;
    text-anchor:middle; dominant-baseline:central; pointer-events:none;
    transition:opacity .14s ease,font-size .14s ease,font-weight .14s ease; }
  .rotulo-uf-mapa.em-destaque { opacity:1; font-size:13px; font-weight:800; }
  .rotulo-uf-mapa.esmaecido { opacity:.28; }
  .tooltip-mapa-interativo { display:none; position:absolute; z-index:5; width:min(285px,86%);
    padding:10px 12px; border-radius:7px; background:rgba(17,24,39,.96); color:white;
    font-size:12px; line-height:1.5; pointer-events:none; box-shadow:0 4px 14px rgba(0,0,0,.25); }
  .legenda-mapa-interativo { display:grid; grid-template-columns:auto;
    grid-template-rows:auto auto 190px auto; justify-items:center; align-items:center;
    gap:5px; width:82px; color:var(--cinza); font-size:10px; text-align:center; }
  .titulo-legenda-mapa { line-height:1.2; }
  .gradiente-legenda-mapa { display:block; width:13px; height:190px;
    border:1px solid var(--borda);
    background:linear-gradient(0deg,", cores$mapa_baixo, ",", cores$mapa_alto, "); }
  .pareto-estatico-impressao { display:none; }
  .bloco-pareto-relatorio { margin:18px 0 28px; }
  .pareto-interativo { border:1px solid var(--borda); border-radius:9px;
    background:white; padding:12px; }
  .pareto-interativo h4 { margin:0 0 8px; text-align:center; font-size:16px; }
  .area-svg-pareto { position:relative; width:100%; overflow:hidden; }
  .svg-pareto-interativo { display:block; width:100%; height:auto; }
  .grade-pareto { stroke:", cores$borda, "; stroke-width:1; }
  .eixo-pareto { stroke:", cores$texto_secundario, "; stroke-width:1.2; }
  .linha-referencia-pareto { stroke-width:1.4; stroke-dasharray:7 6; opacity:.78; }
  .linha-referencia-pareto.referencia-50 { stroke:", cores$pareto_50, "; }
  .linha-referencia-pareto.referencia-80 { stroke:", cores$pareto_80, "; }
  .barra-pareto-interativa,.ponto-linha-pareto-interativo {
    cursor:pointer; transition:opacity .14s ease,filter .14s ease,stroke-width .14s ease; }
  .barra-pareto-interativa:focus,.ponto-linha-pareto-interativo:focus,
  .segmento-interacao-pareto:focus { outline:none; }
  .barra-pareto-interativa.em-destaque,.ponto-linha-pareto-interativo.em-destaque {
    opacity:1 !important; filter:saturate(1.65) contrast(1.12);
    stroke:var(--texto); stroke-width:2; }
  .barra-pareto-interativa.esmaecido,.ponto-linha-pareto-interativo.esmaecido {
    opacity:.18 !important; filter:saturate(.35); }
  .linha-pareto-interativa { stroke-width:3; pointer-events:none; }
  .segmento-interacao-pareto { stroke:transparent; stroke-width:18;
    cursor:pointer; pointer-events:stroke; }
  .rotulo-eixo-x-pareto { fill:var(--texto); font-size:10px; text-anchor:end; }
  .rotulo-eixo-pareto { fill:var(--cinza); font-size:11px; }
  .rotulo-eixo-esquerdo { text-anchor:end; }
  .rotulo-eixo-direito { text-anchor:start; }
  .titulo-eixo-pareto { fill:var(--texto); font-size:13px; text-anchor:middle; }
  .tooltip-pareto-interativo { display:none; position:absolute; z-index:5;
    width:min(310px,86%); padding:10px 12px; border-radius:7px;
    background:rgba(17,24,39,.96); color:white; font-size:12px; line-height:1.5;
    pointer-events:none; box-shadow:0 4px 14px rgba(0,0,0,.25); }
  .legenda-pareto-interativo { display:flex; justify-content:center; gap:22px;
    flex-wrap:wrap; color:var(--cinza); font-size:12px; margin:6px 0 2px; }
  .legenda-pareto-interativo span { display:inline-flex; align-items:center; gap:7px; }
  .amostra-barra-pareto { display:inline-block; width:13px; height:13px;
    background:var(--verde); }
  .amostra-linha-pareto { display:inline-block; width:25px; height:0;
    border-top:3px solid ", cores$pareto_linha, "; }
  .aviso { background:#FFF8E7; border-left:4px solid #D38B12; border-radius:7px; padding:11px 13px; }
  .observacao,.sem-dados { color:var(--cinza); font-size:13px; }
  @media (max-width:760px) {
    .cabecalho-relatorio { grid-template-columns:1fr; gap:18px; padding:24px 20px; }
    .marca-relatorio { min-height:auto; }
    .logo-relatorio { max-width:270px; }
    .identificacao-relatorio { padding:18px 0 0; border-left:0; border-top:1px solid var(--borda); }
    .cabecalho-relatorio h1 { font-size:25px; }
    main { padding-left:14px; padding-right:14px; }
    .secao-relatorio { padding:21px 18px; }
    .grade-graficos { grid-template-columns:1fr; }
    .rodape-relatorio { margin-left:14px; margin-right:14px; }
  }
  @media print { body { background:white; }
    .cabecalho-relatorio { grid-template-columns:215px 1fr; gap:24px; border-bottom-width:4px;
      padding:16px 10px 18px; break-inside:avoid; page-break-inside:avoid; }
    .cabecalho-relatorio::after { bottom:-4px; height:4px; }
    .logo-relatorio { max-width:205px; max-height:70px; }
    .identificacao-relatorio { padding-left:22px; }
    .cabecalho-relatorio h1 { font-size:24px; }
    .metadados-relatorio { font-size:11px; }
    nav { display:none; } main { max-width:none; margin:0; padding:0; }
    .secao-relatorio { border:0; box-shadow:none; margin:0; padding:18px 10px; }
    .quebra-pagina { break-before:page; } .tabela-rolagem { overflow:visible; }
    .somente-tela,.matriz-interativa,.mapa-interativo { display:none !important; }
    .mapa-estatico-impressao { display:block !important; break-inside:avoid;
      page-break-inside:avoid; }
    .pareto-estatico-impressao { display:block !important; break-inside:avoid;
      page-break-inside:avoid; }
    .quadro-matriz-estatica-impressao { display:block !important;
      break-before:page; page-break-before:always;
      break-inside:avoid; page-break-inside:avoid; overflow:hidden; }
    .matriz-estatica-impressao { display:block !important; width:auto !important;
      max-width:100%; height:auto; max-height:560px !important; margin:0 auto 14px; }
    /* O Edge pode manter a caixa de pintura do PNG alguns pixels além da caixa
       de layout. Nota e tabela ficam em camada superior para nunca serem
       encobertas pelo fundo branco da figura. */
    .nota-matriz-prioridade,.detalhes-tabela-prioridade {
      position:relative; z-index:3; background:white; }
    .detalhes-tabela-prioridade { display:block; border:0; }
    .detalhes-tabela-prioridade > summary { display:none; }
    .detalhes-tabela-prioridade .tabela-rolagem { max-height:none; }
    .grade-graficos { display:block; } .grafico-relatorio { max-height:",
      layout$altura_maxima_figura_pdf_px, "px; object-fit:contain; }
  }
  ")
}

# Combina cabeçalho, sumário, seções analíticas e metodologia em HTML completo.
montar_documento_relatorio_validade <- function(config_relatorio, dados) {
  configuracao_documento <- configuracao_de_relatorio("validade")
  dados <- preparar_rotulos_relatorio(dados)
  regioes <- config_relatorio$regioes_selecionadas
  ufs <- config_relatorio$ufs_selecionadas
  municipios <- config_relatorio$municipios_selecionados
  nomes_regioes <- dados$referencia_ufs$nome_regiao[
    match(regioes, dados$referencia_ufs$codigo_regiao)
  ]
  nomes_ufs <- dados$referencia_ufs$sigla_uf[match(ufs, dados$referencia_ufs$codigo_uf)]
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
  config_json <- jsonlite::toJSON(config_relatorio, auto_unbox = TRUE, null = "null",
                                  POSIXt = "ISO8601", pretty = TRUE)
  htmltools::tags$html(
    lang = "pt-BR",
    htmltools::tags$head(
      htmltools::tags$meta(charset = "UTF-8"),
      htmltools::tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
      htmltools::tags$title(configuracao_documento$titulo_documento),
      htmltools::tags$style(htmltools::HTML(css_relatorio_validade())),
      htmltools::tags$script(type = "application/json", id = "config-relatorio",
                             htmltools::HTML(config_json)),
      htmltools::tags$script(htmltools::HTML(javascript_matrizes_prioridade())),
      htmltools::tags$script(htmltools::HTML(javascript_mapas_interativos())),
      htmltools::tags$script(htmltools::HTML(javascript_paretos_interativos()))
    ),
    htmltools::tags$body(
      cabecalho_identidade_relatorio(
        configuracao_documento$titulo_documento,
        config_relatorio$periodo,
        config_relatorio$gerado_em
      ),
      navegacao,
      htmltools::tags$main(
        if (isTRUE(config_relatorio$incluir_brasil)) {
          secao_brasil_relatorio(dados, numero_brasil[[1]])
        },
        lapply(seq_along(regioes), function(i) {
          secao_regiao_relatorio(dados, regioes[[i]], numeros_regioes[[i]])
        }),
        lapply(seq_along(ufs), function(i) secao_uf_relatorio(
          dados, ufs[[i]], numeros_ufs[[i]], config_relatorio$escala_matriz_prioridade
        )),
        lapply(seq_along(municipios), function(i) secao_municipio_relatorio(
          dados, municipios[[i]], numeros_municipios[[i]],
          config_relatorio$escala_matriz_prioridade
        )),
        secao_metodologia_relatorio(dados, numero_metodologia[[1]])
      ),
      bloco_autoria_relatorio()
    )
  )
}

# Localiza o Microsoft Edge usado no Windows para imprimir HTML em PDF.
localizar_edge <- function() {
  candidatos <- c(
    Sys.getenv("EDGE_PATH", unset = ""),
    "C:/Program Files (x86)/Microsoft/Edge/Application/msedge.exe",
    "C:/Program Files/Microsoft/Edge/Application/msedge.exe"
  )
  candidatos <- candidatos[nzchar(candidatos) & file.exists(candidatos)]
  if (!length(candidatos)) return(NULL)
  normalizePath(candidatos[[1]], winslash = "/", mustWork = TRUE)
}

# Solicita ao navegador a impressão sem interface e aguarda o arquivo terminar.
converter_html_para_pdf <- function(caminho_html, caminho_pdf, edge = localizar_edge(),
                                    expandir_detalhes = FALSE) {
  if (is.null(edge)) {
    stop("Microsoft Edge ou Chrome compatível não foi localizado para gerar o PDF.", call. = FALSE)
  }
  # Remove somente o destino exato antes de iniciar a impressão. Sem isso, um
  # PDF de rodada anterior poderia satisfazer a espera abaixo e ser entregue
  # novamente, mesmo quando o HTML e a escala selecionada já tivessem mudado.
  if (file.exists(caminho_pdf) && !file.remove(caminho_pdf)) {
    stop("Não foi possível substituir o PDF anterior; feche o arquivo e tente novamente.",
         call. = FALSE)
  }
  perfil <- tempfile("qualifica-viva-edge-")
  dir.create(perfil, recursive = TRUE)
  on.exit(unlink(perfil, recursive = TRUE, force = TRUE), add = TRUE)
  url <- paste0("file:///", utils::URLencode(
    normalizePath(caminho_html, winslash = "/", mustWork = TRUE), reserved = FALSE
  ))
  if (isTRUE(expandir_detalhes)) url <- paste0(url, "?expandir_regras=1")
  argumentos <- c(
    "--headless=new", "--disable-gpu", "--disable-software-rasterizer",
    "--disable-gpu-compositing", "--disable-features=Vulkan,UseSkiaRenderer",
    "--no-sandbox", "--disable-dev-shm-usage", "--no-first-run",
    "--no-pdf-header-footer",
    paste0("--user-data-dir=", shQuote(normalizePath(perfil, winslash = "/"))),
    paste0("--print-to-pdf=", shQuote(paste0(
      normalizePath(dirname(caminho_pdf), winslash = "/"), "/", basename(caminho_pdf)
    ))),
    shQuote(url)
  )
  saida <- suppressWarnings(system2(edge, argumentos, stdout = TRUE, stderr = TRUE))
  limite_espera <- Sys.time() + 30
  while ((!file.exists(caminho_pdf) || file.info(caminho_pdf)$size <= 0) &&
         Sys.time() < limite_espera) {
    Sys.sleep(0.25)
  }
  if (!file.exists(caminho_pdf) || file.info(caminho_pdf)$size <= 0) {
    stop(sprintf("O navegador não gerou o PDF. Detalhes: %s", paste(saida, collapse = " ")),
         call. = FALSE)
  }
  caminho_pdf
}

# Orquestra carregamento das referências, agregações, montagem e gravação.
# Retorna os caminhos dos artefatos e avisos para a interface de download.
gerar_relatorio_validade <- function(base_padronizada, resultado_validade,
                                     incluir_brasil = FALSE,
                                     regioes = character(), ufs = character(),
                                     municipios = character(),
                                     diretorio_referencias = file.path("data", "reference"),
                                     diretorio_saida = tempfile("relatorio-validade-"),
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
  dados <- pre_agregar_relatorio_validade(
    base_padronizada, resultado_validade, referencias,
    ufs_detalhadas = config$ufs_selecionadas,
    municipios_detalhados = config$municipios_selecionados
  )
  rm(referencias)
  liberar_memoria_ram()
  dir.create(diretorio_saida, recursive = TRUE, showWarnings = FALSE)
  periodo_arquivo <- gsub("[^0-9A-Za-z_-]+", "_", config$periodo)
  base_nome <- paste0(
    configuracao_de_relatorio("validade")$nome_arquivo, "_", periodo_arquivo
  )
  caminho_html <- file.path(diretorio_saida, paste0(base_nome, ".html"))
  caminho_pdf <- file.path(diretorio_saida, paste0(base_nome, ".pdf"))
  concluido <- FALSE
  on.exit({
    if (!concluido) remover_arquivos_temporarios_relatorio(list(
      caminho_html = caminho_html, caminho_pdf = caminho_pdf
    ))
  }, add = TRUE)
  documento <- montar_documento_relatorio_validade(config, dados)
  rm(dados)
  liberar_memoria_ram()
  htmltools::save_html(documento, caminho_html, background = "white")
  rm(documento)
  liberar_memoria_ram(completa = TRUE)
  if (isTRUE(gerar_pdf)) converter_html_para_pdf(caminho_html, caminho_pdf)
  concluido <- TRUE
  list(
    config_relatorio = config,
    caminho_html = normalizePath(caminho_html, winslash = "/", mustWork = TRUE),
    caminho_pdf = if (file.exists(caminho_pdf)) {
      normalizePath(caminho_pdf, winslash = "/", mustWork = TRUE)
    } else NULL
  )
}
