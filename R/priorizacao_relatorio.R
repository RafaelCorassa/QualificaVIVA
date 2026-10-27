# Priorização territorial compartilhada --------------------------------------
#
# Este módulo contém o único algoritmo de prioridade usado pelos relatórios de
# Validade e Consistência. Ele recebe tabelas já agregadas; portanto, não lê a
# base original, não executa regras analíticas e não recalcula as análises.
#
# A apresentação possui duas saídas derivadas da mesma tabela classificada:
# - SVG interativo, incorporado diretamente ao HTML e utilizável sem internet;
# - ggplot estático, incorporado como PNG e exibido somente na impressão/PDF.

# Confere os parâmetros editáveis antes de usá-los. Mensagens explícitas evitam
# que um valor inválido produza classificações silenciosamente incorretas.
validar_configuracao_priorizacao <- function(config = configuracao_priorizacao()) {
  percentis <- c(config$percentil_absoluto, config$percentil_relativo)
  if (length(percentis) != 2L || any(!is.finite(percentis)) ||
      any(percentis <= 0 | percentis >= 1)) {
    stop("Os percentis da priorização devem ser números entre 0 e 1.", call. = FALSE)
  }
  if (length(config$minimo_problemas_prioridade_3) != 1L ||
      !is.finite(config$minimo_problemas_prioridade_3) ||
      config$minimo_problemas_prioridade_3 < 1) {
    stop("O mínimo de problemas da Prioridade 3 deve ser positivo.", call. = FALSE)
  }
  if (!config$metodo_quantile_r %in% 1:9) {
    stop("O método de quantil do R deve ser um inteiro entre 1 e 9.", call. = FALSE)
  }
  if (!config$transformacao_x %in% c("pseudo_log", "log1p", "sqrt", "linear") ||
      !config$transformacao_y %in% c("pseudo_log", "log1p", "sqrt", "linear")) {
    stop("Transformação da matriz desconhecida.", call. = FALSE)
  }
  validar_escala_matriz_prioridade(config$escala_padrao)
  categorias_necessarias <- c(
    "prioridade_1", "prioridade_2", "prioridade_3",
    "monitoramento", "sem_problemas"
  )
  if (!all(categorias_necessarias %in% names(config$categorias))) {
    stop("A configuração não contém todas as categorias de prioridade.", call. = FALSE)
  }
  invisible(config)
}

# Normaliza a escolha de visualização usada no HTML e na figura do PDF. Esta
# função não transforma dados nem recalcula a priorização.
validar_escala_matriz_prioridade <- function(escala) {
  if (is.null(escala) || !length(escala) || is.na(escala[[1]])) {
    escala <- "transformada"
  }
  escala <- as.character(escala[[1]])
  if (!escala %in% c("original", "transformada")) {
    stop("A escala da matriz deve ser 'original' ou 'transformada'.", call. = FALSE)
  }
  escala
}

# Calcula a posição percentual empírica usando a proporção de valores de
# referência menores ou iguais ao observado. Empates recebem a mesma posição e
# o maior valor alcança 100. Valores fora do universo recebem zero.
posicao_percentual_empirica <- function(valores, universo) {
  saida <- rep(0, length(valores))
  referencia <- valores[universo & is.finite(valores)]
  if (!length(referencia)) return(saida)
  indices <- which(universo & is.finite(valores))
  saida[indices] <- vapply(
    valores[indices], function(valor) 100 * sum(referencia <= valor) / length(referencia),
    numeric(1)
  )
  saida
}

# Aplica os cortes P95 e as cinco categorias. O universo dos percentis contém
# apenas localidades com ao menos um problema. `quantile(..., type = 7)` é o
# padrão documentado do R e pode ser alterado somente pela configuração.
classificar_prioridades <- function(
    tabela, config = configuracao_priorizacao(),
    coluna_absoluta = "registros_invalidos",
    coluna_relativa = "percentual_invalidos") {
  validar_configuracao_priorizacao(config)
  if (!all(c(coluna_absoluta, coluna_relativa) %in% names(tabela))) {
    stop("A tabela não contém os indicadores absoluto e relativo da prioridade.",
         call. = FALSE)
  }
  saida <- tabela
  absoluto <- suppressWarnings(as.numeric(saida[[coluna_absoluta]]))
  relativo <- suppressWarnings(as.numeric(saida[[coluna_relativa]]))
  absoluto[!is.finite(absoluto) | absoluto < 0] <- 0
  relativo[!is.finite(relativo) | relativo < 0] <- 0
  universo <- absoluto > 0

  quantil_seguro <- function(valores, probabilidade) {
    valores <- valores[universo & is.finite(valores)]
    if (!length(valores)) return(0)
    as.numeric(stats::quantile(
      valores, probs = probabilidade, na.rm = TRUE, names = FALSE,
      type = as.integer(config$metodo_quantile_r)
    ))
  }
  limite_absoluto <- quantil_seguro(absoluto, config$percentil_absoluto)
  limite_absoluto2 <- quantil_seguro(absoluto, config$percentil_absoluto2)
  limite_relativo <- quantil_seguro(relativo, config$percentil_relativo)
  posicao_absoluta <- posicao_percentual_empirica(absoluto, universo)
  posicao_relativa <- posicao_percentual_empirica(relativo, universo)

  alto_absoluto <- universo & absoluto >= limite_absoluto
  alto_absoluto2 <- universo & absoluto >= limite_absoluto2
  alto_relativo <- universo & relativo >= limite_relativo
  minimo_p3 <- as.numeric(config$minimo_problemas_prioridade_3)

  id <- rep("sem_problemas", nrow(saida))
  id[universo] <- "monitoramento"
  id[universo & !alto_absoluto & alto_relativo & absoluto >= minimo_p3] <-
    "prioridade_3"
  id[alto_absoluto & !alto_relativo] <- "prioridade_2"
  id[(alto_absoluto & alto_relativo) | alto_absoluto2] <- "prioridade_1"

  nomes <- vapply(config$categorias, `[[`, character(1), "nome")
  justificativas <- c(
    prioridade_1 = "Valor absoluto e percentual iguais ou superiores aos respectivos P95, ou valor absoluro igual ou superio ao P98.",
    prioridade_2 = "Valor absoluto igual ou superior ao P95 e percentual abaixo do P95.",
    prioridade_3 = sprintf(
      "Percentual igual ou superior ao P95, absoluto abaixo do P95 e pelo menos %d problemas.",
      as.integer(minimo_p3)
    ),
    monitoramento = "Localidade com problemas que não atende aos cortes de prioridade.",
    sem_problemas = "Nenhum registro problemático."
  )
  ordem <- c(
    prioridade_1 = 1L, prioridade_2 = 2L, prioridade_3 = 3L,
    monitoramento = 4L, sem_problemas = 5L
  )

  saida[[coluna_absoluta]] <- absoluto
  saida[[coluna_relativa]] <- relativo
  saida$limiar_absoluto_p95 <- rep(limite_absoluto, nrow(saida))
  saida$limiar_relativo_p95 <- rep(limite_relativo, nrow(saida))
  saida$percentil_absoluto <- posicao_absoluta
  saida$percentil_relativo <- posicao_relativa
  saida$prioridade_id <- id
  saida$prioridade <- unname(nomes[id])
  saida$ordem_prioridade <- unname(ordem[id])
  saida$justificativa_prioridade <- unname(justificativas[id])
  if (!"contribuicao_invalidos" %in% names(saida)) {
    total <- sum(absoluto, na.rm = TRUE)
    saida$contribuicao_invalidos <- if (total > 0) 100 * absoluto / total else 0
  }
  saida
}

# Insere nos dois relatórios a mesma descrição auditável do modelo. Manter este
# texto junto ao algoritmo evita que Validade e Consistência documentem regras
# diferentes depois de uma futura alteração dos cortes.
bloco_metodologia_priorizacao <- function() {
  config <- configuracao_priorizacao()
  htmltools::tagList(
    htmltools::tags$h3("Matriz de prioridade territorial"),
    htmltools::tags$p(sprintf(
      paste0(
        "Os limites absoluto e relativo são os percentis %.0f calculados, ",
        "separadamente em cada universo territorial exibido, somente entre ",
        "localidades com pelo menos um registro problemático. Os quantis usam ",
        "o método %d do R. Localidades sem problemas permanecem na tabela, mas ",
        "não interferem nos cortes."
      ),
      100 * config$percentil_absoluto, as.integer(config$metodo_quantile_r)
    )),
    htmltools::tags$ul(
      htmltools::tags$li(
        "Prioridade 1 — crítica: absoluto ≥ P95 e relativo ≥ P95."
      ),
      htmltools::tags$li(
        "Prioridade 2 — alto impacto: absoluto ≥ P95 e relativo < P95."
      ),
      htmltools::tags$li(sprintf(
        paste0(
          "Prioridade 3 — atenção relativa: absoluto < P95, relativo ≥ P95 ",
          "e pelo menos %d registros problemáticos."
        ),
        as.integer(config$minimo_problemas_prioridade_3)
      )),
      htmltools::tags$li(
        "Monitoramento: demais localidades com pelo menos um problema."
      ),
      htmltools::tags$li("Sem problemas: nenhum registro problemático.")
    ),
    htmltools::tags$p(sprintf(
      paste0(
        "O volume total de notificações não participa da classificação: ele é ",
        "mostrado apenas como contexto. Os eixos usam transformação %s no ",
        "absoluto e %s no percentual para dispersar visualmente valores muito ",
        "assimétricos; a transformação não altera valores, percentis nem ",
        "categorias. HTML e PDF são derivados da mesma tabela classificada."
      ),
      config$transformacao_x, config$transformacao_y
    ))
  )
}

# Anexa regra e grupo dominantes quando essas informações já foram produzidas
# pelo relatório. A ausência de uma tabela dominante nunca impede a matriz.
anexar_dominantes_prioridade <- function(
    tabela, coluna_codigo, regras = NULL, grupos = NULL) {
  tabela$regra_dominante_prioridade <- NA_character_
  tabela$grupo_dominante_prioridade <- NA_character_
  if (!is.null(regras) && nrow(regras) && coluna_codigo %in% names(regras) &&
      "regra_dominante" %in% names(regras)) {
    indice <- match(as.character(tabela[[coluna_codigo]]),
                    as.character(regras[[coluna_codigo]]))
    tabela$regra_dominante_prioridade <- as.character(regras$regra_dominante[indice])
  }
  if (!is.null(grupos) && nrow(grupos) && coluna_codigo %in% names(grupos) &&
      "grupo_dominante" %in% names(grupos)) {
    indice <- match(as.character(tabela[[coluna_codigo]]),
                    as.character(grupos[[coluna_codigo]]))
    tabela$grupo_dominante_prioridade <- as.character(grupos$grupo_dominante[indice])
  }
  tabela
}

# Prepara nomes, códigos, dominantes e ordenação para que gráfico, tooltip e
# tabela utilizem exatamente a mesma classificação.
preparar_tabela_prioridades <- function(
    tabela, coluna_nome, coluna_codigo, regras_dominantes = NULL,
    grupos_dominantes = NULL, config = configuracao_priorizacao()) {
  if (!all(c(coluna_nome, coluna_codigo) %in% names(tabela))) {
    stop("Nome ou código territorial não encontrado para a matriz.", call. = FALSE)
  }
  dados <- classificar_prioridades(tabela, config)
  dados$nome_prioridade <- as.character(dados[[coluna_nome]])
  dados$codigo_prioridade <- as.character(dados[[coluna_codigo]])
  vazio <- is.na(dados$nome_prioridade) | !nzchar(trimws(dados$nome_prioridade))
  dados$nome_prioridade[vazio] <- ifelse(
    is.na(dados$codigo_prioridade[vazio]) | !nzchar(dados$codigo_prioridade[vazio]),
    "Localidade não identificada", paste("Código", dados$codigo_prioridade[vazio])
  )
  dados <- anexar_dominantes_prioridade(
    dados, coluna_codigo, regras_dominantes, grupos_dominantes
  )
  dados <- dados[order(
    dados$ordem_prioridade, -dados$registros_invalidos,
    -dados$percentual_invalidos, dados$nome_prioridade, na.last = TRUE
  ), , drop = FALSE]
  rownames(dados) <- NULL
  dados
}

# Formata números para o JSON consumido pelo SVG interativo. Os valores brutos
# também permanecem na tabela R usada pelo gráfico estático e pelos testes.
dados_json_matriz_prioridade <- function(
    dados, titulo, modulo,
    escala_inicial = configuracao_priorizacao()$escala_padrao) {
  config <- configuracao_priorizacao()
  escala_inicial <- validar_escala_matriz_prioridade(escala_inicial)
  categorias <- lapply(config$categorias, function(x) {
    list(nome = x$nome, cor = x$cor, forma = x$forma)
  })
  linhas <- data.frame(
    nome = dados$nome_prioridade,
    codigo = dados$codigo_prioridade,
    notificacoes = as.numeric(dados$notificacoes),
    problemas = as.numeric(dados$registros_invalidos),
    percentual = as.numeric(dados$percentual_invalidos),
    percentil_absoluto = as.numeric(dados$percentil_absoluto),
    percentil_relativo = as.numeric(dados$percentil_relativo),
    contribuicao = as.numeric(dados$contribuicao_invalidos),
    prioridade_id = dados$prioridade_id,
    prioridade = dados$prioridade,
    justificativa = dados$justificativa_prioridade,
    regra_dominante = dados$regra_dominante_prioridade,
    grupo_dominante = dados$grupo_dominante_prioridade,
    stringsAsFactors = FALSE
  )
  list(
    titulo = titulo,
    modulo = modulo,
    dados = linhas,
    limites = list(
      absoluto = if (nrow(dados)) dados$limiar_absoluto_p95[[1]] else 0,
      relativo = if (nrow(dados)) dados$limiar_relativo_p95[[1]] else 0,
      minimo_prioridade_3 = config$minimo_problemas_prioridade_3
    ),
    visual = list(
      escala_inicial = escala_inicial,
      transformacao_x = config$transformacao_x,
      transformacao_y = config$transformacao_y,
      sigma_x = config$sigma_x,
      sigma_y = config$sigma_y,
      tamanho_minimo = config$pontos$tamanho_minimo,
      tamanho_maximo = config$pontos$tamanho_maximo,
      transparencia_padrao = config$pontos$transparencia_padrao,
      transparencia_monitoramento = config$pontos$transparencia_monitoramento,
      rotulos_prioridade_1 = config$rotulos_prioridade_1,
      categorias = categorias
    )
  )
}

# Produz uma tabela HTML completa, ordenada por categoria e preparada para os
# filtros de prioridade e busca implementados pelo JavaScript incorporado.
tabela_html_prioridades <- function(dados, modulo) {
  rotulo_problemas <- if (identical(modulo, "consistencia")) {
    "Inconsistentes"
  } else "Inválidos"
  linhas <- lapply(seq_len(nrow(dados)), function(i) {
    linha <- dados[i, , drop = FALSE]
    valores <- c(
      linha$nome_prioridade,
      linha$codigo_prioridade,
      formatar_inteiro_relatorio(linha$notificacoes),
      formatar_inteiro_relatorio(linha$registros_invalidos),
      formatar_percentual_relatorio(linha$percentual_invalidos),
      formatar_percentual_relatorio(linha$percentil_absoluto),
      formatar_percentual_relatorio(linha$percentil_relativo),
      linha$prioridade,
      linha$justificativa_prioridade
    )
    htmltools::tags$tr(
      `data-prioridade` = linha$prioridade_id,
      `data-texto` = tolower(paste(valores, collapse = " ")),
      lapply(valores, function(valor) htmltools::tags$td(
        if (is.na(valor) || !nzchar(as.character(valor))) "—" else valor
      ))
    )
  })
  htmltools::tags$details(
    class = "detalhes-tabela-prioridade",
    htmltools::tags$summary("Consultar tabela completa de prioridades"),
    htmltools::tags$div(
      class = "filtros-tabela-prioridade somente-tela",
      htmltools::tags$label(
        "Categoria ",
        htmltools::tags$select(
          class = "filtro-categoria-prioridade",
          htmltools::tags$option(value = "todos", "Todas"),
          lapply(names(configuracao_priorizacao()$categorias), function(id) {
            htmltools::tags$option(
              value = id, configuracao_priorizacao()$categorias[[id]]$nome
            )
          })
        )
      ),
      htmltools::tags$label(
        "Buscar ",
        htmltools::tags$input(
          type = "search", class = "busca-tabela-prioridade",
          placeholder = "Nome ou código"
        )
      )
    ),
    htmltools::tags$div(
      class = "tabela-rolagem",
      htmltools::tags$table(
        class = "tabela-relatorio tabela-prioridades",
        htmltools::tags$thead(htmltools::tags$tr(lapply(
          c("Localidade", "Código", "Notificações", rotulo_problemas,
            "% problemático", "Percentil absoluto", "Percentil relativo",
            "Prioridade", "Justificativa"),
          htmltools::tags$th
        ))),
        htmltools::tags$tbody(linhas)
      )
    )
  )
}

# Monta, a partir de uma única tabela classificada, a versão interativa para a
# tela, a versão estática para impressão e a tabela consultável.
bloco_matriz_prioridade_relatorio <- function(
    tabela, coluna_nome, coluna_codigo, titulo, modulo = c("validade", "consistencia"),
    regras_dominantes = NULL, grupos_dominantes = NULL,
    escala_matriz = configuracao_priorizacao()$escala_padrao) {
  modulo <- match.arg(modulo)
  escala_matriz <- validar_escala_matriz_prioridade(escala_matriz)
  dados <- preparar_tabela_prioridades(
    tabela, coluna_nome, coluna_codigo, regras_dominantes, grupos_dominantes
  )
  if (!nrow(dados)) {
    return(htmltools::tags$p(class = "sem-dados", "Sem dados para a matriz de prioridade."))
  }
  carga <- dados_json_matriz_prioridade(dados, titulo, modulo, escala_matriz)
  id <- paste0(
    "matriz-", gsub("[^a-z0-9]+", "-", tolower(iconv(titulo, to = "ASCII//TRANSLIT"))),
    "-", sum(utf8ToInt(titulo))
  )
  config <- configuracao_priorizacao()
  dimensoes <- dimensoes_figura_relatorio("matriz_prioridade")
  sem_problemas <- sum(dados$prioridade_id == "sem_problemas")
  htmltools::tags$div(
    class = "bloco-matriz-prioridade",
    id = id,
    htmltools::tags$div(
      class = "matriz-interativa somente-tela",
      htmltools::tags$div(
        class = "controles-matriz-prioridade",
        htmltools::tags$button(type = "button", class = "mostrar-prioridades",
                               "Mostrar somente prioridades"),
        htmltools::tags$button(type = "button", class = "mostrar-todos", "Mostrar todos"),
        htmltools::tags$button(type = "button", class = "restaurar-zoom", "Restaurar zoom"),
        htmltools::tags$span(class = "separador-controles-matriz", "Escala dos eixos:"),
        htmltools::tags$button(
          type = "button", class = "selecionar-escala-matriz",
          `data-escala` = "original", "Original"
        ),
        htmltools::tags$button(
          type = "button", class = "selecionar-escala-matriz",
          `data-escala` = "transformada", "Transformada"
        )
      ),
      htmltools::tags$div(class = "legenda-interativa-prioridade"),
      htmltools::tags$div(
        class = "area-svg-prioridade",
        htmltools::tags$svg(
          class = "svg-matriz-prioridade", role = "img",
          `aria-label` = titulo, viewBox = "0 0 1000 620"
        ),
        htmltools::tags$div(class = "tooltip-prioridade", role = "status")
      ),
      htmltools::tags$script(
        type = "application/json", class = "dados-matriz-prioridade",
        htmltools::HTML(jsonlite::toJSON(
          carga, auto_unbox = TRUE, na = "null", null = "null", digits = 12
        ))
      )
    ),
    # O contêiner próprio impede que navegadores dividam o PNG entre páginas.
    htmltools::tags$div(
      class = "quadro-matriz-estatica-impressao",
      imagem_grafico_relatorio(
        grafico_matriz_prioridade(
          dados, "nome_prioridade", titulo, modulo, escala = escala_matriz
        ),
        paste("Matriz de prioridade", titulo),
        largura = dimensoes$largura, altura = dimensoes$altura,
        dpi = dimensoes$dpi,
        classes = "grafico-relatorio matriz-estatica-impressao"
      )
    ),
    htmltools::tags$p(
      class = "observacao nota-matriz-prioridade",
      sprintf(
        paste0(
          "Cortes: P%.0f absoluto = %s; P%.0f relativo = %s; ",
          "Prioridade 3 exige ao menos %d registros problemáticos. ",
          "%d localidade(s) sem problemas não aparecem nos pontos. ",
          "O HTML contém as escalas original e transformada já prontas; os botões apenas alternam a camada visível. ",
          "A figura do PDF usa a escala %s selecionada antes da geração."
        ),
        100 * config$percentil_absoluto,
        format(
          round(dados$limiar_absoluto_p95[[1]], 2), nsmall = 2,
          big.mark = ".", decimal.mark = ",", trim = TRUE, scientific = FALSE
        ),
        100 * config$percentil_relativo,
        formatar_percentual_relatorio(dados$limiar_relativo_p95[[1]]),
        config$minimo_problemas_prioridade_3, sem_problemas, escala_matriz
      )
    ),
    tabela_html_prioridades(dados, modulo)
  )
}

# JavaScript único compartilhado por todas as matrizes do documento. O desenho
# usa apenas SVG nativo: não baixa bibliotecas, funciona offline e evita repetir
# uma dependência grande em cada gráfico. Zoom, arraste, tooltip, legenda e
# filtros são inicializados a partir do JSON incorporado em cada bloco.
javascript_matrizes_prioridade <- function() {
  "
(function() {
  'use strict';
  var NS = 'http://www.w3.org/2000/svg';

  function elemento(tag, atributos, texto) {
    var no = document.createElementNS(NS, tag);
    Object.keys(atributos || {}).forEach(function(chave) {
      no.setAttribute(chave, atributos[chave]);
    });
    if (texto !== undefined) no.textContent = texto;
    return no;
  }

  function numero(valor, casas) {
    if (valor === null || valor === undefined || !isFinite(valor)) return '—';
    return Number(valor).toLocaleString('pt-BR', {
      minimumFractionDigits: casas || 0,
      maximumFractionDigits: casas || 0
    });
  }

  function transformar(valor, tipo, sigma) {
    valor = Math.max(0, Number(valor) || 0);
    if (tipo === 'pseudo_log') return Math.asinh(valor / (2 * Math.max(sigma, 1e-9))) / Math.LN10;
    if (tipo === 'log1p') return Math.log1p(valor);
    if (tipo === 'sqrt') return Math.sqrt(valor);
    return valor;
  }

  function passos(maximo, percentual) {
    if (!(maximo > 0)) return [0];
    var candidatos = percentual ?
      [0, .1, .25, .5, 1, 2, 5, 10, 20, 40, 60, 80, 100] :
      [0, 1, 2, 5, 10, 20, 50, 100, 200, 500, 1000, 2000, 5000, 10000, 20000, 50000, 100000];
    var validos = candidatos.filter(function(x) { return x <= maximo; });
    if (validos[validos.length - 1] < maximo) validos.push(maximo);
    if (validos.length <= 8) return validos;
    var saida = [];
    for (var i = 0; i < 8; i++) {
      saida.push(validos[Math.round(i * (validos.length - 1) / 7)]);
    }
    return saida.filter(function(x, i, a) { return i === 0 || x !== a[i - 1]; });
  }

  // Cria marcas regulares para a escala original. A escala transformada
  // continua usando `passos`, preservando sua distribuição atual.
  function passosLineares(maximo, quantidade) {
    if (!(maximo > 0)) return [0];
    var bruto = maximo / Math.max(2, quantidade || 6);
    var potencia = Math.pow(10, Math.floor(Math.log(bruto) / Math.LN10));
    var proporcao = bruto / potencia;
    var unidade = proporcao <= 1 ? 1 : proporcao <= 2 ? 2 : proporcao <= 5 ? 5 : 10;
    var intervalo = unidade * potencia;
    var saida = [];
    for (var valor = 0; valor <= maximo + intervalo * 1e-9; valor += intervalo) {
      saida.push(Number(valor.toPrecision(12)));
    }
    return saida;
  }

  function iniciarTabela(raiz) {
    var seletor = raiz.querySelector('.filtro-categoria-prioridade');
    var busca = raiz.querySelector('.busca-tabela-prioridade');
    var linhas = Array.prototype.slice.call(raiz.querySelectorAll('.tabela-prioridades tbody tr'));
    if (!seletor || !busca) return;
    function filtrar() {
      var categoria = seletor.value;
      var termo = busca.value.trim().toLowerCase();
      linhas.forEach(function(linha) {
        var categoriaOk = categoria === 'todos' || linha.dataset.prioridade === categoria;
        var textoOk = !termo || (linha.dataset.texto || '').indexOf(termo) >= 0;
        linha.style.display = categoriaOk && textoOk ? '' : 'none';
      });
    }
    seletor.addEventListener('change', filtrar);
    busca.addEventListener('input', filtrar);
  }

  function iniciarMatriz(raiz) {
    var script = raiz.querySelector('.dados-matriz-prioridade');
    var svg = raiz.querySelector('.svg-matriz-prioridade');
    if (!script || !svg) return;
    var carga = JSON.parse(script.textContent);
    var todos = carga.dados || [];
    var dados = todos.filter(function(d) { return Number(d.problemas) > 0; });
    var visual = carga.visual;
    var limites = carga.limites;
    var largura = 1000, altura = 620;
    var margem = { esquerda: 88, direita: 32, topo: 58, inferior: 78 };
    var areaW = largura - margem.esquerda - margem.direita;
    var areaH = altura - margem.topo - margem.inferior;
    var maxX = Math.max.apply(null, dados.map(function(d) { return Number(d.problemas) || 0; })
      .concat([1]));
    var maxY = Math.max.apply(null, dados.map(function(d) { return Number(d.percentual) || 0; })
      .concat([1]));
    maxX *= 1.08; maxY = Math.min(100, maxY * 1.08);
    var txMax = transformar(maxX, visual.transformacao_x, visual.sigma_x);
    var tyMax = transformar(maxY, visual.transformacao_y, visual.sigma_y);
    function x(valor) {
      return margem.esquerda + areaW * transformar(valor, visual.transformacao_x, visual.sigma_x) / Math.max(txMax, 1e-9);
    }
    function y(valor) {
      return margem.topo + areaH * (1 - transformar(valor, visual.transformacao_y, visual.sigma_y) / Math.max(tyMax, 1e-9));
    }
    function xOriginal(valor) {
      return margem.esquerda + areaW * (Number(valor) || 0) / Math.max(maxX, 1e-9);
    }
    function yOriginal(valor) {
      return margem.topo + areaH * (1 - (Number(valor) || 0) / Math.max(maxY, 1e-9));
    }

    while (svg.firstChild) svg.removeChild(svg.firstChild);
    var fundo = elemento('rect', { x: 0, y: 0, width: largura, height: altura, fill: '#ffffff' });
    svg.appendChild(fundo);
    var camada = elemento('g', {
      'class': 'camada-zoom-prioridade camada-escala-prioridade',
      'data-escala': 'transformada'
    });
    svg.appendChild(camada);
    camada.appendChild(elemento('text', {
      x: margem.esquerda, y: 30, 'font-size': 21, 'font-weight': 700, fill: '#471E82'
    }, carga.titulo));

    passos(maxX, false).forEach(function(valor) {
      var px = x(valor);
      camada.appendChild(elemento('line', {
        x1: px, y1: margem.topo, x2: px, y2: margem.topo + areaH,
        stroke: '#E5E7EB', 'stroke-width': 1, 'data-valor-x': valor,
        'class': 'marca-escala-prioridade'
      }));
      camada.appendChild(elemento('text', {
        x: px, y: margem.topo + areaH + 22, 'text-anchor': 'middle',
        'font-size': 12, fill: '#6B6472', 'data-valor-x': valor,
        'class': 'marca-escala-prioridade'
      }, numero(valor, 0)));
    });
    passos(maxY, true).forEach(function(valor) {
      var py = y(valor);
      camada.appendChild(elemento('line', {
        x1: margem.esquerda, y1: py, x2: margem.esquerda + areaW, y2: py,
        stroke: '#E5E7EB', 'stroke-width': 1, 'data-valor-y': valor,
        'class': 'marca-escala-prioridade'
      }));
      camada.appendChild(elemento('text', {
        x: margem.esquerda - 10, y: py + 4, 'text-anchor': 'end',
        'font-size': 12, fill: '#6B6472', 'data-valor-y': valor,
        'data-deslocamento-y': 4, 'class': 'marca-escala-prioridade'
      }, numero(valor, valor < 1 ? 2 : 0) + '%'));
    });
    camada.appendChild(elemento('line', {
      x1: margem.esquerda, y1: margem.topo + areaH,
      x2: margem.esquerda + areaW, y2: margem.topo + areaH,
      stroke: '#374151', 'stroke-width': 1.2
    }));
    camada.appendChild(elemento('line', {
      x1: margem.esquerda, y1: margem.topo,
      x2: margem.esquerda, y2: margem.topo + areaH,
      stroke: '#374151', 'stroke-width': 1.2
    }));
    camada.appendChild(elemento('text', {
      x: margem.esquerda + areaW / 2, y: altura - 22,
      'text-anchor': 'middle', 'font-size': 14, fill: '#2C2733'
    }, 'Registros problemáticos'));
    var tituloY = carga.modulo === 'consistencia' ? 'Registros inconsistentes (%)' : 'Registros inválidos (%)';
    camada.appendChild(elemento('text', {
      x: 20, y: margem.topo + areaH / 2, transform: 'rotate(-90 20 ' + (margem.topo + areaH / 2) + ')',
      'text-anchor': 'middle', 'font-size': 14, fill: '#2C2733'
    }, tituloY));

    var maxProblemas = Math.max.apply(null, dados.map(function(d) { return Number(d.problemas) || 0; }).concat([1]));
    var minR = Number(visual.tamanho_minimo) || 3.5;
    var maxR = Number(visual.tamanho_maximo) || 11;
    function raio(valor) {
      return minR + (maxR - minR) * Math.sqrt(Number(valor) || 0) / Math.sqrt(maxProblemas);
    }
    var tooltip = raiz.querySelector('.tooltip-prioridade');
    var elementosPontos = [];
    var manipuladoresTooltip = [];
    dados.forEach(function(d, indiceDado) {
      var categoria = visual.categorias[d.prioridade_id];
      var r = raio(d.problemas), px = x(d.problemas), py = y(d.percentual);
      // Todas as categorias usam a mesma forma. Cor e transparência são os
      // únicos atributos categóricos; a área da bolha representa problemas.
      var forma = elemento('circle', {
        cx: px, cy: py, r: r, 'data-valor-x': d.problemas,
        'data-valor-y': d.percentual, 'data-indice-dado': indiceDado
      });
      forma.setAttribute('fill', categoria.cor);
      forma.setAttribute('stroke', d.prioridade_id === 'prioridade_1' ? '#111827' : '#ffffff');
      forma.setAttribute('stroke-width', d.prioridade_id === 'prioridade_1' ? 1.8 : 1);
      forma.setAttribute('opacity', d.prioridade_id === 'monitoramento' ?
        visual.transparencia_monitoramento : visual.transparencia_padrao);
      forma.setAttribute('class', 'ponto-prioridade');
      forma.setAttribute('data-prioridade', d.prioridade_id);
      forma.setAttribute('tabindex', '0');
      forma.setAttribute('aria-label', d.nome + ': ' + d.prioridade);
      function mostrarTooltip(evento) {
        var dominante = '';
        if (d.grupo_dominante) dominante += '<br><strong>Grupo dominante:</strong> ' + d.grupo_dominante;
        if (d.regra_dominante) dominante += '<br><strong>Regra dominante:</strong> ' + d.regra_dominante;
        tooltip.innerHTML = '<strong>' + d.nome + '</strong><br>' +
          '<strong>Código:</strong> ' + (d.codigo || '—') + '<br>' +
          '<strong>Notificações:</strong> ' + numero(d.notificacoes, 0) + '<br>' +
          '<strong>Registros problemáticos:</strong> ' + numero(d.problemas, 0) + '<br>' +
          '<strong>Percentual:</strong> ' + numero(d.percentual, 2) + '%<br>' +
          '<strong>Percentil absoluto:</strong> ' + numero(d.percentil_absoluto, 2) + '<br>' +
          '<strong>Percentil relativo:</strong> ' + numero(d.percentil_relativo, 2) + '<br>' +
          '<strong>Cortes:</strong> ' + numero(limites.absoluto, 2) + ' e ' + numero(limites.relativo, 2) + '%<br>' +
          '<strong>Contribuição:</strong> ' + numero(d.contribuicao, 2) + '%<br>' +
          '<strong>' + d.prioridade + '</strong><br>' + d.justificativa + dominante;
        tooltip.style.display = 'block';
        // A caixa pertence à área do SVG; usar essa mesma área como referência
        // elimina o deslocamento causado pelos botões e pela legenda acima.
        var area = raiz.querySelector('.area-svg-prioridade');
        var caixa = area.getBoundingClientRect();
        var cx = evento.clientX || caixa.left + caixa.width / 2;
        var cy = evento.clientY || caixa.top + caixa.height / 2;
        var esquerda = Math.max(4, Math.min(cx - caixa.left + 9, caixa.width - 326));
        var topoAbaixo = cy - caixa.top + 9;
        var alturaTooltip = tooltip.offsetHeight || 160;
        var topo = topoAbaixo + alturaTooltip <= caixa.height - 4 ?
          topoAbaixo : Math.max(4, cy - caixa.top - alturaTooltip - 9);
        tooltip.style.left = esquerda + 'px';
        tooltip.style.top = topo + 'px';
      }
      function ocultarTooltip() { tooltip.style.display = 'none'; }
      forma.addEventListener('pointerenter', mostrarTooltip);
      forma.addEventListener('pointermove', mostrarTooltip);
      forma.addEventListener('focus', mostrarTooltip);
      forma.addEventListener('pointerleave', ocultarTooltip);
      forma.addEventListener('blur', ocultarTooltip);
      manipuladoresTooltip.push({ mostrar: mostrarTooltip, ocultar: ocultarTooltip });
      camada.appendChild(forma);
      elementosPontos.push(forma);
    });

    // Escolhe o lado com espaço suficiente e inverte a posição vertical perto
    // do topo. Assim nenhum nome crítico ultrapassa o retângulo de plotagem.
    function posicionarRotulo(px, py, texto) {
      var larguraTexto = Math.min(210, Math.max(30, texto.length * 6.2));
      var direita = margem.esquerda + areaW - px;
      var esquerda = px - margem.esquerda;
      var posicaoX, ancora;
      if (direita >= larguraTexto + 10) {
        posicaoX = px + 8; ancora = 'start';
      } else if (esquerda >= larguraTexto + 10) {
        posicaoX = px - 8; ancora = 'end';
      } else {
        posicaoX = margem.esquerda + 4; ancora = 'start';
      }
      var posicaoY = py - 8;
      if (posicaoY < margem.topo + 12) posicaoY = py + 17;
      posicaoY = Math.max(margem.topo + 12, Math.min(margem.topo + areaH - 5, posicaoY));
      return { x: posicaoX, y: posicaoY, ancora: ancora };
    }

    dados.filter(function(d) { return d.prioridade_id === 'prioridade_1'; })
      .sort(function(a, b) { return b.problemas - a.problemas; })
      .slice(0, Number(visual.rotulos_prioridade_1) || 12)
      .forEach(function(d) {
        var texto = d.nome.length > 34 ? d.nome.slice(0, 31) + '…' : d.nome;
        var posicao = posicionarRotulo(x(d.problemas), y(d.percentual), texto);
        var rotulo = elemento('text', {
          x: posicao.x, y: posicao.y, 'text-anchor': posicao.ancora,
          'font-size': 11, fill: '#2C2733', 'data-prioridade': d.prioridade_id,
          'class': 'rotulo-ponto-prioridade', 'data-valor-x': d.problemas,
          'data-valor-y': d.percentual
        }, texto);
        camada.appendChild(rotulo);
        elementosPontos.push(rotulo);
      });

    // A segunda camada é criada uma única vez. Ao clicar nos botões de escala,
    // o navegador apenas alterna `display`; não há novo cálculo nem novo SVG.
    var camadaOriginal = camada.cloneNode(true);
    camadaOriginal.setAttribute('data-escala', 'original');
    camadaOriginal.style.display = 'none';
    Array.prototype.forEach.call(
      camadaOriginal.querySelectorAll('.marca-escala-prioridade'),
      function(no) { no.remove(); }
    );
    var marcasLineares = elemento('g', { 'class': 'marcas-lineares-prioridade' });
    passosLineares(maxX, 6).forEach(function(valor) {
      var px = xOriginal(valor);
      marcasLineares.appendChild(elemento('line', {
        x1: px, y1: margem.topo, x2: px, y2: margem.topo + areaH,
        stroke: '#E5E7EB', 'stroke-width': 1
      }));
      marcasLineares.appendChild(elemento('text', {
        x: px, y: margem.topo + areaH + 22, 'text-anchor': 'middle',
        'font-size': 12, fill: '#6B6472'
      }, numero(valor, 0)));
    });
    passosLineares(maxY, 5).forEach(function(valor) {
      var py = yOriginal(valor);
      marcasLineares.appendChild(elemento('line', {
        x1: margem.esquerda, y1: py, x2: margem.esquerda + areaW, y2: py,
        stroke: '#E5E7EB', 'stroke-width': 1
      }));
      marcasLineares.appendChild(elemento('text', {
        x: margem.esquerda - 10, y: py + 4, 'text-anchor': 'end',
        'font-size': 12, fill: '#6B6472'
      }, numero(valor, valor < 1 ? 2 : 0) + '%'));
    });
    camadaOriginal.insertBefore(marcasLineares, camadaOriginal.children[1] || null);
    Array.prototype.forEach.call(camadaOriginal.querySelectorAll('[data-valor-x]'), function(no) {
      if (no.classList.contains('rotulo-ponto-prioridade')) return;
      var posicao = xOriginal(no.getAttribute('data-valor-x')) +
        Number(no.getAttribute('data-deslocamento-x') || 0);
      ['x', 'x1', 'x2', 'cx'].forEach(function(atributo) {
        if (no.hasAttribute(atributo)) no.setAttribute(atributo, posicao);
      });
    });
    Array.prototype.forEach.call(camadaOriginal.querySelectorAll('[data-valor-y]'), function(no) {
      if (no.classList.contains('rotulo-ponto-prioridade')) return;
      var posicao = yOriginal(no.getAttribute('data-valor-y')) +
        Number(no.getAttribute('data-deslocamento-y') || 0);
      ['y', 'y1', 'y2', 'cy'].forEach(function(atributo) {
        if (no.hasAttribute(atributo)) no.setAttribute(atributo, posicao);
      });
    });
    Array.prototype.forEach.call(
      camadaOriginal.querySelectorAll('.rotulo-ponto-prioridade'), function(rotulo) {
        var posicao = posicionarRotulo(
          xOriginal(rotulo.getAttribute('data-valor-x')),
          yOriginal(rotulo.getAttribute('data-valor-y')),
          rotulo.textContent || ''
        );
        rotulo.setAttribute('x', posicao.x);
        rotulo.setAttribute('y', posicao.y);
        rotulo.setAttribute('text-anchor', posicao.ancora);
      }
    );
    Array.prototype.forEach.call(
      camadaOriginal.querySelectorAll('.ponto-prioridade'), function(ponto) {
        var manipulador = manipuladoresTooltip[Number(ponto.getAttribute('data-indice-dado'))];
        ponto.addEventListener('pointerenter', manipulador.mostrar);
        ponto.addEventListener('pointermove', manipulador.mostrar);
        ponto.addEventListener('focus', manipulador.mostrar);
        ponto.addEventListener('pointerleave', manipulador.ocultar);
        ponto.addEventListener('blur', manipulador.ocultar);
      }
    );
    svg.appendChild(camadaOriginal);
    Array.prototype.push.apply(
      elementosPontos,
      Array.prototype.slice.call(camadaOriginal.querySelectorAll(
        '.ponto-prioridade,.rotulo-ponto-prioridade'
      ))
    );

    // Hover produz realce temporário. Clique fixa o realce até um segundo
    // clique na mesma bolha ou até a seleção de outra localidade.
    var indiceFixado = null;
    var pontosInterativos = Array.prototype.slice.call(
      svg.querySelectorAll('.ponto-prioridade')
    );
    function aplicarDestaque(indice) {
      pontosInterativos.forEach(function(ponto) {
        var mesmo = ponto.getAttribute('data-indice-dado') === String(indice);
        ponto.classList.toggle('em-destaque', mesmo);
        ponto.classList.toggle('esmaecida', !mesmo);
      });
    }
    function limparDestaque() {
      pontosInterativos.forEach(function(ponto) {
        ponto.classList.remove('em-destaque', 'esmaecida');
      });
    }
    pontosInterativos.forEach(function(ponto) {
      var indice = ponto.getAttribute('data-indice-dado');
      ponto.addEventListener('pointerenter', function() { aplicarDestaque(indice); });
      ponto.addEventListener('pointerleave', function() {
        if (indiceFixado === null) limparDestaque(); else aplicarDestaque(indiceFixado);
      });
      ponto.addEventListener('focus', function() { aplicarDestaque(indice); });
      ponto.addEventListener('blur', function() {
        if (indiceFixado === null) limparDestaque(); else aplicarDestaque(indiceFixado);
      });
      ponto.addEventListener('click', function(evento) {
        evento.stopPropagation();
        indiceFixado = indiceFixado === indice ? null : indice;
        if (indiceFixado === null) limparDestaque(); else aplicarDestaque(indiceFixado);
      });
    });

    var escalaAtual = visual.escala_inicial === 'original' ? 'original' : 'transformada';
    var botoesEscala = raiz.querySelectorAll('.selecionar-escala-matriz');
    function selecionarEscala(modo) {
      escalaAtual = modo === 'original' ? 'original' : 'transformada';
      Array.prototype.forEach.call(
        svg.querySelectorAll('.camada-escala-prioridade'), function(grupo) {
          grupo.style.display = grupo.getAttribute('data-escala') === escalaAtual ? '' : 'none';
        }
      );
      Array.prototype.forEach.call(botoesEscala, function(botao) {
        var ativo = botao.dataset.escala === escalaAtual;
        botao.classList.toggle('ativa', ativo);
        botao.setAttribute('aria-pressed', ativo ? 'true' : 'false');
      });
      tooltip.style.display = 'none';
    }
    Array.prototype.forEach.call(botoesEscala, function(botao) {
      botao.addEventListener('click', function() { selecionarEscala(botao.dataset.escala); });
    });
    selecionarEscala(escalaAtual);

    var legenda = raiz.querySelector('.legenda-interativa-prioridade');
    var ocultas = {};
    ['prioridade_1', 'prioridade_2', 'prioridade_3', 'monitoramento'].forEach(function(id) {
      var categoria = visual.categorias[id];
      var botao = document.createElement('button');
      botao.type = 'button'; botao.dataset.prioridade = id;
      botao.innerHTML = '<span style=\"background:' + categoria.cor + '\"></span>' + categoria.nome;
      botao.addEventListener('click', function() {
        ocultas[id] = !ocultas[id];
        botao.classList.toggle('inativa', ocultas[id]);
        elementosPontos.forEach(function(ponto) {
          if (ponto.dataset.prioridade === id) ponto.style.display = ocultas[id] ? 'none' : '';
        });
      });
      legenda.appendChild(botao);
    });
    raiz.querySelector('.mostrar-prioridades').addEventListener('click', function() {
      ocultas.monitoramento = true;
      elementosPontos.forEach(function(ponto) {
        ponto.style.display = ponto.dataset.prioridade === 'monitoramento' ? 'none' : '';
      });
      var b = legenda.querySelector('[data-prioridade=\"monitoramento\"]');
      if (b) b.classList.add('inativa');
    });
    raiz.querySelector('.mostrar-todos').addEventListener('click', function() {
      ocultas = {};
      elementosPontos.forEach(function(ponto) { ponto.style.display = ''; });
      Array.prototype.forEach.call(legenda.querySelectorAll('button'), function(b) { b.classList.remove('inativa'); });
    });

    var view = { x: 0, y: 0, w: largura, h: altura };
    function aplicarView() { svg.setAttribute('viewBox', [view.x, view.y, view.w, view.h].join(' ')); }
    raiz.querySelector('.restaurar-zoom').addEventListener('click', function() {
      view = { x: 0, y: 0, w: largura, h: altura }; aplicarView();
    });
    svg.addEventListener('wheel', function(evento) {
      evento.preventDefault();
      var fator = evento.deltaY > 0 ? 1.18 : .84;
      var caixa = svg.getBoundingClientRect();
      var px = view.x + (evento.clientX - caixa.left) / caixa.width * view.w;
      var py = view.y + (evento.clientY - caixa.top) / caixa.height * view.h;
      var nw = Math.min(largura, Math.max(260, view.w * fator));
      var nh = nw * altura / largura;
      view.x = Math.max(0, Math.min(largura - nw, px - (px - view.x) * nw / view.w));
      view.y = Math.max(0, Math.min(altura - nh, py - (py - view.y) * nh / view.h));
      view.w = nw; view.h = nh; aplicarView();
    }, { passive: false });
    var arrastando = false, anterior = null;
    svg.addEventListener('pointerdown', function(evento) {
      // Não captura o ponteiro iniciado em uma bolha: isso preserva o evento
      // de clique usado para fixar o destaque. O arraste continua disponível
      // em qualquer área vazia do gráfico.
      if (evento.target.classList.contains('ponto-prioridade')) return;
      arrastando = true; anterior = { x: evento.clientX, y: evento.clientY };
      svg.setPointerCapture(evento.pointerId);
    });
    svg.addEventListener('pointermove', function(evento) {
      if (!arrastando) return;
      var caixa = svg.getBoundingClientRect();
      view.x = Math.max(0, Math.min(largura - view.w,
        view.x - (evento.clientX - anterior.x) / caixa.width * view.w));
      view.y = Math.max(0, Math.min(altura - view.h,
        view.y - (evento.clientY - anterior.y) / caixa.height * view.h));
      anterior = { x: evento.clientX, y: evento.clientY }; aplicarView();
    });
    svg.addEventListener('pointerup', function() { arrastando = false; });
    svg.addEventListener('pointercancel', function() { arrastando = false; });
  }

  function iniciarTudo() {
    document.querySelectorAll('.bloco-matriz-prioridade').forEach(function(raiz) {
      iniciarMatriz(raiz); iniciarTabela(raiz);
    });
  }
  if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', iniciarTudo);
  } else iniciarTudo();
  window.addEventListener('beforeprint', function() {
    document.querySelectorAll('.detalhes-tabela-prioridade').forEach(function(detalhe) {
      detalhe.open = true;
    });
  });
})();
  "
}
