# Aplicativo QualificaVIVA -----------------------------------------------------
#
# Arquivo de composição da interface Shiny. Os módulos em `R/` concentram a
# lógica de leitura, padronização, análise e relatório; este arquivo coordena o
# estado da sessão, as ações do usuário e a apresentação dos resultados.

# A configuração editável é carregada antes de qualquer módulo. Ela concentra
# nomes, títulos, cores, dimensões e limites que podem ser ajustados sem entrar
# nos motores internos de cálculo.
source("R/configuracao_aplicativo.R", local = environment())
# O controlador abaixo consulta o interruptor de desenvolvimento salvo na
# configuração antes de cada operação e recarrega apenas módulos seguros.
source("R/modo_desenvolvimento.R", local = environment())
source("R/gerenciamento_memoria.R", local = environment())

# Converte o limite amigável em MiB para o número de bytes esperado pelo Shiny.
options(
  shiny.maxRequestSize = configuracao_viva_quali$aplicativo$limite_upload_mib * 1024^2
)

# Dependências usadas diretamente na interface e nas tabelas interativas.
library(shiny)
library(DT)

# Carregamento explícito dos módulos na ordem de suas dependências.
# `local` mantém as funções no ambiente do aplicativo, sem poluir o ambiente global.
ambiente_aplicativo <- environment()
# Guarda a raiz enquanto `app.R` está sendo carregado. Esse caminho permanece
# estável mesmo quando testes ou ferramentas mudam temporariamente a pasta ativa.
diretorio_projeto_aplicativo <- normalizePath(
  getwd(), winslash = "/", mustWork = TRUE
)
source("R/importar_dados.R", local = ambiente_aplicativo)
source("R/importar_csv.R", local = ambiente_aplicativo)
source("R/importar_excel.R", local = ambiente_aplicativo)
source("R/importar_dbf.R", local = ambiente_aplicativo)
source("R/importar_dta.R", local = ambiente_aplicativo)
source("R/regras_padronizacao.R", local = ambiente_aplicativo)
source("R/padronizar_base.R", local = ambiente_aplicativo)
source("R/enriquecer_cnes.R", local = ambiente_aplicativo)
source("R/regras_validade.R", local = ambiente_aplicativo)
source("R/analisar_validade.R", local = ambiente_aplicativo)
source("R/regras_consistencia.R", local = ambiente_aplicativo)
source("R/analisar_consistencia.R", local = ambiente_aplicativo)
source("R/referencias_territoriais.R", local = ambiente_aplicativo)
source("R/indicadores_relatorio_validade.R", local = ambiente_aplicativo)
source("R/indicadores_relatorio_consistencia.R", local = ambiente_aplicativo)
source("R/priorizacao_relatorio.R", local = ambiente_aplicativo)
source("R/visualizacoes_relatorio.R", local = ambiente_aplicativo)
source("R/visualizacoes_relatorio_consistencia.R", local = ambiente_aplicativo)
source("R/relatorio_validade.R", local = ambiente_aplicativo)
source("R/relatorio_consistencia.R", local = ambiente_aplicativo)

# Interface --------------------------------------------------------------------
#
# O aplicativo é dividido em quatro páginas: início, importação/padronização,
# validade e consistência. Resultados dinâmicos são inseridos pelos `uiOutput`.
identidade_aplicativo <- configuracao_viva_quali$aplicativo$identidade

cabecalho_pagina <- function(titulo, subtitulo) {
  div(
    class = "cabecalho cabecalho-modulo",
    tags$img(
      class = "cabecalho-simbolo", src = identidade_aplicativo$simbolo,
      alt = "", `aria-hidden` = "true"
    ),
    div(class = "cabecalho-texto", h1(titulo), p(subtitulo))
  )
}

ui <- navbarPage(
  title = tags$span(
    class = "marca-navbar",
    tags$img(
      class = "marca-navbar-logo",
      src = identidade_aplicativo$logo,
      alt = paste("Logomarca", configuracao_viva_quali$aplicativo$nome)
    ),
    tags$img(
      class = "marca-navbar-simbolo",
      src = identidade_aplicativo$simbolo,
      alt = paste("Logomarca", configuracao_viva_quali$aplicativo$nome)
    )
  ),
  windowTitle = paste(
    configuracao_viva_quali$aplicativo$nome, "—",
    "Qualidade dos dados do VIVA-Sinan"
  ),
  id = "navegacao_principal",
  collapsible = TRUE,
  # Estilos comuns a todas as páginas, painéis, cartões, tabelas e relatórios.
  header = tags$head(
    tags$link(rel = "icon", type = "image/png", href = identidade_aplicativo$simbolo),
    tags$meta(name = "theme-color", content = cor_viva_quali("principal")),
    tags$style(HTML(paste0(css_variaveis_aplicativo(), "
      html { min-height: 100%; }
      body { min-height: 100%; background: var(--viva-fundo); color: var(--viva-texto);
             font-family: 'Segoe UI', Arial, sans-serif; font-size: 15px; line-height: 1.5; }
      a { color: var(--viva-principal); }
      a:hover, a:focus { color: var(--viva-marca); }
      .navbar-default { min-height: 72px; margin-bottom: 0; background: rgba(255,255,255,.98);
                        border: 0; border-bottom: 1px solid var(--viva-borda);
                        box-shadow: 0 4px 18px rgba(48,35,64,.07); }
      .navbar-header { min-height: 72px; }
      .navbar-brand { height: 72px; padding: 11px 20px; display: flex; align-items: center; }
      .marca-navbar { display: inline-flex; align-items: center; }
      .marca-navbar img { object-fit: contain; object-position: left center; }
      .marca-navbar-logo { display: block; width: 218px; height: 50px; }
      .marca-navbar-simbolo { display: none; width: 44px; height: 44px; }
      .navbar-nav > li > a { min-height: 72px; padding: 25px 15px 22px; color: var(--viva-secundario);
                             font-weight: 600; transition: color .16s ease, box-shadow .16s ease; }
      .navbar-default .navbar-nav > li > a:hover,
      .navbar-default .navbar-nav > li > a:focus { color: var(--viva-principal); background: transparent; }
      .navbar-default .navbar-nav > .active > a,
      .navbar-default .navbar-nav > .active > a:hover,
      .navbar-default .navbar-nav > .active > a:focus { color: var(--viva-principal);
        background: transparent; box-shadow: inset 0 -4px 0 var(--viva-marca); }
      .navbar-toggle { margin-top: 18px; border-color: var(--viva-borda); border-radius: 8px; }
      .navbar-default .navbar-toggle .icon-bar { background-color: var(--viva-principal); }
      .tab-pane { max-width: 1240px; min-height: 64vh; margin: 0 auto; padding: 38px 28px 64px; }
      .cabecalho { margin-bottom: 30px; }
      .cabecalho-modulo { display: flex; align-items: center; gap: 18px; padding-bottom: 20px;
                          border-bottom: 1px solid var(--viva-borda); }
      .cabecalho-simbolo { width: 64px; height: 64px; flex: 0 0 64px; object-fit: contain; }
      .cabecalho-texto { min-width: 0; }
      .cabecalho h1 { color: var(--viva-principal); font-size: 32px; line-height: 1.2;
                      font-weight: 750; margin: 0 0 5px; }
      .cabecalho p { color: var(--viva-secundario); font-size: 17px; margin: 0; }
      .hero-inicio { position: relative; overflow: hidden; display: grid;
                     grid-template-columns: minmax(0, 1fr) 220px; align-items: center; gap: 36px;
                     background: white; border: 1px solid var(--viva-borda); border-radius: 20px;
                     box-shadow: 0 16px 42px rgba(54,35,74,.09); margin-bottom: 38px;
                     padding: 42px 46px; }
      .hero-inicio::after { content: ''; position: absolute; left: 0; right: 0; bottom: 0; height: 6px;
                            background: linear-gradient(90deg, var(--viva-principal),
                              var(--viva-marca) 68%, var(--viva-validacao)); }
      .hero-conteudo { position: relative; z-index: 1; }
      .hero-selo { display: inline-block; color: var(--viva-marca); font-size: 12px; font-weight: 750;
                   letter-spacing: .12em; margin-bottom: 18px; text-transform: uppercase; }
      .logo-inicio { display: block; width: min(100%, 440px); height: auto; margin-bottom: 20px; }
      .hero-inicio p { max-width: 720px; color: var(--viva-secundario); font-size: 19px; margin: 0; }
      .hero-simbolo { width: 210px; height: 210px; object-fit: contain; opacity: .10; }
      .titulo-secao-inicio { color: var(--viva-principal); font-size: 22px; margin: 0 0 18px; }
      .painel { background: white; border: 1px solid var(--viva-borda); border-radius: 14px;
                box-shadow: 0 7px 24px rgba(54,35,74,.055); margin-bottom: 24px; padding: 26px; }
      .painel h3 { color: var(--viva-principal); font-size: 21px; font-weight: 700; margin: 0 0 18px; }
      .painel h4 { color: var(--viva-principal); }
      .formatos, .observacao { color: var(--viva-secundario); font-size: 13px; margin-top: 10px; }
      .resumo { display: grid; grid-template-columns: repeat(auto-fit, minmax(170px, 1fr)); gap: 12px; }
      .resumo-item { min-width: 0; padding: 14px 16px; background: var(--viva-superficie-suave);
                     border: 1px solid var(--viva-borda); border-top: 3px solid var(--viva-marca);
                     border-radius: 10px; }
      .resumo-item span { color: var(--viva-secundario); display: block; font-size: 11px;
                          font-weight: 650; letter-spacing: .06em; text-transform: uppercase; }
      .resumo-item strong { display: block; color: var(--viva-texto); font-size: 18px; margin-top: 3px; }
      .btn { min-height: 42px; border-radius: 9px; font-weight: 650; padding: 9px 15px;
             transition: background .16s ease, border-color .16s ease, color .16s ease,
                         box-shadow .16s ease, transform .16s ease; }
      .btn:focus, .btn.focus { outline: 0; box-shadow: 0 0 0 3px rgba(144,8,152,.18); }
      .btn-primary { color: white; background: var(--viva-principal); border-color: var(--viva-principal); }
      .btn-primary:hover, .btn-primary:focus, .btn-primary:active { color: white;
        background: var(--viva-principal-escura); border-color: var(--viva-principal-escura); }
      .btn-default { color: var(--viva-principal); background: white; border-color: var(--viva-borda); }
      .btn-default:hover, .btn-default:focus { color: var(--viva-principal); background: var(--viva-superficie-marca);
                                               border-color: var(--viva-principal-clara); }
      .btn[disabled], .btn.disabled { opacity: .58; }
      .alerta-leitura, .aviso-modulo { background: #FFF8E7; border-left: 4px solid #D38B12;
                        border-radius: 8px; color: #694B0B; margin: 14px 0; padding: 14px 16px; }
      .erro-leitura { background: #FFF2F2; border-left: 4px solid #B83A46;
                      border-radius: 8px; color: #74242C; margin-bottom: 18px; padding: 14px 16px; }
      .status-sucesso { background: #EAF7F5; border-left: 4px solid var(--viva-validacao-escura);
                        border-radius: 8px; color: #155B54; margin: 14px 0; padding: 14px 16px; }
      .cards-modulos { display: grid; grid-template-columns: repeat(3, minmax(0, 1fr)); gap: 20px; }
      .card-modulo { position: relative; width: 100%; min-height: 205px; overflow: hidden;
                     background: white; border: 1px solid var(--viva-borda); border-radius: 16px;
                     box-shadow: 0 8px 26px rgba(54,35,74,.07); color: var(--viva-texto);
                     padding: 28px; text-align: left; white-space: normal; }
      .card-modulo::after { content: '→'; position: absolute; right: 24px; bottom: 20px;
                            color: var(--viva-marca); font-size: 25px; font-weight: 700; }
      .card-modulo:hover, .card-modulo:focus { border-color: var(--viva-marca); color: var(--viva-texto);
        box-shadow: 0 13px 32px rgba(71,30,130,.12); transform: translateY(-3px); }
      .card-modulo-indice { display: inline-flex; align-items: center; justify-content: center;
                            width: 38px; height: 30px; margin-bottom: 18px; border-radius: 999px;
                            background: var(--viva-superficie-marca); color: var(--viva-marca);
                            font-size: 12px; font-weight: 800; letter-spacing: .05em; }
      .card-modulo h3 { color: var(--viva-principal); font-size: 20px; margin: 0 0 11px; }
      .card-modulo p { max-width: 90%; font-size: 15px; font-weight: normal; margin: 0; }
      .navegacao-modulo { display: flex; flex-wrap: wrap; gap: 8px; margin-bottom: 26px;
                          padding-bottom: 16px; border-bottom: 1px solid var(--viva-borda); }
      .navegacao-modulo .btn { min-height: 36px; padding: 6px 12px; font-size: 13px; }
      .form-control, .selectize-input { min-height: 40px; border-color: var(--viva-borda);
                                       border-radius: 8px; box-shadow: none; }
      .form-control:focus, .selectize-input.focus { border-color: var(--viva-marca);
        box-shadow: 0 0 0 3px rgba(144,8,152,.12); }
      .progress { height: 10px; border-radius: 999px; background: var(--viva-superficie-marca); }
      .progress-bar { background: var(--viva-validacao-escura); }
      .shiny-file-input-progress { margin-bottom: 8px; }
      .dataTables_wrapper { overflow-x: auto; }
      table.dataTable thead th { color: white; background: var(--viva-principal); border-color: var(--viva-principal); }
      table.dataTable.stripe tbody tr.odd, table.dataTable.display tbody tr.odd { background: #FBF9FC; }
      table.dataTable.hover tbody tr:hover, table.dataTable.display tbody tr:hover { background: var(--viva-superficie-marca); }
      .acoes-relatorio { display: flex; flex-wrap: wrap; gap: 9px; margin: 14px 0; }
      .visualizador-relatorio { border: 1px solid var(--viva-borda); border-radius: 12px;
                                box-shadow: 0 7px 24px rgba(54,35,74,.055); height: 760px; width: 100%; }
      .grupo-consistencia { border: 1px solid var(--viva-borda); border-radius: 10px;
                            margin: 11px 0; overflow: hidden; }
      .grupo-consistencia summary { background: var(--viva-superficie-marca); color: var(--viva-principal);
                                    cursor: pointer; font-size: 15px; font-weight: 650; padding: 14px 16px; }
      .grupo-consistencia summary span { color: var(--viva-secundario); float: right; font-weight: normal; }
      .grupo-consistencia table { margin: 0; width: 100%; }
      .grupo-consistencia th, .grupo-consistencia td { padding: 9px 12px; vertical-align: top; }
      .grupo-consistencia th { background: #FBF9FC; color: var(--viva-secundario); }
      .dados-autoria { display:grid; grid-template-columns:max-content 1fr; gap:8px 18px; margin:18px 0 24px; }
      .dados-autoria dt { color:var(--viva-secundario); font-weight:650; }
      .dados-autoria dd { margin:0; }
      .rodape-aplicativo { display: flex; align-items: center; justify-content: center; gap: 18px;
                           border-top:1px solid var(--viva-borda); color:var(--viva-secundario);
                           background: white; font-size:13px; margin-top:30px; padding:22px 28px; }
      .rodape-simbolo { width: 42px; height: 42px; object-fit: contain; }
      .rodape-texto { text-align: left; }
      .rodape-aplicativo p { margin:3px 0; }
      @media (max-width: 991px) {
        .cards-modulos { grid-template-columns: 1fr; }
        .hero-inicio { grid-template-columns: 1fr; }
        .hero-simbolo { display: none; }
        .navbar-nav > li > a { padding-left: 10px; padding-right: 10px; }
      }
      @media (max-width: 767px) {
        .navbar-default, .navbar-header { min-height: 64px; }
        .navbar-brand { height: 64px; padding: 10px 15px; }
        .marca-navbar-logo { display: none; }
        .marca-navbar-simbolo { display: block; }
        .navbar-collapse { background: white; border-top-color: var(--viva-borda); }
        .navbar-nav > li > a { min-height: auto; padding: 13px 20px; }
        .navbar-default .navbar-nav > .active > a { box-shadow: inset 4px 0 0 var(--viva-marca); }
        .tab-pane { padding: 28px 16px 52px; }
        .hero-inicio { padding: 30px 24px 34px; border-radius: 16px; }
        .hero-inicio p { font-size: 17px; }
        .cabecalho-modulo { align-items: flex-start; }
        .cabecalho-simbolo { width: 50px; height: 50px; flex-basis: 50px; }
        .cabecalho h1 { font-size: 26px; }
        .cabecalho p { font-size: 15px; }
        .painel { padding: 20px; }
        .dados-autoria { grid-template-columns: 1fr; gap: 2px; }
        .dados-autoria dd { margin: 0 0 9px; }
        .rodape-aplicativo { align-items: flex-start; }
      }
    ")))
  ),
  # Página inicial: atalhos para os três fluxos funcionais.
  tabPanel(
    "Página inicial", value = "inicio",
    div(
      class = "hero-inicio",
      div(
        class = "hero-conteudo",
        tags$span(class = "hero-selo", "Qualidade dos dados em saúde"),
        tags$img(
          class = "logo-inicio", src = identidade_aplicativo$logo,
          alt = paste("Logomarca", configuracao_viva_quali$aplicativo$nome)
        ),
        h1(class = "sr-only", configuracao_viva_quali$aplicativo$nome),
        p(configuracao_viva_quali$aplicativo$subtitulo)
      ),
      tags$img(
        class = "hero-simbolo", src = identidade_aplicativo$simbolo,
        alt = "", `aria-hidden` = "true"
      )
    ),
    h2(class = "titulo-secao-inicio", "Funções disponíveis"),
    div(class = "cards-modulos",
        actionButton(
          "abrir_importacao", class = "card-modulo",
          label = tagList(tags$span(class = "card-modulo-indice", "01"),
                          h3("Importação e Padronização"),
                          p("Importe, visualize, prepare e padronize a base de dados."))
        ),
        actionButton(
          "abrir_validade", class = "card-modulo",
          label = tagList(tags$span(class = "card-modulo-indice", "02"),
                          h3("Análise de Validade"),
                          p("Execute as regras de validade sobre a base padronizada."))
        ),
        actionButton(
          "abrir_consistencia", class = "card-modulo",
          label = tagList(tags$span(class = "card-modulo-indice", "03"),
                          h3("Análise de Consistência"),
                          p("Identifique incompatibilidades internas na base padronizada."))
        ))
  ),
  # Importação e padronização: configuração do arquivo, prévias e auditoria.
  tabPanel(
    "Importação e Padronização", value = "importacao",
    div(class = "navegacao-modulo",
        actionButton("importacao_inicio", "Página inicial"),
        actionButton("importacao_validade", "Análise de Validade"),
        actionButton("importacao_consistencia", "Análise de Consistência")),
    cabecalho_pagina(
      "Importação e Padronização",
      "Carregamento, configuração e preparação da base de dados"
    ),
    div(class = "painel",
        h3("Importar base de dados"),
        fileInput(
          "arquivo", "Selecione ou arraste um arquivo para esta área",
          accept = c(".dbf", ".dta", ".xls", ".xlsx", ".csv"), multiple = FALSE,
          buttonLabel = "Selecionar arquivo", placeholder = "Nenhum arquivo selecionado"
        ),
        div(class = "formatos", "Formatos aceitos: DBF, DTA, XLS, XLSX ou CSV")),
    uiOutput("mensagem_erro"),
    uiOutput("configuracao_excel"),
    uiOutput("configuracao_csv"),
    uiOutput("resumo_importacao"),
    uiOutput("acao_padronizacao"),
    uiOutput("mensagem_erros_padronizacao"),
    uiOutput("secao_previa_final"),
    uiOutput("resumo_padronizacao"),
    uiOutput("acao_enriquecimento"),
    uiOutput("mensagem_enriquecimento"),
    uiOutput("resumo_enriquecimento"),
    uiOutput("variaveis_ausentes_ui"),
    uiOutput("auditoria_ui"),
    uiOutput("secao_previa_padronizada")
  ),
  # Validade: execução, síntese, detalhamento e geração do relatório.
  tabPanel(
    "Análise de Validade", value = "validade",
    div(class = "navegacao-modulo",
        actionButton("validade_inicio", "Página inicial"),
        actionButton("validade_importacao", "Importação e Padronização"),
        actionButton("validade_consistencia", "Análise de Consistência")),
    cabecalho_pagina(
      "Análise de Validade",
      "Aplicação das regras metodológicas à base padronizada"
    ),
    uiOutput("estado_modulo_validade"),
    uiOutput("resumo_validade_ui"),
    uiOutput("avisos_validade_ui"),
    uiOutput("resultado_regras_validade_ui"),
    uiOutput("grafico_validade_ui"),
    uiOutput("registros_validade_ui"),
    uiOutput("relatorio_validade_ui")
  ),
  # Consistência: resultados por grupo/regra e relatório analítico hierárquico.
  tabPanel(
    "Análise de Consistência", value = "consistencia",
    div(class = "navegacao-modulo",
        actionButton("consistencia_inicio", "Página inicial"),
        actionButton("consistencia_importacao", "Importação e Padronização"),
        actionButton("consistencia_validade", "Análise de Validade")),
    cabecalho_pagina(
      "Análise de Consistência",
      "Identificação de incompatibilidades internas na base padronizada"
    ),
    uiOutput("estado_modulo_consistencia"),
    uiOutput("resumo_consistencia_ui"),
    uiOutput("avisos_consistencia_ui"),
    uiOutput("resultado_regras_consistencia_ui"),
    uiOutput("grafico_consistencia_ui"),
    uiOutput("registros_consistencia_ui"),
    uiOutput("relatorio_consistencia_ui")
  ),
  # Informações institucionais, autoria e ferramentas de apoio ao desenvolvimento.
  tabPanel(
    "Sobre", value = "sobre",
    cabecalho_pagina(
      "Sobre o aplicativo",
      configuracao_viva_quali$aplicativo$subtitulo
    ),
    div(
      class = "painel",
      h3("Autoria"),
      tags$dl(
        class = "dados-autoria",
        tags$dt("Autor(es)"), tags$dd(valor_autoria("autores")),
        tags$dt("Instituição"), tags$dd(valor_autoria("instituicao")),
        tags$dt("Contato"), tags$dd(valor_autoria("contato")),
        tags$dt("Ano"), tags$dd(valor_autoria("ano")),
        tags$dt("Versão"), tags$dd(valor_autoria("versao"))
      ),
      h3("Apoio ao desenvolvimento"),
      p(configuracao_autoria()$declaracao_apoio),
      p(class = "observacao", configuracao_autoria()$declaracao_responsabilidade)
    )
  ),
  footer = tags$footer(
    class = "rodape-aplicativo",
    tags$img(
      class = "rodape-simbolo", src = identidade_aplicativo$simbolo,
      alt = "", `aria-hidden` = "true"
    ),
    div(
      class = "rodape-texto",
      tags$p(
        tags$strong("Autoria: "), valor_autoria("autores"),
        " — ", valor_autoria("instituicao")
      ),
      tags$p(
        paste("Versão", valor_autoria("versao"), "—", valor_autoria("ano")),
        " · Desenvolvido com apoio do ", configuracao_autoria()$ferramenta_apoio, "."
      )
    )
  )
)

# Servidor ---------------------------------------------------------------------
#
# Cada chamada de `reactiveVal` armazena um estado da sessão do usuário. Quando
# seu valor muda, somente as saídas dependentes são recalculadas pelo Shiny.
server <- function(input, output, session) {
  # Estados de importação e padronização.
  formato_atual <- reactiveVal(NULL)
  planilhas_atuais <- reactiveVal(character())
  base_bruta <- reactiveVal(NULL)
  previa_base_bruta <- reactiveVal(NULL)
  base_padronizada <- reactiveVal(NULL)
  resultado_padronizacao <- reactiveVal(NULL)
  resultado_enriquecimento <- reactiveVal(NULL)
  erro_enriquecimento <- reactiveVal(NULL)

  # Estado das análises de validade e consistência.
  resultado_validade <- reactiveVal(NULL)
  erro_validade <- reactiveVal(NULL)
  resultado_consistencia <- reactiveVal(NULL)
  erro_consistencia <- reactiveVal(NULL)

  # Artefatos temporários e falhas dos dois relatórios.
  artefato_relatorio <- reactiveVal(NULL)
  erro_relatorio <- reactiveVal(NULL)
  artefato_relatorio_consistencia <- reactiveVal(NULL)
  erro_relatorio_consistencia <- reactiveVal(NULL)
  # Diagnósticos e metadados apresentados na interface.
  erros_padronizacao <- reactiveVal(character())
  metadados_importados <- reactiveVal(NULL)
  erro_interface <- reactiveVal(NULL)

  # Cada sessão reutiliza dois endereços de visualização. Antes de substituir
  # um relatório, seu diretório temporário é removido e a memória é coletada.
  sufixo_sessao <- gsub("[^A-Za-z0-9]", "", session$token)
  prefixo_relatorio_validade <- paste0("relatorio-validade-", sufixo_sessao)
  prefixo_relatorio_consistencia <- paste0(
    "relatorio-consistencia-", sufixo_sessao
  )

  limpar_artefato_sessao <- function(estado, prefixo) {
    artefato <- isolate(estado())
    suppressWarnings(try(shiny::removeResourcePath(prefixo), silent = TRUE))
    remover_arquivos_temporarios_relatorio(artefato)
    estado(NULL)
    invisible(NULL)
  }

  # Invalida todo resultado derivado quando uma nova base é escolhida/importada.
  limpar_resultado <- function() {
    limpar_artefato_sessao(artefato_relatorio, prefixo_relatorio_validade)
    limpar_artefato_sessao(
      artefato_relatorio_consistencia, prefixo_relatorio_consistencia
    )
    base_bruta(NULL)
    previa_base_bruta(NULL)
    base_padronizada(NULL)
    resultado_padronizacao(NULL)
    resultado_enriquecimento(NULL)
    erro_enriquecimento(NULL)
    resultado_validade(NULL)
    resultado_consistencia(NULL)
    erro_relatorio(NULL)
    erro_relatorio_consistencia(NULL)
    erro_validade(NULL)
    erro_consistencia(NULL)
    erros_padronizacao(character())
    metadados_importados(NULL)
    erro_interface(NULL)
    liberar_memoria_ram(completa = TRUE)
  }

  # Traduz exceções de leitura e as associa ao formato em processamento.
  registrar_erro <- function(erro, contexto) {
    message(sprintf("[%s] %s", contexto, conditionMessage(erro)))
    erro_interface(mensagem_erro_importacao(erro, contexto))
  }

  # Registra uma importação bem-sucedida e os metadados mostrados no resumo.
  concluir_importacao <- function(dados, arquivo, formato, planilha = NULL) {
    validar_base_importada(dados)
    base_bruta(dados)
    previa_base_bruta(utils::head(dados, 20L))
    base_padronizada(NULL)
    resultado_padronizacao(NULL)
    resultado_enriquecimento(NULL)
    erro_enriquecimento(NULL)
    resultado_validade(NULL)
    resultado_consistencia(NULL)
    limpar_artefato_sessao(artefato_relatorio, prefixo_relatorio_validade)
    erro_relatorio(NULL)
    limpar_artefato_sessao(
      artefato_relatorio_consistencia, prefixo_relatorio_consistencia
    )
    erro_relatorio_consistencia(NULL)
    erro_validade(NULL)
    erro_consistencia(NULL)
    erros_padronizacao(character())
    metadados_importados(list(
      arquivo = arquivo,
      formato = toupper(formato),
      registros = nrow(dados),
      variaveis = ncol(dados),
      planilha = planilha
    ))
    erro_interface(NULL)
  }

  session$onSessionEnded(function() {
    remover_arquivos_temporarios_relatorio(isolate(artefato_relatorio()))
    remover_arquivos_temporarios_relatorio(
      isolate(artefato_relatorio_consistencia())
    )
    suppressWarnings(try(
      shiny::removeResourcePath(prefixo_relatorio_validade), silent = TRUE
    ))
    suppressWarnings(try(
      shiny::removeResourcePath(prefixo_relatorio_consistencia), silent = TRUE
    ))
    liberar_memoria_ram(completa = TRUE)
  })

  # Navegação ------------------------------------------------------------------
  # Todos os atalhos alteram a aba ativa sem recarregar a sessão.
  ir_para <- function(pagina) {
    updateNavbarPage(session, "navegacao_principal", selected = pagina)
  }

  observeEvent(input$abrir_importacao, ir_para("importacao"))
  observeEvent(input$abrir_validade, ir_para("validade"))
  observeEvent(input$abrir_consistencia, ir_para("consistencia"))
  observeEvent(input$importacao_inicio, ir_para("inicio"))
  observeEvent(input$importacao_validade, ir_para("validade"))
  observeEvent(input$importacao_consistencia, ir_para("consistencia"))
  observeEvent(input$validade_inicio, ir_para("inicio"))
  observeEvent(input$validade_importacao, ir_para("importacao"))
  observeEvent(input$validade_consistencia, ir_para("consistencia"))
  observeEvent(input$validade_ir_importacao, ir_para("importacao"))
  observeEvent(input$consistencia_inicio, ir_para("inicio"))
  observeEvent(input$consistencia_importacao, ir_para("importacao"))
  observeEvent(input$consistencia_validade, ir_para("validade"))
  observeEvent(input$consistencia_ir_importacao, ir_para("importacao"))

  # Importação -----------------------------------------------------------------
  # Ao receber um arquivo, detecta o formato. DBF e DTA são lidos imediatamente;
  # Excel aguarda a escolha da aba e CSV aguarda a configuração de leitura.
  observeEvent(input$arquivo, {
    limpar_resultado()
    planilhas_atuais(character())
    formato <- tryCatch(
      identificar_formato(input$arquivo$name),
      error = function(e) {
        registrar_erro(e, "formato")
        NULL
      }
    )
    formato_atual(formato)
    req(formato)

    if (identical(formato, "dbf")) {
      withProgress(message = "Importando arquivo DBF...", value = 0.4, {
        tryCatch({
          dados <- importar_dbf(input$arquivo$datapath)
          incProgress(0.6)
          concluir_importacao(dados, input$arquivo$name, formato)
        }, error = function(e) registrar_erro(e, "DBF"))
      })
    }

    if (identical(formato, "dta")) {
      withProgress(message = "Importando arquivo Stata...", value = 0.4, {
        tryCatch({
          dados <- importar_dta(input$arquivo$datapath)
          incProgress(0.6)
          concluir_importacao(dados, input$arquivo$name, formato)
        }, error = function(e) registrar_erro(e, "DTA"))
      })
    }

    if (formato %in% c("xls", "xlsx")) {
      tryCatch({
        planilhas <- listar_planilhas_excel(input$arquivo$datapath)
        planilhas_atuais(planilhas)
        if (length(planilhas) == 1L) {
          withProgress(message = "Importando planilha...", value = 0.4, {
            dados <- importar_excel(input$arquivo$datapath, planilhas[[1]])
            incProgress(0.6)
            concluir_importacao(dados, input$arquivo$name, formato, planilhas[[1]])
          })
        }
      }, error = function(e) registrar_erro(e, "Excel"))
    }
  }, ignoreNULL = TRUE)

  # Seleção e leitura de planilhas Excel.
  output$configuracao_excel <- renderUI({
    req(formato_atual() %in% c("xls", "xlsx"), length(planilhas_atuais()) > 1L)
    div(class = "painel",
        h3("Selecionar planilha"),
        selectInput("planilha_excel", "Planilha a importar",
                    choices = c("Selecione uma planilha" = "", planilhas_atuais()),
                    selected = ""))
  })

  observeEvent(input$planilha_excel, {
    req(input$arquivo, nzchar(input$planilha_excel))
    limpar_resultado()
    withProgress(message = "Importando planilha...", value = 0.4, {
      tryCatch({
        dados <- importar_excel(input$arquivo$datapath, input$planilha_excel)
        incProgress(0.6)
        concluir_importacao(dados, input$arquivo$name, formato_atual(), input$planilha_excel)
      }, error = function(e) registrar_erro(e, "Excel"))
    })
  }, ignoreInit = TRUE)

  # Configuração, prévia e confirmação de arquivos CSV.
  output$configuracao_csv <- renderUI({
    req(identical(formato_atual(), "csv"))
    div(class = "painel",
        h3("Configuração do CSV"),
        fluidRow(
          column(4, selectInput("separador_csv", "Separador de campos",
                                choices = c("Ponto e vírgula (;)" = ";",
                                            "Vírgula (,)" = ",",
                                            "Tabulação" = "\\t",
                                            "Barra vertical (|)" = "|",
                                            "Outro" = "outro"), selected = ";")),
          column(4, selectInput("delimitador_csv", "Delimitador de texto",
                                choices = c("Aspas duplas (\")" = "\"",
                                            "Aspas simples (')" = "'",
                                            "Nenhum" = ""), selected = "\"")),
          column(4, selectInput("codificacao_csv", "Codificação de caracteres",
                                choices = c("Windows-1252" = "Windows-1252",
                                            "UTF-8" = "UTF-8",
                                            "Latin-1 / ISO-8859-1" = "ISO-8859-1"),
                                selected = "Windows-1252"))
        ),
        conditionalPanel(
          condition = "input.separador_csv == 'outro'",
          textInput("separador_personalizado", "Outro separador (um caractere)", value = "")
        ),
        hr(),
        h4("Prévia da leitura"),
        uiOutput("status_previa_csv"),
        DTOutput("tabela_previa_csv"),
        br(),
        actionButton("importar_csv", "Importar base", class = "btn-primary")
    )
  })

  parametros_csv <- reactive({
    req(identical(formato_atual(), "csv"), input$separador_csv,
        !is.null(input$delimitador_csv), input$codificacao_csv)
    separador <- if (identical(input$separador_csv, "outro")) {
      req(input$separador_personalizado)
      input$separador_personalizado
    } else {
      input$separador_csv
    }
    if (identical(separador, "\\t")) separador <- "\t"
    validar_parametros_csv(separador, input$delimitador_csv)
    list(separador = separador, delimitador = input$delimitador_csv,
         codificacao = input$codificacao_csv)
  })

  previa_csv <- reactive({
    req(input$arquivo)
    tryCatch(
      {
        parametros <- parametros_csv()
        ler_csv_configurado(
          input$arquivo$datapath,
          separador = parametros$separador,
          delimitador = parametros$delimitador,
          codificacao = parametros$codificacao,
          n_max = 20L
        )
      },
      error = function(e) structure(list(erro = e), class = "erro_previa_csv")
    )
  })

  output$status_previa_csv <- renderUI({
    resultado <- previa_csv()
    if (inherits(resultado, "erro_previa_csv")) {
      return(div(class = "erro-leitura",
                 mensagem_erro_importacao(resultado$erro, "CSV")))
    }
    avisos <- resultado$avisos
    tagList(
      tags$p(tags$strong(format(resultado$numero_colunas, big.mark = ".", decimal.mark = ",")),
             if (resultado$numero_colunas == 1L) " coluna identificada na amostra" else
               " colunas identificadas na amostra"),
      if (length(avisos)) div(class = "alerta-leitura", lapply(avisos, tags$div))
    )
  })

  output$tabela_previa_csv <- renderDT({
    resultado <- previa_csv()
    req(!inherits(resultado, "erro_previa_csv"))
    datatable(resultado$dados, rownames = FALSE,
              options = list(scrollX = TRUE, pageLength = 20, dom = "t", autoWidth = TRUE))
  }, server = FALSE)

  observeEvent(input$importar_csv, {
    req(input$arquivo, identical(formato_atual(), "csv"))
    withProgress(message = "Importando o arquivo CSV completo...", value = 0.2, {
      tryCatch({
        parametros <- parametros_csv()
        resultado <- ler_csv_configurado(
          input$arquivo$datapath,
          separador = parametros$separador,
          delimitador = parametros$delimitador,
          codificacao = parametros$codificacao,
          n_max = Inf,
          mostrar_progresso = FALSE
        )
        incProgress(0.7)
        concluir_importacao(resultado$dados, input$arquivo$name, "csv")
        incProgress(0.1)
      }, error = function(e) registrar_erro(e, "CSV"))
    })
  })

  # Resumo da importação e apresentação de erros/avisos ao usuário.
  output$mensagem_erro <- renderUI({
    req(erro_interface())
    div(class = "erro-leitura", erro_interface())
  })

  output$resumo_importacao <- renderUI({
    info <- metadados_importados()
    req(info)
    div(class = "painel",
        h3("Base importada"),
        div(class = "resumo",
            div(class = "resumo-item", span("Arquivo"), strong(info$arquivo)),
            div(class = "resumo-item", span("Formato"), strong(info$formato)),
            div(class = "resumo-item", span("Registros"),
                strong(format(info$registros, big.mark = ".", decimal.mark = ",",
                              scientific = FALSE))),
            div(class = "resumo-item", span("Variáveis"),
                strong(format(info$variaveis, big.mark = ".", decimal.mark = ",",
                              scientific = FALSE))),
            if (!is.null(info$planilha))
              div(class = "resumo-item", span("Planilha"), strong(info$planilha))))
  })

  # Padronização ---------------------------------------------------------------
  # O botão só aparece quando há uma base importada; a escolha de codificação é
  # encaminhada ao pipeline e qualquer lista de erros é preservada integralmente.
  output$acao_padronizacao <- renderUI({
    info <- metadados_importados()
    req(base_bruta(), info, info$formato %in% c("CSV", "DTA", "DBF"))
    div(class = "painel",
        h3("Preparação e padronização da base"),
        p(paste(
          "A base bruta será mantida até a conclusão da padronização.",
          "Depois disso, somente a base padronizada e uma prévia serão conservadas em memória."
        )),
        if (info$formato %in% c("DBF", "DTA"))
          tagList(
            selectInput(
              "codificacao_padronizacao",
              "Codificação dos campos de texto",
              choices = c(
                "Automática (UTF-8 com fallback Windows-1252)" = "auto",
                "Windows-1252" = "Windows-1252",
                "Latin-1 / ISO-8859-1" = "ISO-8859-1",
                "UTF-8" = "UTF-8"
              ),
              selected = "auto"
            ),
            tags$p(
              class = "formatos",
              "No modo automático, textos UTF-8 válidos são preservados e somente sequências inválidas são interpretadas como Windows-1252."
            )
          ),
        actionButton("padronizar_base", "Padronizar base", class = "btn-primary"))
  })

  observeEvent(input$padronizar_base, {
    req(base_bruta())
    base_padronizada(NULL)
    resultado_padronizacao(NULL)
    resultado_enriquecimento(NULL)
    erro_enriquecimento(NULL)
    resultado_validade(NULL)
    resultado_consistencia(NULL)
    limpar_artefato_sessao(artefato_relatorio, prefixo_relatorio_validade)
    erro_relatorio(NULL)
    limpar_artefato_sessao(
      artefato_relatorio_consistencia, prefixo_relatorio_consistencia
    )
    erro_relatorio_consistencia(NULL)
    erro_validade(NULL)
    erro_consistencia(NULL)
    erros_padronizacao(character())
    withProgress(message = "Preparando e padronizando a base...", value = 0.1, {
      tryCatch({
        # Em modo de desenvolvimento, incorpora alterações nas regras e no
        # motor de padronização sem exigir nova importação do arquivo original.
        recarregar_modulos_desenvolvimento(
          "padronizacao", ambiente_aplicativo, diretorio_projeto_aplicativo
        )
        info <- metadados_importados()
        codificacao <- if (is.null(info) || !info$formato %in% c("DBF", "DTA") ||
                            is.null(input$codificacao_padronizacao)) {
          "auto"
        } else {
          input$codificacao_padronizacao
        }
        resultado <- padronizar_base(
          base_bruta(), codificacao_texto = codificacao
        )
        incProgress(0.8)
        base_final <- resultado$base_padronizada
        resultado_compacto <- compactar_resultado_padronizacao(resultado)
        base_padronizada(base_final)
        resultado_padronizacao(resultado_compacto)
        # A prévia de 20 linhas basta para auditoria visual; a base bruta
        # completa deixa de ser necessária após uma padronização bem-sucedida.
        base_bruta(NULL)
        resultado_enriquecimento(NULL)
        erro_enriquecimento(NULL)
        resultado_validade(NULL)
        resultado_consistencia(NULL)
        limpar_artefato_sessao(artefato_relatorio, prefixo_relatorio_validade)
        erro_relatorio(NULL)
        limpar_artefato_sessao(
          artefato_relatorio_consistencia, prefixo_relatorio_consistencia
        )
        erro_relatorio_consistencia(NULL)
        erro_validade(NULL)
        erro_consistencia(NULL)
        erros_padronizacao(character())
        erro_interface(NULL)
        rm(resultado, resultado_compacto, base_final)
        liberar_memoria_ram(completa = TRUE)
        incProgress(0.1)
      }, error = function(e) {
        message(sprintf("[Padronização] %s", conditionMessage(e)))
        erros_padronizacao(extrair_erros_padronizacao(e))
      })
    })
  })

  # Saídas da padronização: erros, prévias, totais, ausências e auditoria.
  output$mensagem_erros_padronizacao <- renderUI({
    erros <- erros_padronizacao()
    req(length(erros) > 0L)
    div(class = "erro-leitura",
        h4("A padronização não pôde ser concluída"),
        p(sprintf(
          "%s %s encontrado%s. A base bruta foi preservada:",
          length(erros),
          if (length(erros) == 1L) "erro foi" else "erros foram",
          if (length(erros) == 1L) "" else "s"
        )),
        tags$ul(lapply(erros, tags$li)))
  })

  output$secao_previa_final <- renderUI({
    req(previa_base_bruta())
    div(class = "painel", h3("Prévia da base bruta importada"), DTOutput("tabela_final"))
  })

  output$tabela_final <- renderDT({
    req(previa_base_bruta())
    datatable(previa_base_bruta(), rownames = FALSE,
              options = list(scrollX = TRUE, pageLength = 20, dom = "t", autoWidth = TRUE))
  }, server = FALSE)

  output$resumo_padronizacao <- renderUI({
    resultado <- resultado_padronizacao()
    req(resultado)
    resumo <- resultado$resumo
    item_resumo <- function(rotulo, valor) {
      div(class = "resumo-item", span(rotulo),
          strong(format(valor, big.mark = ".", decimal.mark = ",", scientific = FALSE)))
    }
    div(class = "painel",
        h3("Resultado da padronização"),
        div(class = "resumo",
            item_resumo("Registros", resumo$registros),
            item_resumo("Variáveis", resumo$variaveis),
            item_resumo("Esperadas presentes", resumo$variaveis_presentes),
            item_resumo("Esperadas ausentes", resumo$variaveis_ausentes),
            item_resumo("Códigos inválidos removidos", resumo$codigos_invalidos),
            item_resumo("Datas não convertidas", resumo$datas_invalidas),
            item_resumo("Municípios inválidos removidos", resumo$municipios_invalidos)))
  })

  # Enriquecimento CNES --------------------------------------------------------
  # Uma única fotografia mensal é selecionada a partir de `nu_ano` e
  # `dt_notific`. O vínculo posterior usa exclusivamente o código da unidade.
  output$acao_enriquecimento <- renderUI({
    req(base_padronizada(), resultado_padronizacao())
    div(class = "painel",
        h3("Enriquecimento com o CNES"),
        p(paste(
          "Baixe a competência mensal mais recente aplicável e acrescente o",
          "tipo da unidade notificadora à base padronizada."
        )),
        actionButton(
          "enriquecer_base", "Enriquecer", class = "btn-primary"
        ))
  })

  observeEvent(input$enriquecer_base, {
    req(base_padronizada(), resultado_padronizacao())
    erro_enriquecimento(NULL)
    withProgress(message = "Baixando e relacionando estabelecimentos do CNES...", value = 0.1, {
      tryCatch({
        recarregar_modulos_desenvolvimento(
          "enriquecimento", ambiente_aplicativo, diretorio_projeto_aplicativo
        )
        resultado <- enriquecer_base_cnes(base_padronizada())
        incProgress(0.8)
        base_enriquecida <- resultado$base_enriquecida
        resultado_compacto <- compactar_resultado_enriquecimento(resultado)
        base_padronizada(base_enriquecida)
        resultado_enriquecimento(resultado_compacto)
        resultado_validade(NULL)
        resultado_consistencia(NULL)
        limpar_artefato_sessao(artefato_relatorio, prefixo_relatorio_validade)
        erro_relatorio(NULL)
        limpar_artefato_sessao(
          artefato_relatorio_consistencia, prefixo_relatorio_consistencia
        )
        erro_relatorio_consistencia(NULL)
        erro_validade(NULL)
        erro_consistencia(NULL)
        rm(resultado, resultado_compacto, base_enriquecida)
        liberar_memoria_ram(completa = TRUE)
        incProgress(0.1)
      }, error = function(e) {
        message(sprintf("[Enriquecimento CNES] %s", conditionMessage(e)))
        erro_enriquecimento(conditionMessage(e))
      })
    })
  })

  output$mensagem_enriquecimento <- renderUI({
    erro <- erro_enriquecimento()
    req(!is.null(erro), nzchar(erro))
    div(class = "erro-leitura",
        h4("O enriquecimento não pôde ser concluído"),
        p(erro),
        p("A base padronizada anterior foi preservada."))
  })

  output$resumo_enriquecimento <- renderUI({
    resultado <- resultado_enriquecimento()
    req(resultado)
    resumo <- resultado$resumo
    item_resumo <- function(rotulo, valor) {
      div(class = "resumo-item", span(rotulo), strong(valor))
    }
    problemas <- c(
      if (resumo$anos_ausentes > 0L || resumo$anos_invalidos > 0L)
        sprintf("Anos de notificação: %s ausente(s) e %s inválido(s).",
                resumo$anos_ausentes, resumo$anos_invalidos),
      if (resumo$unidades_nao_localizadas > 0L)
        sprintf("%s registro(s) possuem CNES válido sem correspondência na competência.",
                resumo$unidades_nao_localizadas),
      if (resumo$ids_ausentes_ou_invalidos > 0L)
        sprintf("%s registro(s) possuem id_unidade ausente ou inválido.",
                resumo$ids_ausentes_ou_invalidos),
      if (resumo$datas_ausentes > 0L || resumo$datas_invalidas > 0L)
        sprintf("Datas de notificação: %s ausente(s) e %s inválida(s).",
                resumo$datas_ausentes, resumo$datas_invalidas),
      if (resumo$divergencias_ano_data > 0L)
        sprintf("%s divergência(s) entre nu_ano e o ano de dt_notific.",
                resumo$divergencias_ano_data),
      if (resumo$cnes_invalidos_descartados > 0L)
        sprintf("%s código(s) inválido(s) foram descartados da referência CNES.",
                resumo$cnes_invalidos_descartados),
      if (resumo$duplicacoes_reduzidas > 0L)
        sprintf("%s duplicação(ões) equivalentes foram reduzidas na referência CNES.",
                resumo$duplicacoes_reduzidas)
    )
    div(class = "painel",
        h3("Resultado do enriquecimento"),
        div(class = "status-sucesso",
            sprintf("Enriquecimento concluído com o CNES da competência %s.",
                    resumo$competencia)),
        div(class = "resumo",
            item_resumo("Competência CNES", resumo$competencia),
            item_resumo("Registros preservados", resumo$registros),
            item_resumo("Registros com unidade localizada", resumo$unidades_localizadas),
            item_resumo("Registros sem correspondência", resumo$unidades_nao_localizadas)),
        if (length(problemas))
          div(class = "alerta-leitura", h4("Pontos de atenção"),
              tags$ul(lapply(problemas, tags$li))))
  })

  output$variaveis_ausentes_ui <- renderUI({
    resultado <- resultado_padronizacao()
    req(resultado)
    div(class = "painel",
        h3("Variáveis esperadas ausentes"),
        if (nrow(resultado$variaveis_ausentes) == 0L)
          p("Todas as variáveis esperadas estavam presentes na base importada.")
        else
          DTOutput("tabela_variaveis_ausentes"))
  })

  output$tabela_variaveis_ausentes <- renderDT({
    resultado <- resultado_padronizacao()
    req(resultado, nrow(resultado$variaveis_ausentes) > 0L)
    datatable(resultado$variaveis_ausentes, rownames = FALSE,
              colnames = c("Variável"),
              options = list(pageLength = 10, dom = "tip", autoWidth = TRUE))
  }, server = FALSE)

  output$auditoria_ui <- renderUI({
    req(resultado_padronizacao())
    div(class = "painel", h3("Auditoria da padronização"), DTOutput("tabela_auditoria"))
  })

  output$tabela_auditoria <- renderDT({
    resultado <- resultado_padronizacao()
    req(resultado)
    datatable(
      resultado$auditoria,
      rownames = FALSE,
      colnames = c("Variável", "Transformação", "Valores alterados", "Valores inválidos"),
      options = list(scrollX = TRUE, pageLength = 10, autoWidth = TRUE)
    )
  }, server = FALSE)

  output$secao_previa_padronizada <- renderUI({
    req(base_padronizada())
    div(class = "painel", h3("Prévia da base padronizada"),
        DTOutput("tabela_padronizada"))
  })

  output$tabela_padronizada <- renderDT({
    req(base_padronizada())
    datatable(utils::head(base_padronizada(), 20L), rownames = FALSE,
              options = list(scrollX = TRUE, pageLength = 20, dom = "t", autoWidth = TRUE))
  }, server = FALSE)

  # Análise de validade --------------------------------------------------------
  # Disponibiliza a execução somente após padronização e mantém resultados
  # vinculados à versão atual da base.
  output$estado_modulo_validade <- renderUI({
    if (is.null(base_padronizada()) || is.null(resultado_padronizacao())) {
      return(div(
        class = "painel aviso-modulo",
        h3("Base padronizada não disponível"),
        p("É necessário importar e padronizar uma base antes de executar a Análise de Validade."),
        actionButton("validade_ir_importacao", "Ir para Importação e Padronização",
                     class = "btn-primary")
      ))
    }
    dados <- base_padronizada()
    div(
      class = "painel",
      h3("Base disponível para análise"),
      div(class = "resumo",
          div(class = "resumo-item", span("Registros"),
              strong(format(nrow(dados), big.mark = ".", decimal.mark = ","))),
          div(class = "resumo-item", span("Variáveis"),
              strong(format(ncol(dados), big.mark = ".", decimal.mark = ",")))),
      br(),
      actionButton("executar_validade", "Executar Análise de Validade",
                   class = "btn-primary"),
      if (!is.null(erro_validade()))
        div(class = "erro-leitura", style = "margin-top: 16px;", erro_validade())
    )
  })

  observeEvent(input$executar_validade, {
    req(base_padronizada(), resultado_padronizacao())
    erro_validade(NULL)
    resultado_validade(NULL)
    limpar_artefato_sessao(artefato_relatorio, prefixo_relatorio_validade)
    erro_relatorio(NULL)
    liberar_memoria_ram()
    withProgress(message = "Executando regras de validade...", value = 0.15, {
      tryCatch({
        # Atualiza somente as regras e o motor da análise de validade. A base
        # padronizada que já está na sessão é preservada e reutilizada.
        recarregar_modulos_desenvolvimento(
          "validade", ambiente_aplicativo, diretorio_projeto_aplicativo
        )
        resultado <- executar_analise_validade(
          base_padronizada(),
          resultado_padronizacao()$variaveis_originais
        )
        incProgress(0.8)
        resultado_validade(resultado)
        rm(resultado)
        liberar_memoria_ram()
        incProgress(0.05)
      }, error = function(e) {
        message(sprintf("[Validade] %s", conditionMessage(e)))
        resultado_validade(NULL)
        erro_validade(conditionMessage(e))
      })
    })
  })

  # Totais, avisos, tabela por regra, gráfico e registros sinalizados.
  output$resumo_validade_ui <- renderUI({
    resultado <- resultado_validade()
    req(resultado)
    resumo <- resultado$resumo
    item <- function(rotulo, valor) {
      div(class = "resumo-item", span(rotulo), strong(valor))
    }
    div(
      class = "painel",
      h3("Resumo dos resultados"),
      div(class = "resumo",
          item("Notificações analisadas",
               format(resumo$notificacoes, big.mark = ".", decimal.mark = ",")),
          item("Registros com problema",
               format(resumo$registros_problematicos, big.mark = ".", decimal.mark = ",")),
          item("Percentual da base",
               paste0(format(round(resumo$percentual_problematicos, 2), nsmall = 2,
                             decimal.mark = ","), "%")),
          item("Ocorrências de problemas",
               format(resumo$ocorrencias, big.mark = ".", decimal.mark = ","))),
      p(class = "observacao",
        "Um registro pode atender a mais de uma regra; por isso, ocorrências podem superar registros problemáticos.")
    )
  })

  output$avisos_validade_ui <- renderUI({
    resultado <- resultado_validade()
    req(resultado, length(resultado$avisos) > 0L)
    div(class = "alerta-leitura",
        h4("Regras não executáveis"),
        tags$ul(lapply(resultado$avisos, tags$li)))
  })

  output$resultado_regras_validade_ui <- renderUI({
    req(resultado_validade())
    div(class = "painel", h3("Resultado por regra"), DTOutput("tabela_regras_validade"))
  })

  output$tabela_regras_validade <- renderDT({
    resultado <- resultado_validade()
    req(resultado)
    tabela <- resultado$por_regra
    tabela$registros_sinalizados <- ifelse(
      is.na(tabela$registros_sinalizados), "—",
      format(tabela$registros_sinalizados, big.mark = ".", decimal.mark = ",")
    )
    tabela$percentual_total <- ifelse(
      is.na(tabela$percentual_total), "—",
      paste0(format(round(tabela$percentual_total, 2), nsmall = 2,
                    decimal.mark = ","), "%")
    )
    datatable(
      tabela,
      rownames = FALSE,
      colnames = c("Regra", "Descrição", "Status", "Detalhe",
                   "Registros sinalizados", "% do total"),
      options = list(scrollX = TRUE, pageLength = 10, dom = "t", autoWidth = TRUE)
    )
  }, server = FALSE)

  output$grafico_validade_ui <- renderUI({
    req(resultado_validade())
    div(class = "painel", h3("Registros sinalizados por regra"),
        plotOutput("grafico_regras_validade", height = "360px"))
  })

  output$grafico_regras_validade <- renderPlot({
    resultado <- resultado_validade()
    req(resultado)
    tabela <- subset(resultado$por_regra,
                     status == "Executada" & !is.na(registros_sinalizados))
    if (!nrow(tabela)) {
      plot.new()
      text(0.5, 0.5, "Nenhuma regra pôde ser executada.")
      return(invisible())
    }
    tabela <- tabela[order(tabela$registros_sinalizados), , drop = FALSE]
    valores <- tabela$registros_sinalizados
    limite <- max(c(valores, 1)) * 1.18
    par(mar = c(4, 7, 2, 2))
    posicoes <- barplot(
      valores,
      names.arg = paste("Regra", tabela$regra),
      horiz = TRUE,
      las = 1,
      col = cor_viva_quali("principal"),
      border = NA,
      xlim = c(0, limite),
      xlab = "Registros sinalizados"
    )
    text(valores, posicoes,
         labels = format(valores, big.mark = ".", decimal.mark = ","),
         pos = 4, cex = 0.9)
  })

  output$registros_validade_ui <- renderUI({
    resultado <- resultado_validade()
    req(resultado)
    regras_executadas <- subset(resultado$por_regra, status == "Executada")
    escolhas_regras <- stats::setNames(
      as.character(regras_executadas$regra),
      paste0("Regra ", regras_executadas$regra, " — ", regras_executadas$descricao)
    )
    div(
      class = "painel",
      h3("Registros sinalizados"),
      selectInput(
        "filtro_regra_validade", "Registros a apresentar",
        choices = c("Todos os registros com problema" = "todos", escolhas_regras),
        selected = "todos"
      ),
      DTOutput("tabela_registros_validade")
    )
  })

  output$tabela_registros_validade <- renderDT({
    resultado <- resultado_validade()
    req(resultado, input$filtro_regra_validade)
    tabela <- obter_registros_validade(
      base_padronizada(), resultado, input$filtro_regra_validade
    )
    datatable(
      tabela,
      rownames = FALSE,
      options = list(scrollX = TRUE, pageLength = 15, autoWidth = TRUE)
    )
  }, server = TRUE)

  # Análise de consistência ----------------------------------------------------
  # Replica o fluxo de validade, acrescentando a organização por grupos.
  output$estado_modulo_consistencia <- renderUI({
    if (is.null(base_padronizada()) || is.null(resultado_padronizacao())) {
      return(div(
        class = "painel aviso-modulo",
        h3("Base padronizada não disponível"),
        p("É necessário importar e padronizar uma base antes de executar a Análise de Consistência."),
        actionButton("consistencia_ir_importacao", "Ir para Importação e Padronização",
                     class = "btn-primary")
      ))
    }
    dados <- base_padronizada()
    div(
      class = "painel",
      h3("Base disponível para análise"),
      div(class = "resumo",
          div(class = "resumo-item", span("Registros"),
              strong(format(nrow(dados), big.mark = ".", decimal.mark = ","))),
          div(class = "resumo-item", span("Variáveis"),
              strong(format(ncol(dados), big.mark = ".", decimal.mark = ",")))),
      br(),
      actionButton("executar_consistencia", "Executar Análise de Consistência",
                   class = "btn-primary"),
      if (!is.null(erro_consistencia()))
        div(class = "erro-leitura", style = "margin-top: 16px;", erro_consistencia())
    )
  })

  observeEvent(input$executar_consistencia, {
    req(base_padronizada(), resultado_padronizacao())
    erro_consistencia(NULL)
    resultado_consistencia(NULL)
    limpar_artefato_sessao(
      artefato_relatorio_consistencia, prefixo_relatorio_consistencia
    )
    erro_relatorio_consistencia(NULL)
    liberar_memoria_ram()
    withProgress(message = "Executando regras de consistência...", value = 0.15, {
      tryCatch({
        # Atualiza somente as regras e o motor da análise de consistência. A
        # base padronizada permanece carregada para acelerar os testes.
        recarregar_modulos_desenvolvimento(
          "consistencia", ambiente_aplicativo, diretorio_projeto_aplicativo
        )
        resultado <- executar_analise_consistencia(
          base_padronizada(),
          resultado_padronizacao()$variaveis_originais
        )
        incProgress(0.8)
        resultado_consistencia(resultado)
        rm(resultado)
        liberar_memoria_ram()
        incProgress(0.05)
      }, error = function(e) {
        message(sprintf("[Consistência] %s", conditionMessage(e)))
        resultado_consistencia(NULL)
        erro_consistencia(conditionMessage(e))
      })
    })
  })

  # Totais, avisos e tabelas hierárquicas de grupos e regras.
  output$resumo_consistencia_ui <- renderUI({
    resultado <- resultado_consistencia()
    req(resultado)
    resumo <- resultado$resumo
    item <- function(rotulo, valor) {
      div(class = "resumo-item", span(rotulo), strong(valor))
    }
    div(
      class = "painel",
      h3("Resumo dos resultados"),
      div(class = "resumo",
          item("Notificações analisadas",
               format(resumo$notificacoes, big.mark = ".", decimal.mark = ",")),
          item("Registros com problema de consistência",
               format(resumo$registros_problematicos, big.mark = ".", decimal.mark = ",")),
          item("Percentual da base",
               paste0(format(round(resumo$percentual_problematicos, 2), nsmall = 2,
                             decimal.mark = ","), "%")),
          item("Ocorrências de problemas de consistência",
               format(resumo$ocorrencias, big.mark = ".", decimal.mark = ","))),
      p(class = "observacao",
        "Um registro pode atender a mais de uma regra; por isso, ocorrências podem superar registros problemáticos.")
    )
  })

  output$avisos_consistencia_ui <- renderUI({
    resultado <- resultado_consistencia()
    req(resultado, length(resultado$avisos) > 0L)
    div(class = "alerta-leitura",
        h4("Regras não executáveis"),
        tags$ul(lapply(resultado$avisos, tags$li)))
  })

  output$resultado_regras_consistencia_ui <- renderUI({
    resultado <- resultado_consistencia()
    req(resultado)
    formatar_numero <- function(x) {
      if (is.na(x)) "—" else format(x, big.mark = ".", decimal.mark = ",")
    }
    formatar_percentual <- function(x) {
      if (is.na(x)) "—" else paste0(format(round(x, 2), nsmall = 2, decimal.mark = ","), "%")
    }
    grupos_ordenados <- resultado$por_grupo[
      order(-ifelse(is.na(resultado$por_grupo$registros_sinalizados), -1,
                    resultado$por_grupo$registros_sinalizados)), , drop = FALSE
    ]
    paineis <- lapply(seq_len(nrow(grupos_ordenados)), function(i) {
      grupo <- grupos_ordenados[i, ]
      regras <- resultado$por_regra[
        resultado$por_regra$grupo_id == grupo$grupo_id, , drop = FALSE
      ]
      regras <- regras[order(-ifelse(is.na(regras$registros_sinalizados), -1,
                                     regras$registros_sinalizados)), , drop = FALSE]
      linhas <- lapply(seq_len(nrow(regras)), function(j) {
        regra <- regras[j, ]
        tags$tr(
          tags$td(paste("Regra", regra$regra)),
          tags$td(regra$descricao),
          tags$td(regra$status),
          tags$td(regra$detalhe),
          tags$td(style = "text-align: right;", formatar_numero(regra$registros_sinalizados)),
          tags$td(style = "text-align: right;", formatar_percentual(regra$percentual_total))
        )
      })
      tags$details(
        class = "grupo-consistencia",
        tags$summary(
          tags$strong(grupo$grupo),
          tags$span(paste0(
            formatar_numero(grupo$registros_sinalizados), " registros; ",
            formatar_numero(grupo$ocorrencias), " ocorrências"
          ))
        ),
        tags$table(
          class = "table table-striped table-hover",
          tags$thead(tags$tr(
            tags$th("Regra"), tags$th("Descrição"), tags$th("Status"),
            tags$th("Detalhe"), tags$th("Registros sinalizados"), tags$th("% do total")
          )),
          tags$tbody(linhas)
        )
      )
    })
    div(
      class = "painel",
      h3("Resultado por grupo e por regra"),
      p(class = "observacao",
        "Selecione um grupo para expandir ou reduzir suas regras. O total do grupo conta cada registro uma única vez."),
      paineis
    )
  })

  # O gráfico alterna entre visão geral por grupo e regras do grupo escolhido.
  output$grafico_consistencia_ui <- renderUI({
    resultado <- resultado_consistencia()
    req(resultado)
    altura <- max(560L, 25L * (nrow(resultado$por_regra) + nrow(resultado$por_grupo)))
    div(class = "painel", h3("Registros sinalizados por grupo e por regra"),
        p(class = "observacao",
          "As barras escuras representam grupos; as barras claras representam as regras aninhadas."),
        plotOutput("grafico_regras_consistencia", height = paste0(altura, "px")))
  })

  output$grafico_regras_consistencia <- renderPlot({
    resultado <- resultado_consistencia()
    req(resultado)
    grupos <- subset(resultado$por_grupo, !is.na(registros_sinalizados))
    if (!nrow(grupos)) {
      plot.new()
      text(0.5, 0.5, "Nenhuma regra pôde ser executada.")
      return(invisible())
    }
    grupos <- grupos[order(-grupos$registros_sinalizados), , drop = FALSE]
    partes <- lapply(seq_len(nrow(grupos)), function(i) {
      grupo <- grupos[i, ]
      regras <- resultado$por_regra[
        resultado$por_regra$grupo_id == grupo$grupo_id &
          resultado$por_regra$status == "Executada", , drop = FALSE
      ]
      regras <- regras[order(-regras$registros_sinalizados), , drop = FALSE]
      rbind(
        data.frame(
          rotulo = grupo$grupo,
          valor = grupo$registros_sinalizados,
          tipo = "Grupo",
          stringsAsFactors = FALSE
        ),
        data.frame(
          rotulo = paste0("   Regra ", regras$regra),
          valor = regras$registros_sinalizados,
          tipo = "Regra",
          stringsAsFactors = FALSE
        )
      )
    })
    dados <- do.call(rbind, partes)
    dados <- dados[rev(seq_len(nrow(dados))), , drop = FALSE]
    limite <- max(c(dados$valor, 1)) * 1.2
    par(mar = c(4, 24, 2, 2))
    posicoes <- barplot(
      dados$valor,
      names.arg = dados$rotulo,
      horiz = TRUE,
      las = 1,
      col = ifelse(
        dados$tipo == "Grupo",
        cor_viva_quali("principal"), cor_viva_quali("principal_clara")
      ),
      border = NA,
      xlim = c(0, limite),
      xlab = "Registros sinalizados",
      cex.names = 0.82
    )
    text(dados$valor, posicoes,
         labels = format(dados$valor, big.mark = ".", decimal.mark = ","),
         pos = 4, cex = 0.8)
  })

  output$registros_consistencia_ui <- renderUI({
    resultado <- resultado_consistencia()
    req(resultado)
    regras_executadas <- subset(resultado$por_regra, status == "Executada")
    escolhas_regras <- stats::setNames(
      regras_executadas$regra,
      paste0("Regra ", regras_executadas$regra, " — ", regras_executadas$descricao)
    )
    div(
      class = "painel",
      h3("Registros sinalizados"),
      selectInput(
        "filtro_regra_consistencia", "Registros a apresentar",
        choices = c("Todos os registros com problema de consistência" = "todos",
                    escolhas_regras),
        selected = "todos"
      ),
      DTOutput("tabela_registros_consistencia")
    )
  })

  output$tabela_registros_consistencia <- renderDT({
    resultado <- resultado_consistencia()
    req(resultado, input$filtro_regra_consistencia)
    tabela <- obter_registros_consistencia(
      base_padronizada(), resultado, input$filtro_regra_consistencia
    )
    datatable(
      tabela,
      rownames = FALSE,
      options = list(scrollX = TRUE, pageLength = 15, autoWidth = TRUE)
    )
  }, server = TRUE)

  # Relatório de consistência --------------------------------------------------
  # Carrega referências para preencher as opções territoriais da interface.
  referencias_selecao_relatorio_consistencia <- reactive({
    req(resultado_consistencia())
    carregar_referencias_territoriais(
      diretorio = file.path(diretorio_projeto_aplicativo, "data", "reference"),
      carregar_geografia = FALSE, carregar_cnes = FALSE
    )
  })

  output$relatorio_consistencia_ui <- renderUI({
    req(resultado_consistencia())
    referencias <- tryCatch(
      referencias_selecao_relatorio_consistencia(), error = function(e) e
    )
    if (inherits(referencias, "error")) {
      return(div(
        class = "painel erro-leitura",
        h3("Relatório de Consistência indisponível"),
        conditionMessage(referencias)
      ))
    }
    ufs <- referencias$ufs
    regioes <- ufs[!duplicated(ufs$codigo_regiao), , drop = FALSE]
    escolhas_regioes <- stats::setNames(
      regioes$codigo_regiao,
      paste0(regioes$codigo_regiao, " — ", regioes$nome_regiao)
    )
    escolhas_ufs <- stats::setNames(
      ufs$codigo_uf,
      paste0(ufs$codigo_uf, " — ", ufs$nome_uf, " (", ufs$sigla_uf, ")")
    )
    escolhas_municipios <- listar_municipios_disponiveis(
      base_padronizada(), referencias
    )
    municipios_selecionados <- isolate(
      if (is.null(input$relatorio_consistencia_municipios)) character()
      else input$relatorio_consistencia_municipios
    )
    municipios_selecionados <- intersect(
      municipios_selecionados, unname(escolhas_municipios)
    )
    session$onFlushed(function() {
      updateSelectizeInput(
        session, "relatorio_consistencia_municipios",
        choices = escolhas_municipios, selected = municipios_selecionados,
        server = TRUE
      )
    }, once = TRUE)
    artefato <- artefato_relatorio_consistencia()
    div(
      class = "painel",
      h3("Relatório de Consistência"),
      p("Selecione uma ou mais abrangências para compor o relatório."),
      checkboxInput(
        "relatorio_consistencia_incluir_brasil",
        "Incluir análise nacional — Brasil",
        value = isolate(isTRUE(input$relatorio_consistencia_incluir_brasil))
      ),
      fluidRow(
        column(6, selectizeInput(
          "relatorio_consistencia_regioes", "Regiões detalhadas",
          choices = escolhas_regioes,
          selected = isolate(if (is.null(input$relatorio_consistencia_regioes)) {
            character()
          } else input$relatorio_consistencia_regioes),
          multiple = TRUE,
          options = list(placeholder = "Selecione uma ou mais regiões")
        )),
        column(6, selectizeInput(
          "relatorio_consistencia_ufs", "UFs detalhadas",
          choices = escolhas_ufs,
          selected = isolate(if (is.null(input$relatorio_consistencia_ufs)) {
            character()
          } else input$relatorio_consistencia_ufs),
          multiple = TRUE,
          options = list(placeholder = "Selecione uma ou mais UFs")
        ))
      ),
      selectizeInput(
        "relatorio_consistencia_municipios", "Municípios detalhados",
        choices = NULL, selected = municipios_selecionados, multiple = TRUE,
        options = list(
          placeholder = "Busque por nome, código ou sigla da UF",
          maxOptions = 100L
        )
      ),
      selectInput(
        "relatorio_consistencia_escala_matriz", "Escala das matrizes",
        choices = stats::setNames(
          names(configuracao_priorizacao()$rotulos_escalas),
          unname(configuracao_priorizacao()$rotulos_escalas)
        ),
        selected = isolate(if (is.null(input$relatorio_consistencia_escala_matriz)) {
          configuracao_priorizacao()$escala_padrao
        } else input$relatorio_consistencia_escala_matriz)
      ),
      div(
        class = "acoes-relatorio",
        actionButton("consistencia_selecionar_ufs_regioes", "Selecionar UFs das regiões"),
        actionButton("consistencia_todas_regioes", "Todas as regiões"),
        actionButton("consistencia_todas_ufs", "Todas as UFs"),
        actionButton("consistencia_limpar_relatorio", "Limpar seleção")
      ),
      p(
        class = "observacao",
        "O detalhamento de uma região sempre compara todas as UFs que a compõem. No HTML, os resultados podem alternar entre apenas grupos e todas as regras."
      ),
      actionButton(
        "gerar_relatorio_consistencia", "Gerar relatório", class = "btn-primary"
      ),
      if (!is.null(erro_relatorio_consistencia()))
        div(
          class = "erro-leitura", style = "margin-top:14px;",
          erro_relatorio_consistencia()
        ),
      if (!is.null(artefato)) tagList(
        div(
          class = "status-sucesso",
          strong("Relatório gerado com sucesso."),
          tags$div(paste("Período:", artefato$config_relatorio$periodo)),
          tags$div(paste(
            "Geração:",
            format(artefato$config_relatorio$gerado_em, "%d/%m/%Y %H:%M:%S")
          )),
          tags$div(paste(
            "Abrangências:", artefato$config_relatorio$descricao_abrangencias
          )),
          tags$div(paste(
            "Escala das matrizes no PDF:",
            configuracao_priorizacao()$rotulos_escalas[[
              artefato$config_relatorio$escala_matriz_prioridade
            ]]
          ))
        ),
        div(
          class = "acoes-relatorio",
          downloadButton("baixar_relatorio_consistencia_html", "Baixar HTML"),
          if (!is.null(artefato$caminho_pdf))
            downloadButton("baixar_relatorio_consistencia_pdf", "Baixar PDF")
        ),
        h4("Visualização do relatório"),
        tags$iframe(
          class = "visualizador-relatorio", src = artefato$url_relativa,
          title = "Visualização do Relatório de Consistência"
        )
      )
    )
  })

  # Atalhos de seleção territorial do relatório de consistência.
  observeEvent(input$consistencia_todas_regioes, {
    referencias <- referencias_selecao_relatorio_consistencia()
    updateSelectizeInput(
      session, "relatorio_consistencia_regioes",
      selected = unique(referencias$ufs$codigo_regiao)
    )
  })

  observeEvent(input$consistencia_todas_ufs, {
    referencias <- referencias_selecao_relatorio_consistencia()
    updateSelectizeInput(
      session, "relatorio_consistencia_ufs",
      selected = referencias$ufs$codigo_uf
    )
  })

  observeEvent(input$consistencia_selecionar_ufs_regioes, {
    referencias <- referencias_selecao_relatorio_consistencia()
    regioes <- input$relatorio_consistencia_regioes
    if (!length(regioes)) {
      showNotification("Selecione ao menos uma região.", type = "warning")
      return()
    }
    codigos <- referencias$ufs$codigo_uf[
      referencias$ufs$codigo_regiao %in% regioes
    ]
    updateSelectizeInput(
      session, "relatorio_consistencia_ufs", selected = codigos
    )
  })

  observeEvent(input$consistencia_limpar_relatorio, {
    updateCheckboxInput(
      session, "relatorio_consistencia_incluir_brasil", value = FALSE
    )
    updateSelectizeInput(
      session, "relatorio_consistencia_regioes", selected = character()
    )
    updateSelectizeInput(
      session, "relatorio_consistencia_ufs", selected = character()
    )
    updateSelectizeInput(
      session, "relatorio_consistencia_municipios", selected = character(),
      server = TRUE
    )
  })

  # Gera o relatório em diretório temporário da sessão e habilita os downloads.
  observeEvent(input$gerar_relatorio_consistencia, {
    req(base_padronizada(), resultado_consistencia())
    regioes <- input$relatorio_consistencia_regioes
    ufs <- input$relatorio_consistencia_ufs
    municipios <- input$relatorio_consistencia_municipios
    incluir_brasil <- isTRUE(input$relatorio_consistencia_incluir_brasil)
    erro_relatorio_consistencia(NULL)
    limpar_artefato_sessao(
      artefato_relatorio_consistencia, prefixo_relatorio_consistencia
    )
    liberar_memoria_ram(completa = TRUE)
    if (!incluir_brasil && !length(regioes) && !length(ufs) && !length(municipios)) {
      erro_relatorio_consistencia(
        paste(
          "Selecione ao menos uma abrangência para gerar o relatório:",
          "Brasil, região, UF ou município."
        )
      )
      return()
    }
    withProgress(message = "Gerando relatório de consistência...", value = 0.1, {
      tryCatch({
        # Recarrega os módulos de indicadores e apresentação do relatório, mas
        # reutiliza o resultado de consistência já calculado na sessão.
        recarregar_modulos_desenvolvimento(
          "relatorio_consistencia", ambiente_aplicativo,
          diretorio_projeto_aplicativo
        )
        resultado <- gerar_relatorio_consistencia(
          base_padronizada = base_padronizada(),
          resultado_consistencia = resultado_consistencia(),
          incluir_brasil = incluir_brasil,
          regioes = regioes, ufs = ufs, municipios = municipios,
          diretorio_referencias = file.path(
            diretorio_projeto_aplicativo, "data", "reference"
          ),
          gerar_pdf = TRUE,
          escala_matriz = input$relatorio_consistencia_escala_matriz
        )
        incProgress(0.85)
        prefixo <- prefixo_relatorio_consistencia
        shiny::addResourcePath(prefixo, dirname(resultado$caminho_html))
        resultado$url_relativa <- paste0(
          "/", prefixo, "/", basename(resultado$caminho_html)
        )
        artefato <- compactar_artefato_relatorio(resultado)
        artefato_relatorio_consistencia(artefato)
        rm(resultado, artefato)
        liberar_memoria_ram(completa = TRUE)
        incProgress(0.05)
      }, error = function(e) {
        message(sprintf("[Relatório de Consistência] %s", conditionMessage(e)))
        erro_relatorio_consistencia(conditionMessage(e))
      })
    })
  })

  output$baixar_relatorio_consistencia_html <- downloadHandler(
    filename = function() basename(artefato_relatorio_consistencia()$caminho_html),
    content = function(arquivo) {
      req(artefato_relatorio_consistencia())
      file.copy(
        artefato_relatorio_consistencia()$caminho_html,
        arquivo, overwrite = TRUE
      )
    },
    contentType = "text/html"
  )

  output$baixar_relatorio_consistencia_pdf <- downloadHandler(
    filename = function() basename(artefato_relatorio_consistencia()$caminho_pdf),
    content = function(arquivo) {
      req(
        artefato_relatorio_consistencia(),
        artefato_relatorio_consistencia()$caminho_pdf
      )
      file.copy(
        artefato_relatorio_consistencia()$caminho_pdf,
        arquivo, overwrite = TRUE
      )
    },
    contentType = "application/pdf"
  )

  # Relatório de validade ------------------------------------------------------
  # Mantém o mesmo fluxo territorial e de geração usado pela consistência.
  referencias_selecao_relatorio <- reactive({
    req(resultado_validade())
    carregar_referencias_territoriais(
      diretorio = file.path(diretorio_projeto_aplicativo, "data", "reference"),
      carregar_geografia = FALSE, carregar_cnes = FALSE
    )
  })

  output$relatorio_validade_ui <- renderUI({
    req(resultado_validade())
    referencias <- tryCatch(referencias_selecao_relatorio(), error = function(e) e)
    if (inherits(referencias, "error")) {
      return(div(class = "painel erro-leitura",
                 h3("Relatório de Validade indisponível"), conditionMessage(referencias)))
    }
    ufs <- referencias$ufs
    regioes <- ufs[!duplicated(ufs$codigo_regiao), , drop = FALSE]
    escolhas_regioes <- stats::setNames(
      regioes$codigo_regiao, paste0(regioes$codigo_regiao, " — ", regioes$nome_regiao)
    )
    escolhas_ufs <- stats::setNames(
      ufs$codigo_uf, paste0(ufs$codigo_uf, " — ", ufs$nome_uf, " (", ufs$sigla_uf, ")")
    )
    escolhas_municipios <- listar_municipios_disponiveis(
      base_padronizada(), referencias
    )
    municipios_selecionados <- isolate(
      if (is.null(input$relatorio_municipios)) character()
      else input$relatorio_municipios
    )
    municipios_selecionados <- intersect(
      municipios_selecionados, unname(escolhas_municipios)
    )
    session$onFlushed(function() {
      updateSelectizeInput(
        session, "relatorio_municipios", choices = escolhas_municipios,
        selected = municipios_selecionados, server = TRUE
      )
    }, once = TRUE)
    artefato <- artefato_relatorio()
    div(
      class = "painel",
      h3("Relatório de Validade"),
      p("Selecione uma ou mais abrangências para compor o relatório."),
      checkboxInput(
        "relatorio_incluir_brasil", "Incluir análise nacional — Brasil",
        value = isolate(isTRUE(input$relatorio_incluir_brasil))
      ),
      fluidRow(
        column(6, selectizeInput(
          "relatorio_regioes", "Regiões detalhadas", choices = escolhas_regioes,
          selected = isolate(if (is.null(input$relatorio_regioes)) character() else input$relatorio_regioes),
          multiple = TRUE,
          options = list(placeholder = "Selecione uma ou mais regiões")
        )),
        column(6, selectizeInput(
          "relatorio_ufs", "UFs detalhadas", choices = escolhas_ufs,
          selected = isolate(if (is.null(input$relatorio_ufs)) character() else input$relatorio_ufs),
          multiple = TRUE,
          options = list(placeholder = "Selecione uma ou mais UFs")
        ))
      ),
      selectizeInput(
        "relatorio_municipios", "Municípios detalhados",
        choices = NULL, selected = municipios_selecionados, multiple = TRUE,
        options = list(
          placeholder = "Busque por nome, código ou sigla da UF",
          maxOptions = 100L
        )
      ),
      selectInput(
        "relatorio_escala_matriz", "Escala das matrizes",
        choices = stats::setNames(
          names(configuracao_priorizacao()$rotulos_escalas),
          unname(configuracao_priorizacao()$rotulos_escalas)
        ),
        selected = isolate(if (is.null(input$relatorio_escala_matriz)) {
          configuracao_priorizacao()$escala_padrao
        } else input$relatorio_escala_matriz)
      ),
      div(class = "acoes-relatorio",
          actionButton("selecionar_ufs_regioes", "Selecionar UFs das regiões"),
          actionButton("selecionar_todas_regioes", "Todas as regiões"),
          actionButton("selecionar_todas_ufs", "Todas as UFs"),
          actionButton("limpar_selecao_relatorio", "Limpar seleção")),
      p(class = "observacao",
        "O detalhamento de uma região sempre compara todas as UFs que a compõem, independentemente da seleção de UFs detalhadas."),
      actionButton("gerar_relatorio_validade", "Gerar relatório", class = "btn-primary"),
      if (!is.null(erro_relatorio()))
        div(class = "erro-leitura", style = "margin-top:14px;", erro_relatorio()),
      if (!is.null(artefato)) tagList(
        div(class = "status-sucesso",
            strong("Relatório gerado com sucesso."),
            tags$div(paste("Período:", artefato$config_relatorio$periodo)),
            tags$div(paste("Geração:", format(artefato$config_relatorio$gerado_em,
                                             "%d/%m/%Y %H:%M:%S"))),
            tags$div(paste(
              "Abrangências:", artefato$config_relatorio$descricao_abrangencias
            )),
            tags$div(paste(
              "Escala das matrizes no PDF:",
              configuracao_priorizacao()$rotulos_escalas[[
                artefato$config_relatorio$escala_matriz_prioridade
              ]]
            ))),
        div(class = "acoes-relatorio",
            downloadButton("baixar_relatorio_html", "Baixar HTML"),
            if (!is.null(artefato$caminho_pdf))
              downloadButton("baixar_relatorio_pdf", "Baixar PDF")),
        h4("Visualização do relatório"),
        tags$iframe(class = "visualizador-relatorio", src = artefato$url_relativa,
                    title = "Visualização do Relatório de Validade")
      )
    )
  })

  # Atalhos de seleção territorial do relatório de validade.
  observeEvent(input$selecionar_todas_regioes, {
    referencias <- referencias_selecao_relatorio()
    updateSelectizeInput(session, "relatorio_regioes",
      selected = unique(referencias$ufs$codigo_regiao))
  })

  observeEvent(input$selecionar_todas_ufs, {
    referencias <- referencias_selecao_relatorio()
    updateSelectizeInput(session, "relatorio_ufs", selected = referencias$ufs$codigo_uf)
  })

  observeEvent(input$selecionar_ufs_regioes, {
    referencias <- referencias_selecao_relatorio()
    regioes <- input$relatorio_regioes
    if (!length(regioes)) {
      showNotification("Selecione ao menos uma região.", type = "warning")
      return()
    }
    codigos <- referencias$ufs$codigo_uf[referencias$ufs$codigo_regiao %in% regioes]
    updateSelectizeInput(session, "relatorio_ufs", selected = codigos)
  })

  observeEvent(input$limpar_selecao_relatorio, {
    updateCheckboxInput(session, "relatorio_incluir_brasil", value = FALSE)
    updateSelectizeInput(session, "relatorio_regioes", selected = character())
    updateSelectizeInput(session, "relatorio_ufs", selected = character())
    updateSelectizeInput(
      session, "relatorio_municipios", selected = character(), server = TRUE
    )
  })

  # Gera HTML/PDF temporários e disponibiliza cópias com nomes amigáveis.
  observeEvent(input$gerar_relatorio_validade, {
    req(base_padronizada(), resultado_validade())
    regioes <- input$relatorio_regioes
    ufs <- input$relatorio_ufs
    municipios <- input$relatorio_municipios
    incluir_brasil <- isTRUE(input$relatorio_incluir_brasil)
    erro_relatorio(NULL)
    limpar_artefato_sessao(artefato_relatorio, prefixo_relatorio_validade)
    liberar_memoria_ram(completa = TRUE)
    if (!incluir_brasil && !length(regioes) && !length(ufs) && !length(municipios)) {
      erro_relatorio(paste(
        "Selecione ao menos uma abrangência para gerar o relatório:",
        "Brasil, região, UF ou município."
      ))
      return()
    }
    withProgress(message = "Gerando relatório de validade...", value = 0.1, {
      tryCatch({
        # Recarrega indicadores, gráficos e montagem do relatório. A análise de
        # validade existente é reutilizada, sem executar novamente suas regras.
        recarregar_modulos_desenvolvimento(
          "relatorio_validade", ambiente_aplicativo,
          diretorio_projeto_aplicativo
        )
        resultado <- gerar_relatorio_validade(
          base_padronizada = base_padronizada(),
          resultado_validade = resultado_validade(),
          incluir_brasil = incluir_brasil,
          regioes = regioes, ufs = ufs, municipios = municipios,
          diretorio_referencias = file.path(
            diretorio_projeto_aplicativo, "data", "reference"
          ),
          gerar_pdf = TRUE, escala_matriz = input$relatorio_escala_matriz
        )
        incProgress(0.85)
        prefixo <- prefixo_relatorio_validade
        shiny::addResourcePath(prefixo, dirname(resultado$caminho_html))
        resultado$url_relativa <- paste0("/", prefixo, "/", basename(resultado$caminho_html))
        artefato <- compactar_artefato_relatorio(resultado)
        artefato_relatorio(artefato)
        rm(resultado, artefato)
        liberar_memoria_ram(completa = TRUE)
        incProgress(0.05)
      }, error = function(e) {
        message(sprintf("[Relatório de Validade] %s", conditionMessage(e)))
        erro_relatorio(conditionMessage(e))
      })
    })
  })

  output$baixar_relatorio_html <- downloadHandler(
    filename = function() basename(artefato_relatorio()$caminho_html),
    content = function(arquivo) {
      req(artefato_relatorio())
      file.copy(artefato_relatorio()$caminho_html, arquivo, overwrite = TRUE)
    }, contentType = "text/html"
  )

  output$baixar_relatorio_pdf <- downloadHandler(
    filename = function() basename(artefato_relatorio()$caminho_pdf),
    content = function(arquivo) {
      req(artefato_relatorio(), artefato_relatorio()$caminho_pdf)
      file.copy(artefato_relatorio()$caminho_pdf, arquivo, overwrite = TRUE)
    }, contentType = "application/pdf"
  )
}

# Inicializa o aplicativo com a interface e o servidor definidos acima.
shinyApp(ui, server)
