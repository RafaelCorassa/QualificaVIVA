# Configuração central do QualificaVIVA ---------------------------------------
#
# Este é o principal arquivo para personalizações simples do aplicativo e dos
# relatórios. Títulos, cores, tamanhos de figuras e limites de exibição ficam
# reunidos aqui para evitar alterações nos motores de cálculo.
#
# IMPORTANTE:
# - valores entre aspas são textos, nomes ou cores;
# - valores numéricos de figuras estão em polegadas;
# - não remova nomes localizados à esquerda do sinal `=`;
# - depois de uma mudança, execute os testes descritos no GUIA_DE_MANUTENCAO.md.

# Área de edição segura --------------------------------------------------------
# Altere somente os valores desta lista quando a mudança for visual ou textual.
configuracao_viva_quali <- list(
  aplicativo = list(
    # Nome exibido na barra superior e título principal da aplicação.
    nome = "QualificaVIVA",
    # Texto apresentado na página inicial, abaixo do nome do sistema.
    subtitulo = "Sistema de avaliação da qualidade dos dados do VIVA-Sinan",
    # Arquivos servidos pelo Shiny e incorporados aos relatórios autossuficientes.
    identidade = list(
      logo = "identidade/qualifica-viva-logo.png",
      simbolo = "identidade/qualifica-viva-simbolo.png",
      logo_arquivo = file.path("www", "identidade", "qualifica-viva-logo.png"),
      simbolo_arquivo = file.path("www", "identidade", "qualifica-viva-simbolo.png")
    ),
    # AUTORIA E CRÉDITOS
    #
    # Preencha os campos vazios abaixo. Eles são exibidos na guia "Sobre", no
    # rodapé do aplicativo e nos relatórios. O Codex é registrado somente como
    # ferramenta de apoio; a autoria e a responsabilidade permanecem humanas.
    autoria = list(
      autores = "",
      instituicao = "",
      contato = "",
      ano = "",
      versao = "",
      ferramenta_apoio = "OpenAI Codex",
      declaracao_apoio = paste(
        "O OpenAI Codex foi utilizado como ferramenta de apoio à implementação,",
        "revisão e teste do código."
      ),
      declaracao_responsabilidade = paste(
        "As decisões metodológicas, a validação dos resultados e a responsabilidade",
        "pelo aplicativo são de seus autores."
      )
    ),
    # Tamanho máximo de arquivo aceito no envio, em mebibytes (MiB).
    limite_upload_mib = 1024
  ),

  desenvolvimento = list(
    # MODO DE DESENVOLVIMENTO
    #
    # TRUE: antes de padronizar, analisar ou gerar um relatório, o aplicativo
    # recarrega somente os scripts seguros daquela etapa. A base já importada e
    # os demais resultados permanecem na memória.
    #
    # FALSE: funcionamento normal recomendado após concluir as edições. Os
    # scripts são carregados uma única vez quando o aplicativo é iniciado.
    #
    # Para retornar ao modo normal, troque apenas TRUE por FALSE e salve.
    ativo = TRUE,
    # Exibe no console do R quais módulos foram atualizados em cada comando.
    mostrar_mensagens = TRUE
  ),

  relatorios = list(
    geral = list(
      # Paleta compartilhada pela interface, pelos relatórios e pelos gráficos.
      cores = list(
        principal = "#471E82",
        principal_escura = "#3B1B69",
        marca = "#900898",
        validacao = "#309890",
        validacao_escura = "#18766C",
        principal_clara = "#E8D8F0",
        superficie_suave = "#F8F5FA",
        superficie_marca = "#F3EAF6",
        texto = "#2C2733",
        texto_secundario = "#6B6472",
        borda = "#DDD4E3",
        fundo = "#F8F5FA",
        mapa_baixo = "#F2E8FF",
        mapa_alto = "#a61b1b",
        mapa_ausente = "#dedede",
        pareto_linha = "#d97706",
        pareto_50 = "#c99700",
        pareto_80 = "#b33a3a"
      ),

      # Dimensões usadas quando os gráficos são convertidos em imagens.
      figuras = list(
        padrao = list(largura = 8.5, altura = 5, dpi = 150),
        mapas = list(
          largura = 8.5,
          altura = 8.5,
          # Use "lado_a_lado" ou "vertical" no relatório HTML.
          disposicao = "lado_a_lado"
        ),
        # Largura e altura dos gráficos de Pareto, em polegadas. A mesma
        # proporção controla a altura da versão interativa exibida no HTML.
        # Aumente o valor de altura para deixar os gráficos mais altos.
        pareto = list(largura = 11, altura = 9.5),
        grupos = list(altura = 5.4),
        regras = list(
          altura_minima = 3.4,
          altura_maxima = 6.2,
          altura_base = 2.4,
          altura_por_regra = 0.42
        ),
        # Dimensões da versão estática das matrizes usada no PDF.
        matriz_prioridade = list(largura = 10.5, altura = 6.5, dpi = 120)
      ),

      # Quantidades máximas usadas nas tabelas e nos gráficos prioritários.
      limites = list(
        ranking = 20L,
        pareto = 50L,
        rotulos_matriz = 10L
      ),

      # Medidas do documento HTML e do PDF impresso.
      layout = list(
        largura_conteudo_px = 1180L,
        altura_maxima_figura_pdf_px = 680L
      ),

      # Modelo compartilhado pelas matrizes de Validade e Consistência.
      # Os percentis usam valores entre 0 e 1. Por exemplo, 0.95 corresponde
      # ao percentil 95. O mínimo abaixo conta registros problemáticos, não o
      # total de notificações da localidade.
      priorizacao = list(
        percentil_absoluto = 0.95,
        percentil_absoluto2 = 0.98,
        percentil_relativo = 0.95,
        minimo_problemas_prioridade_3 = 30L,
        metodo_quantile_r = 7L,
        # A transformação afeta somente a posição visual; cálculos e tooltips
        # continuam usando os valores originais.
        transformacao_x = "pseudo_log",
        transformacao_y = "pseudo_log",
        sigma_x = 1,
        sigma_y = 0.5,
        # Escala exibida inicialmente no HTML e usada na figura enviada ao PDF.
        # O usuário também pode escolher a escala na tela de geração do relatório.
        escala_padrao = "transformada",
        rotulos_escalas = c(
          original = "Escala original",
          transformada = "Escala transformada"
        ),
        # Quantidade máxima de nomes críticos escritos permanentemente.
        rotulos_prioridade_1 = 12L,
        omitir_sem_problemas_na_matriz = TRUE,
        pontos = list(
          tamanho_minimo = 1,
          tamanho_maximo = 30,
          transparencia_padrao = 0.80,
          transparencia_monitoramento = 0.40
        ),
        categorias = list(
          prioridade_1 = list(
            nome = "Prioridade 1 — crítica", cor = "#B2182B", forma = 21L
          ),
          prioridade_2 = list(
            nome = "Prioridade 2 — alto impacto", cor = "#E08214", forma = 21L
          ),
          prioridade_3 = list(
            nome = "Prioridade 3 — atenção relativa", cor = "#2166AC", forma = 21L
          ),
          monitoramento = list(
            nome = "Monitoramento", cor = "yellowgreen", forma = 21L
          ),
          sem_problemas = list(
            nome = "Sem problemas", cor = "#D1D5DB", forma = 21L
          )
        )
      )
    ),

    validade = list(
      # Título do arquivo HTML, cabeçalho e identificação do relatório.
      titulo_documento = "QualificaVIVA — Relatório de Validade",
      nome_arquivo = "QualificaVIVA_Relatorio_Validade",
      # Títulos sem numeração fixa. O gerador acrescenta números contínuos de
      # acordo com as abrangências efetivamente selecionadas.
      titulos = list(
        brasil = "Brasil",
        sintese_nacional = "Síntese nacional",
        resultados_nacionais = "Resultados por regra",
        distribuicao_uf = "Distribuição por UF",
        mapas = "Mapas",
        regiao = "Região %s",
        uf = "%s",
        municipio = "%s",
        resultados_territorio = "Resultados por regra",
        comparacao_ufs = "Comparação entre UFs da região",
        municipios = "Municípios prioritários",
        estabelecimentos = "Estabelecimentos notificadores prioritários",
        metodologia = "Metodologia e auditoria",
        relacionamentos = "Validação dos relacionamentos territoriais"
      )
    ),

    consistencia = list(
      # Título do arquivo HTML, cabeçalho e identificação do relatório.
      titulo_documento = "QualificaVIVA — Relatório de Consistência",
      nome_arquivo = "QualificaVIVA_Relatorio_Consistencia",
      # Títulos sem numeração fixa. O gerador acrescenta números contínuos de
      # acordo com as abrangências efetivamente selecionadas.
      titulos = list(
        brasil = "Brasil",
        sintese_nacional = "Síntese nacional",
        resultados_nacionais = "Resultados por grupos e regras de consistência",
        distribuicao_uf = "Resultados por UF",
        mapas = "Distribuição das inconsistências por UF",
        regiao = "Região %s",
        uf = "%s",
        municipio = "%s",
        resultados_territorio = "Resultados por grupos e regras de consistência",
        comparacao_ufs = "Resultados por UF da região",
        municipios = "Identificação de municípios prioritários",
        estabelecimentos = "Identificação de estabelecimentos prioritários",
        metodologia = "Metodologia de análise",
        relacionamentos = "Validação de dados",
        sintese_grupos = "Resultados por grupos de regras"
      ),
      # Textos dos controles exclusivos do HTML interativo.
      controles = list(
        apenas_grupos = "Ver apenas grupos",
        expandir_regras = "Expandir todas as regras",
        abrir_regras = "Expandir regras individuais",
        abrir_dominantes = "Expandir regras dominantes"
      )
    )
  )
)

# Funções auxiliares da configuração ------------------------------------------
#
# As funções abaixo são usadas pelos módulos internos. Em geral, não é
# necessário modificá-las; elas apenas validam os nomes e entregam os valores da
# lista acima com mensagens de erro compreensíveis.

# Retorna as configurações comuns a todos os relatórios.
configuracao_geral_relatorios <- function() {
  configuracao_viva_quali$relatorios$geral
}

# Retorna os metadados de autoria usados pela interface e pelos relatórios.
configuracao_autoria <- function() {
  configuracao_viva_quali$aplicativo$autoria
}

# Mantém campos ainda não preenchidos visíveis e fáceis de localizar, sem
# inventar nomes, vínculos institucionais, contatos, datas ou versões.
valor_autoria <- function(chave) {
  rotulos <- c(
    autores = "Rafael Bello Corassa",
    instituicao = "CGVIVA/DAENT/SVSA/MS",
    contato = "rafael.corassa@saude.gov.br",
    ano = "2026",
    versao = "preencher versão"
  )
  autoria <- configuracao_autoria()
  if (!chave %in% names(rotulos) || !chave %in% names(autoria)) {
    stop(sprintf("Campo de autoria desconhecido: %s.", chave), call. = FALSE)
  }
  valor <- trimws(as.character(autoria[[chave]]))
  if (nzchar(valor)) valor else sprintf("[%s]", rotulos[[chave]])
}

# Retorna os cortes, nomes, cores e parâmetros visuais da priorização. Manter
# esse acesso centralizado permite ajustar o modelo sem editar os cálculos.
configuracao_priorizacao <- function() {
  configuracao_geral_relatorios()$priorizacao
}

# Retorna a configuração de validade ou consistência e rejeita nomes incorretos.
configuracao_de_relatorio <- function(modulo) {
  if (!modulo %in% c("validade", "consistencia")) {
    stop(sprintf("Módulo de relatório desconhecido: %s.", modulo), call. = FALSE)
  }
  configuracao_viva_quali$relatorios[[modulo]]
}

# Localiza um título pelo nome estável usado internamente pelos geradores.
titulo_de_relatorio <- function(modulo, chave) {
  titulos <- configuracao_de_relatorio(modulo)$titulos
  if (!chave %in% names(titulos)) {
    stop(sprintf(
      "Título '%s' não foi definido para o relatório de %s.", chave, modulo
    ), call. = FALSE)
  }
  titulos[[chave]]
}

# Retorna uma cor da paleta compartilhada pelos gráficos e documentos.
cor_viva_quali <- function(chave) {
  cores <- configuracao_geral_relatorios()$cores
  if (!chave %in% names(cores)) {
    stop(sprintf("Cor de configuração desconhecida: %s.", chave), call. = FALSE)
  }
  cores[[chave]]
}

# Constrói variáveis CSS usadas pela interface Shiny. Assim, as cores principais
# da aplicação e dos relatórios permanecem sincronizadas pela mesma paleta.
css_variaveis_aplicativo <- function() {
  paste0(
    ":root {",
    "--viva-principal:", cor_viva_quali("principal"), ";",
    "--viva-principal-escura:", cor_viva_quali("principal_escura"), ";",
    "--viva-marca:", cor_viva_quali("marca"), ";",
    "--viva-validacao:", cor_viva_quali("validacao"), ";",
    "--viva-validacao-escura:", cor_viva_quali("validacao_escura"), ";",
    "--viva-principal-clara:", cor_viva_quali("principal_clara"), ";",
    "--viva-superficie-suave:", cor_viva_quali("superficie_suave"), ";",
    "--viva-superficie-marca:", cor_viva_quali("superficie_marca"), ";",
    "--viva-texto:", cor_viva_quali("texto"), ";",
    "--viva-secundario:", cor_viva_quali("texto_secundario"), ";",
    "--viva-borda:", cor_viva_quali("borda"), ";",
    "--viva-fundo:", cor_viva_quali("fundo"), ";",
    "}"
  )
}

# Retorna as dimensões de uma categoria de figura, como mapas ou Paretos.
dimensoes_figura_relatorio <- function(tipo = "padrao") {
  figuras <- configuracao_geral_relatorios()$figuras
  if (!tipo %in% names(figuras)) {
    stop(sprintf("Tipo de figura desconhecido: %s.", tipo), call. = FALSE)
  }
  figuras[[tipo]]
}

# Traduz a opção amigável dos mapas para a classe CSS usada no HTML. O argumento
# opcional também permite validar uma escolha isoladamente nos testes.
classe_layout_mapas <- function(
    disposicao = dimensoes_figura_relatorio("mapas")$disposicao) {
  if (!disposicao %in% c("lado_a_lado", "vertical")) {
    stop(
      "A disposição dos mapas deve ser 'lado_a_lado' ou 'vertical'.",
      call. = FALSE
    )
  }
  if (identical(disposicao, "vertical")) "mapas-verticais" else "mapas-lado-a-lado"
}
