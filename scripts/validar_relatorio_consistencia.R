# Validação manual do relatório de consistência -------------------------------
#
# Reproduz o fluxo completo com uma base DTA real e gera HTML/PDF de homologação
# em `output`. O script é independente da interface e serve para verificar o
# relatório após alterações de regras, referências ou apresentação.

# Carrega os módulos necessários na mesma sequência lógica do aplicativo.
source("R/configuracao_aplicativo.R")
source("R/gerenciamento_memoria.R")
source("R/importar_dados.R")
source("R/importar_dta.R")
source("R/regras_padronizacao.R")
source("R/padronizar_base.R")
source("R/regras_consistencia.R")
source("R/analisar_consistencia.R")
source("R/referencias_territoriais.R")
source("R/indicadores_relatorio_validade.R")
source("R/indicadores_relatorio_consistencia.R")
source("R/priorizacao_relatorio.R")
source("R/visualizacoes_relatorio.R")
source("R/visualizacoes_relatorio_consistencia.R")
source("R/relatorio_validade.R")
source("R/relatorio_consistencia.R")

# Localiza a primeira base DTA mantida na pasta de dados de validação.
candidatos <- list.files(
  "Dados", pattern = "\\.dta$", full.names = TRUE, ignore.case = TRUE
)
caminho_base <- if (length(candidatos)) candidatos[[1]] else ""
if (!file.exists(caminho_base)) stop("Base de validação não encontrada.")

# Reproduz importação, padronização, consistência e vínculo territorial.
base_bruta <- importar_dta(caminho_base)
padronizacao <- padronizar_base(base_bruta)
rm(base_bruta)
liberar_memoria_ram(completa = TRUE)
consistencia <- executar_analise_consistencia(
  padronizacao$base_padronizada, padronizacao$variaveis_originais
)
referencias <- carregar_referencias_territoriais(carregar_geografia = FALSE)
registros <- preparar_registros_territoriais_consistencia(
  padronizacao$base_padronizada, consistencia, referencias
)
ufs_validas <- registros$codigo_uf[registros$uf_localizada]
if (!length(ufs_validas)) stop("A base não possui UF de notificação localizada.")
uf_escolhida <- names(sort(table(ufs_validas), decreasing = TRUE))[[1]]
regiao_escolhida <- referencias$ufs$codigo_regiao[
  match(uf_escolhida, referencias$ufs$codigo_uf)
]
rm(registros, referencias, ufs_validas)
liberar_memoria_ram()

# Gera HTML e PDF para inspeção do conteúdo e da hierarquia grupo > regra.
artefato <- gerar_relatorio_consistencia(
  base_padronizada = padronizacao$base_padronizada,
  resultado_consistencia = consistencia,
  incluir_brasil = TRUE,
  regioes = regiao_escolhida, ufs = uf_escolhida,
  diretorio_saida = file.path("output", "validacao_relatorio_consistencia"),
  gerar_pdf = TRUE
)

message(sprintf("HTML: %s", artefato$caminho_html))
message(sprintf("PDF: %s", artefato$caminho_pdf))
message(sprintf(
  "Configuração: região %s; UF %s; período %s.",
  artefato$config_relatorio$regioes_selecionadas,
  artefato$config_relatorio$ufs_selecionadas,
  artefato$config_relatorio$periodo
))
