# Validação manual do relatório de validade ------------------------------------
#
# Executa o fluxo completo usando o primeiro DTA da pasta `Dados`, seleciona a
# UF com mais registros localizados e grava HTML/PDF em `output`. É um roteiro de
# homologação local, não faz parte da execução normal do aplicativo.

# Carrega os módulos necessários na mesma sequência lógica do aplicativo.
source("R/configuracao_aplicativo.R")
source("R/gerenciamento_memoria.R")
source("R/importar_dados.R")
source("R/importar_dta.R")
source("R/regras_padronizacao.R")
source("R/padronizar_base.R")
source("R/regras_validade.R")
source("R/analisar_validade.R")
source("R/referencias_territoriais.R")
source("R/indicadores_relatorio_validade.R")
source("R/priorizacao_relatorio.R")
source("R/visualizacoes_relatorio.R")
source("R/relatorio_validade.R")

# Localiza a base real de validação disponibilizada no projeto.
candidatos <- list.files("Dados", pattern = "\\.dta$", full.names = TRUE,
                         ignore.case = TRUE)
caminho_base <- if (length(candidatos)) candidatos[[1]] else ""
if (!file.exists(caminho_base)) stop("Base de validação não encontrada.")

# Reproduz importação, padronização, validade e relacionamento territorial.
base_bruta <- importar_dta(caminho_base)
padronizacao <- padronizar_base(base_bruta)
rm(base_bruta)
liberar_memoria_ram(completa = TRUE)
validade <- executar_analise_validade(
  padronizacao$base_padronizada, padronizacao$variaveis_originais
)
referencias <- carregar_referencias_territoriais(carregar_geografia = FALSE)
registros <- preparar_registros_territoriais(
  padronizacao$base_padronizada, validade, referencias
)
uf_escolhida <- names(sort(table(registros$codigo_uf[registros$uf_localizada]), decreasing = TRUE))[[1]]
regiao_escolhida <- referencias$ufs$codigo_regiao[
  match(uf_escolhida, referencias$ufs$codigo_uf)
]
rm(registros, referencias)
liberar_memoria_ram()

# Gera os dois formatos para inspeção visual e funcional.
artefato <- gerar_relatorio_validade(
  base_padronizada = padronizacao$base_padronizada,
  resultado_validade = validade,
  incluir_brasil = TRUE,
  regioes = regiao_escolhida, ufs = uf_escolhida,
  diretorio_saida = file.path("output", "validacao_relatorio"),
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
