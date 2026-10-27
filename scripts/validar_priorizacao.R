# Homologação do modelo compartilhado de prioridade -------------------------
#
# Este roteiro reproduz importação, padronização, Validade e Consistência com
# a base DTA real do projeto. Ele seleciona Roraima e Minas Gerais, gera HTML e
# PDF dos dois módulos e grava um resumo das categorias para auditoria.

# Carrega os módulos na mesma ordem de `app.R` para reproduzir o comportamento
# do aplicativo sem abrir a interface gráfica.
source("R/configuracao_aplicativo.R")
source("R/gerenciamento_memoria.R")
source("R/importar_dados.R")
source("R/importar_dta.R")
source("R/regras_padronizacao.R")
source("R/padronizar_base.R")
source("R/regras_validade.R")
source("R/analisar_validade.R")
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

# Localiza a base de homologação mantida pelo projeto.
candidatos <- list.files(
  "Dados", pattern = "\\.dta$", full.names = TRUE, ignore.case = TRUE
)
if (!length(candidatos)) stop("Base DTA de homologação não encontrada.")

# Executa as análises uma única vez. A emissão dos relatórios reutiliza estes
# resultados e não recalcula as regras.
base_bruta <- importar_dta(candidatos[[1]])
padronizacao <- padronizar_base(base_bruta)
rm(base_bruta)
liberar_memoria_ram(completa = TRUE)
validade <- executar_analise_validade(
  padronizacao$base_padronizada, padronizacao$variaveis_originais
)
consistencia <- executar_analise_consistencia(
  padronizacao$base_padronizada, padronizacao$variaveis_originais
)

# Roraima (14) e Minas Gerais (31) representam universos territoriais de portes
# muito diferentes. As regiões 1 e 3 são obtidas pelos códigos oficiais.
ufs_homologacao <- c("14", "31")
regioes_homologacao <- c("1", "3")
saida <- file.path("output", "validacao_priorizacao")
dir.create(saida, recursive = TRUE, showWarnings = FALSE)

artefato_validade <- gerar_relatorio_validade(
  base_padronizada = padronizacao$base_padronizada,
  resultado_validade = validade,
  incluir_brasil = TRUE,
  regioes = regioes_homologacao, ufs = ufs_homologacao,
  diretorio_saida = file.path(saida, "validade"), gerar_pdf = TRUE
)
artefato_consistencia <- gerar_relatorio_consistencia(
  base_padronizada = padronizacao$base_padronizada,
  resultado_consistencia = consistencia,
  incluir_brasil = TRUE,
  regioes = regioes_homologacao, ufs = ufs_homologacao,
  diretorio_saida = file.path(saida, "consistencia"), gerar_pdf = TRUE
)

# Confere automaticamente a seleção territorial incorporada aos artefatos.
stopifnot(
  identical(artefato_validade$config_relatorio$regioes_selecionadas,
            regioes_homologacao),
  identical(artefato_validade$config_relatorio$ufs_selecionadas,
            ufs_homologacao),
  identical(artefato_consistencia$config_relatorio$regioes_selecionadas,
            regioes_homologacao),
  identical(artefato_consistencia$config_relatorio$ufs_selecionadas,
            ufs_homologacao)
)

message("Relatório de Validade: ", artefato_validade$caminho_html)
message("Relatório de Consistência: ", artefato_consistencia$caminho_html)
message("Homologação territorial concluída para RR e MG.")
