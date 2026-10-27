# Preparação comum dos testes --------------------------------------------------
#
# Carrega os módulos de produção uma única vez antes dos casos de teste. A ordem
# replica as dependências do aplicativo, evitando que um teste dependa do `app.R`.
diretorio_r <- testthat::test_path("..", "..", "R")

# A configuração deve existir antes dos demais módulos, assim como no app.
source(file.path(diretorio_r, "configuracao_aplicativo.R"))
source(file.path(diretorio_r, "modo_desenvolvimento.R"))
source(file.path(diretorio_r, "gerenciamento_memoria.R"))
source(file.path(diretorio_r, "importar_dados.R"))
source(file.path(diretorio_r, "importar_csv.R"))
source(file.path(diretorio_r, "importar_excel.R"))
source(file.path(diretorio_r, "importar_dbf.R"))
source(file.path(diretorio_r, "importar_dta.R"))
source(file.path(diretorio_r, "regras_padronizacao.R"))
source(file.path(diretorio_r, "padronizar_base.R"))
source(file.path(diretorio_r, "enriquecer_cnes.R"))
source(file.path(diretorio_r, "regras_validade.R"))
source(file.path(diretorio_r, "analisar_validade.R"))
source(file.path(diretorio_r, "regras_consistencia.R"))
source(file.path(diretorio_r, "analisar_consistencia.R"))
source(file.path(diretorio_r, "referencias_territoriais.R"))
source(file.path(diretorio_r, "indicadores_relatorio_validade.R"))
source(file.path(diretorio_r, "indicadores_relatorio_consistencia.R"))
source(file.path(diretorio_r, "priorizacao_relatorio.R"))
source(file.path(diretorio_r, "visualizacoes_relatorio.R"))
source(file.path(diretorio_r, "visualizacoes_relatorio_consistencia.R"))
source(file.path(diretorio_r, "relatorio_validade.R"))
source(file.path(diretorio_r, "relatorio_consistencia.R"))
