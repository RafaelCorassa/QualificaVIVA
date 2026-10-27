# Importação de planilhas Excel -----------------------------------------------
#
# A listagem das abas é separada da leitura porque a interface primeiro precisa
# exibir as planilhas disponíveis e somente depois importar a aba escolhida.

# Retorna os nomes das abas existentes no arquivo XLS/XLSX.
listar_planilhas_excel <- function(caminho) {
  validar_arquivo_leitura(caminho)
  planilhas <- readxl::excel_sheets(caminho)
  if (!length(planilhas)) {
    stop("Nenhuma planilha foi encontrada no arquivo.", call. = FALSE)
  }
  planilhas
}

# Lê uma aba previamente validada e preserva os nomes originais das colunas.
importar_excel <- function(caminho, planilha) {
  validar_arquivo_leitura(caminho)
  if (length(planilha) != 1L || is.na(planilha) || !nzchar(planilha)) {
    stop("Selecione uma planilha válida.", call. = FALSE)
  }
  planilhas <- listar_planilhas_excel(caminho)
  if (!planilha %in% planilhas) {
    stop("A planilha selecionada não existe no arquivo.", call. = FALSE)
  }
  dados <- readxl::read_excel(caminho, sheet = planilha, .name_repair = "minimal")
  dados <- as.data.frame(dados, check.names = FALSE, stringsAsFactors = FALSE)
  validar_base_importada(dados)
  dados
}
