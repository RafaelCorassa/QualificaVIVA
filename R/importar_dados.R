# Módulo central de importação -------------------------------------------------
#
# Reúne as validações compartilhadas pelos leitores de DBF, DTA, Excel e CSV.
# Os módulos específicos chamam estas funções antes e depois da leitura para
# oferecer mensagens uniformes na interface. Ao incluir um novo formato, ele
# deve ser registrado em `extensoes_permitidas` e tratado pelo fluxo do app.

# Lista única de extensões aceitas, sempre em letras minúsculas.
extensoes_permitidas <- c("dbf", "dta", "xls", "xlsx", "csv")

# Identifica o formato pelo nome original do arquivo enviado pelo usuário.
# Retorna somente a extensão validada; interrompe o fluxo para nomes vazios ou
# formatos não contemplados pelo aplicativo.
identificar_formato <- function(nome_arquivo) {
  if (length(nome_arquivo) != 1L || is.na(nome_arquivo) || !nzchar(nome_arquivo)) {
    stop("O nome do arquivo não foi informado.", call. = FALSE)
  }
  extensao <- tolower(tools::file_ext(nome_arquivo))
  if (!nzchar(extensao) || !extensao %in% extensoes_permitidas) {
    stop(
      sprintf("Formato não suportado. Selecione um arquivo DBF, DTA, XLS, XLSX ou CSV."),
      call. = FALSE
    )
  }
  extensao
}

# Confirma que o arquivo temporário existe e possui conteúdo antes da leitura.
validar_arquivo_leitura <- function(caminho) {
  if (length(caminho) != 1L || !file.exists(caminho)) {
    stop("O arquivo selecionado não está disponível para leitura.", call. = FALSE)
  }
  tamanho <- file.info(caminho)$size
  if (is.na(tamanho) || tamanho == 0) {
    stop("O arquivo selecionado está vazio.", call. = FALSE)
  }
  invisible(TRUE)
}

# Verifica o contrato mínimo comum a toda importação: uma tabela com colunas.
validar_base_importada <- function(dados) {
  if (!is.data.frame(dados)) {
    stop("O arquivo não produziu uma tabela válida.", call. = FALSE)
  }
  if (ncol(dados) == 0L) {
    stop("Nenhuma coluna reconhecível foi encontrada no arquivo.", call. = FALSE)
  }
  invisible(TRUE)
}

# Converte mensagens técnicas dos pacotes de leitura em orientações compreensíveis.
# `contexto` permite especializar a mensagem conforme o formato selecionado.
mensagem_erro_importacao <- function(erro, contexto = "arquivo") {
  texto <- conditionMessage(erro)
  texto_minusculo <- tolower(texto)
  if (grepl("encoding|unicode|multibyte|character", texto_minusculo)) {
    return("Não foi possível interpretar a codificação de caracteres. Verifique a codificação selecionada.")
  }
  if (grepl("empty|vazio|0 bytes", texto_minusculo)) {
    return("O arquivo selecionado está vazio ou não contém dados legíveis.")
  }
  if (grepl("sheet|planilha", texto_minusculo)) {
    return("A planilha selecionada não pôde ser lida. Verifique se o arquivo não está corrompido.")
  }
  if (grepl("formato não suportado", texto_minusculo)) return(texto)
  if (identical(contexto, "CSV")) {
    return("Não foi possível interpretar o CSV. Verifique o separador, o delimitador de texto e a codificação.")
  }
  if (identical(contexto, "DBF")) {
    return("Não foi possível ler o DBF. O arquivo pode estar corrompido ou usar uma codificação incompatível.")
  }
  if (identical(contexto, "DTA")) {
    return("Não foi possível ler o arquivo Stata. Verifique se o DTA é válido e usa uma versão compatível (8 a 15).")
  }
  sprintf("Não foi possível ler o %s. Verifique se o arquivo é válido e não está corrompido.", contexto)
}
