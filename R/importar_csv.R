# Importação de arquivos CSV ---------------------------------------------------
#
# Este módulo valida as escolhas feitas na interface e usa `readr` para ler o
# arquivo sem alterar automaticamente espaços, nomes de colunas ou codificação.

# Garante que separador e delimitador correspondam às opções aceitas pela tela.
validar_parametros_csv <- function(separador, delimitador) {
  if (length(separador) != 1L || is.na(separador) || nchar(separador, type = "chars") != 1L) {
    stop("O separador deve conter exatamente um caractere.", call. = FALSE)
  }
  if (length(delimitador) != 1L || is.na(delimitador) ||
      !delimitador %in% c("\"", "'", "")) {
    stop("O delimitador de texto selecionado não é válido.", call. = FALSE)
  }
  invisible(TRUE)
}

# Executa a leitura configurável do CSV.
# Retorna a base e também diagnósticos usados na pré-visualização: quantidade de
# colunas, avisos e problemas de conversão identificados pelo `readr`.
ler_csv_configurado <- function(caminho, separador = ";", delimitador = "\"",
                                codificacao = "Windows-1252", n_max = Inf,
                                mostrar_progresso = FALSE) {
  validar_arquivo_leitura(caminho)
  validar_parametros_csv(separador, delimitador)

  # Os avisos do pacote são coletados abaixo por `readr::problems()`, evitando
  # duplicação de mensagens técnicas na interface.
  dados <- suppressWarnings(readr::read_delim(
    file = caminho,
    delim = separador,
    quote = delimitador,
    locale = readr::locale(encoding = codificacao),
    n_max = n_max,
    name_repair = "minimal",
    trim_ws = FALSE,
    show_col_types = FALSE,
    progress = mostrar_progresso,
    lazy = FALSE
  ))
  dados <- as.data.frame(dados, check.names = FALSE, stringsAsFactors = FALSE)
  validar_base_importada(dados)

  # Uma única coluna normalmente indica escolha incorreta de separador.
  problemas <- readr::problems(dados)
  avisos <- character()
  if (ncol(dados) == 1L) {
    avisos <- c(avisos,
      "Foi identificada apenas uma coluna. Verifique se o separador selecionado corresponde ao utilizado no arquivo.")
  }
  if (nrow(problemas) > 0L) {
    avisos <- c(avisos,
      sprintf("Foram encontrados %s problemas de leitura. O separador ou o delimitador de texto pode estar incorreto.",
              format(nrow(problemas), big.mark = ".")))
  }

  list(dados = dados, numero_colunas = ncol(dados), avisos = unique(avisos),
       problemas = problemas)
}

# Atalho usado na importação definitiva: entrega apenas a tabela completa.
importar_csv <- function(caminho, separador = ";", delimitador = "\"",
                         codificacao = "Windows-1252") {
  ler_csv_configurado(caminho, separador, delimitador, codificacao, n_max = Inf)$dados
}
