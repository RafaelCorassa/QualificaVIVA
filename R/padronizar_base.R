# Pipeline de padronização da base --------------------------------------------
#
# Este módulo transforma a base importada no formato esperado pelas análises.
# O fluxo é deliberadamente dividido em validação prévia, transformações e
# auditoria. Toda falha recebe contexto de etapa e variável para ser exibida ao
# usuário sem esconder o erro técnico que impediu a continuidade.

# Procura um arquivo de referência subindo a árvore de diretórios do projeto.
localizar_referencia <- function(nome_arquivo, inicio = getwd()) {
  diretorio <- normalizePath(inicio, winslash = "/", mustWork = TRUE)
  for (i in seq_len(8L)) {
    candidato <- file.path(diretorio, "inst", "referencias", nome_arquivo)
    if (file.exists(candidato)) return(candidato)
    superior <- dirname(diretorio)
    if (identical(superior, diretorio)) break
    diretorio <- superior
  }
  stop(sprintf("Arquivo de referência não encontrado: %s", nome_arquivo), call. = FALSE)
}

# Cria uma condição de erro própria, capaz de transportar vários problemas de
# validação em vez de mostrar apenas o primeiro.
criar_erro_padronizacao <- function(erros) {
  erros <- unique(as.character(erros))
  estrutura <- structure(
    list(
      message = paste0(
        "A padronização foi interrompida por ", length(erros),
        if (length(erros) == 1L) " erro:" else " erros:",
        "\n- ", paste(erros, collapse = "\n- ")
      ),
      call = NULL,
      erros = erros
    ),
    class = c("erro_padronizacao", "error", "condition")
  )
  estrutura
}

# Interrompe o fluxo com a condição estruturada definida acima.
interromper_padronizacao <- function(erros) {
  stop(criar_erro_padronizacao(erros))
}

# Recupera a lista detalhada de falhas para apresentação na interface.
extrair_erros_padronizacao <- function(erro) {
  if (inherits(erro, "erro_padronizacao") && length(erro$erros)) {
    return(erro$erros)
  }
  conditionMessage(erro)
}

# Opções oferecidas ao usuário; `auto` preserva UTF-8 e converte textos legados.
codificacoes_texto_permitidas <- c("auto", "Windows-1252", "ISO-8859-1", "UTF-8")

# Valida a opção antes de qualquer tentativa de conversão.
validar_opcao_codificacao <- function(codificacao) {
  if (length(codificacao) != 1L || is.na(codificacao) ||
      !codificacao %in% codificacoes_texto_permitidas) {
    stop(sprintf(
      "Codificação de texto inválida: '%s'. Opções permitidas: %s.",
      paste(codificacao, collapse = ", "),
      paste(codificacoes_texto_permitidas, collapse = ", ")
    ), call. = FALSE)
  }
  invisible(TRUE)
}

# Converte vetores textuais para UTF-8.
# No modo automático, cada valor é classificado como UTF-8 válido ou legado
# Windows-1252. O retorno informa quantos valores foram convertidos ou perdidos.
normalizar_codificacao_texto <- function(x, codificacao = "auto") {
  validar_opcao_codificacao(codificacao)
  texto <- if (is.factor(x)) as.character(x) else x
  if (!is.character(texto)) {
    return(list(valores = x, convertidos = 0L, invalidos = 0L))
  }

  resultado <- texto
  nao_vazio <- !is.na(texto) & nzchar(texto)
  if (!any(nao_vazio)) {
    return(list(valores = resultado, convertidos = 0L, invalidos = 0L))
  }

  if (identical(codificacao, "auto")) {
    # A detecção por valor é necessária porque DBF/DTA podem reunir caracteres
    # já válidos em UTF-8 e bytes provenientes de codificações antigas.
    utf8_valido <- rep(TRUE, length(texto))
    utf8_valido[nao_vazio] <- stringi::stri_enc_isutf8(texto[nao_vazio])
    indices_utf8 <- which(nao_vazio & utf8_valido)
    indices_legado <- which(nao_vazio & !utf8_valido)

    if (length(indices_utf8)) {
      # Textos ASCII já têm exatamente a representação produzida por `iconv`.
      # Converter apenas os valores UTF-8 não ASCII evita cópias de vetores
      # extensos sem alterar conteúdo ou marcação de codificação.
      indices_utf8 <- indices_utf8[
        !stringi::stri_enc_isascii(texto[indices_utf8])
      ]
      if (length(indices_utf8)) {
        resultado[indices_utf8] <- iconv(
          texto[indices_utf8], from = "UTF-8", to = "UTF-8", sub = NA, mark = TRUE
        )
      }
    }
    if (length(indices_legado)) {
      resultado[indices_legado] <- iconv(
        texto[indices_legado], from = "Windows-1252", to = "UTF-8",
        sub = NA, mark = TRUE
      )
    }
    convertidos <- length(indices_legado)
  } else {
    indices <- which(nao_vazio)
    resultado[indices] <- iconv(
      texto[indices], from = codificacao, to = "UTF-8", sub = NA, mark = TRUE
    )
    convertidos <- length(indices)
  }

  invalidos <- sum(nao_vazio & is.na(resultado))
  list(
    valores = resultado,
    convertidos = as.integer(convertidos - invalidos),
    invalidos = as.integer(invalidos)
  )
}

# Produz uma representação textual estável para comparar tipos heterogêneos.
representar_valores <- function(x) {
  if (inherits(x, "Date")) return(ifelse(is.na(x), NA_character_, format(x, "%Y-%m-%d")))
  if (inherits(x, "POSIXt")) return(ifelse(is.na(x), NA_character_, format(x, "%Y-%m-%d %H:%M:%S")))
  if (is.factor(x)) return(as.character(x))
  if (is.numeric(x)) {
    return(ifelse(is.na(x), NA_character_, format(x, trim = TRUE, scientific = FALSE,
                                                  digits = 15)))
  }
  as.character(x)
}

# Versão textual que substitui NA por vazio, convenção usada na base final.
para_character_sem_na <- function(x) {
  resultado <- representar_valores(x)
  resultado[is.na(resultado)] <- ""
  resultado
}

# Conta quantas posições mudaram, tratando NA e texto de forma explícita.
contar_diferencas <- function(antes, depois) {
  # Datas já tipadas podem ser comparadas diretamente pelo valor numérico. Isso
  # evita formatar vetores inteiros duas vezes apenas para confirmar igualdade.
  if (inherits(antes, "Date") && inherits(depois, "Date")) {
    a <- unclass(antes)
    d <- unclass(depois)
    diferentes <- xor(is.na(a), is.na(d)) |
      (!is.na(a) & !is.na(d) & a != d)
    return(sum(diferentes))
  }
  a <- representar_valores(antes)
  d <- representar_valores(depois)
  diferentes <- xor(is.na(a), is.na(d)) |
    (!is.na(a) & !is.na(d) & a != d)
  sum(diferentes)
}

# Cria uma linha padronizada da trilha de auditoria de transformações.
linha_auditoria <- function(variavel, tipo, alterados = 0L, invalidos = 0L) {
  data.frame(
    variavel = variavel,
    tipo_transformacao = tipo,
    valores_alterados = as.integer(alterados),
    valores_invalidos = as.integer(invalidos),
    stringsAsFactors = FALSE
  )
}

# Renomeia somente variáveis conhecidas, sem modificar colunas desconhecidas.
# Em correspondências por caixa, dá preferência ao nome já escrito no padrão.
normalizar_nomes_esperados <- function(dados, esperadas) {
  nomes_originais <- names(dados)
  nomes_finais <- nomes_originais
  auditoria <- list()

  for (esperada in esperadas) {
    indices <- which(tolower(nomes_originais) == esperada)
    if (!length(indices)) next
    indice <- if (esperada %in% nomes_originais[indices]) {
      indices[match(esperada, nomes_originais[indices])]
    } else {
      indices[[1L]]
    }
    if (!identical(nomes_finais[[indice]], esperada)) {
      auditoria[[length(auditoria) + 1L]] <-
        linha_auditoria(esperada, "normalizacao_nome", 0L, 0L)
      nomes_finais[[indice]] <- esperada
    }
  }
  names(dados) <- nomes_finais
  list(dados = dados, auditoria = auditoria)
}

# Normaliza texto livre: maiúsculas, sem acentos/pontuação e espaços duplicados.
padronizar_campo_aberto <- function(x) {
  x <- para_character_sem_na(x)
  x <- stringi::stri_trans_toupper(x, locale = "pt_BR")
  x <- stringi::stri_trans_general(x, "NFD; [:Nonspacing Mark:] Remove; NFC")
  x <- stringi::stri_replace_all_regex(x, "[^A-Z0-9 ]", "")
  x <- stringi::stri_replace_all_regex(x, " +", " ")
  trimws(x)
}

# Converte datas já tipadas ou textos AAAAMMDD/AAAA-MM-DD para classe Date.
# Com `detalhes = TRUE`, também devolve as contagens usadas pela auditoria sem
# reformatar o vetor de datas resultante.
converter_data_ymd <- function(x, detalhes = FALSE) {
  if (inherits(x, "Date")) {
    if (!isTRUE(detalhes)) return(x)
    return(list(valores = x, alterados = 0L, invalidos = 0L))
  }
  if (inherits(x, "POSIXt")) {
    resultado <- as.Date(x)
    if (!isTRUE(detalhes)) return(resultado)
    return(list(
      valores = resultado,
      alterados = as.integer(sum(!is.na(x))),
      invalidos = as.integer(sum(!is.na(x) & is.na(resultado)))
    ))
  }

  texto_original <- representar_valores(x)
  texto <- trimws(texto_original)
  vazio <- is.na(texto) | texto == ""
  formato_aceito <- !vazio & grepl("^[0-9]{4}([^0-9]?[0-9]{2}){2}$", texto)
  digitos <- gsub("[^0-9]", "", texto)
  resultado <- rep(as.Date(NA), length(texto))
  indices_aceitos <- which(formato_aceito)
  if (length(indices_aceitos)) {
    datas_distintas <- unique(digitos[indices_aceitos])
    datas_convertidas <- as.Date(datas_distintas, format = "%Y%m%d")
    resultado[indices_aceitos] <- datas_convertidas[
      match(digitos[indices_aceitos], datas_distintas)
    ]
  }
  if (!isTRUE(detalhes)) return(resultado)

  representacao_saida <- rep(NA_character_, length(resultado))
  indices_validos <- which(!is.na(resultado))
  if (length(indices_validos)) {
    valor <- digitos[indices_validos]
    representacao_saida[indices_validos] <- paste0(
      substr(valor, 1L, 4L), "-", substr(valor, 5L, 6L), "-",
      substr(valor, 7L, 8L)
    )
  }
  diferentes <- xor(is.na(texto_original), is.na(representacao_saida)) |
    (!is.na(texto_original) & !is.na(representacao_saida) &
       texto_original != representacao_saida)
  list(
    valores = resultado,
    alterados = as.integer(sum(diferentes)),
    invalidos = as.integer(sum(!vazio & is.na(resultado)))
  )
}

# Valida um domínio numérico e formata seus códigos com dois caracteres.
padronizar_codigo_formatado <- function(x, validos) {
  texto <- trimws(para_character_sem_na(x))
  numero <- suppressWarnings(as.numeric(texto))
  inteiro <- !is.na(numero) & is.finite(numero) & numero == trunc(numero)
  valido <- texto == "" | (inteiro & numero %in% validos)
  resultado <- rep("", length(texto))
  resultado[texto != "" & valido] <- sprintf("%02d", as.integer(numero[texto != "" & valido]))
  list(
    valores = resultado,
    invalidos = sum(texto != "" & !valido),
    alterados = contar_diferencas(texto, resultado)
  )
}

# Lê a lista oficial de códigos municipais utilizada na validação territorial.
carregar_codigos_municipios <- function(caminho = NULL) {
  if (is.null(caminho)) caminho <- localizar_referencia("codigos_municipios.csv")
  referencia <- utils::read.csv(caminho, colClasses = c(codigo = "integer", tipo = "character"),
                                check.names = FALSE)
  unique(referencia$codigo)
}

# Faz todas as verificações que independem da transformação dos registros:
# estrutura da tabela, nomes, pacotes, codificação e referência municipal.
# Retorna todas as falhas encontradas em uma única passagem.
validar_pre_requisitos_padronizacao <- function(base_bruta, caminho_municipios = NULL,
                                                codificacao_texto = "auto") {
  erros <- character()
  caminho_resolvido <- caminho_municipios
  codigos_municipios <- NULL

  erro_codificacao <- tryCatch({
    validar_opcao_codificacao(codificacao_texto)
    NULL
  }, error = identity)
  if (!is.null(erro_codificacao)) {
    erros <- c(erros, conditionMessage(erro_codificacao))
  }

  if (!is.data.frame(base_bruta)) {
    erros <- c(erros, "A base bruta não é uma tabela do tipo data.frame.")
  } else {
    if (ncol(base_bruta) == 0L) {
      erros <- c(erros, "A base bruta não possui colunas reconhecíveis.")
    }
    nomes <- names(base_bruta)
    indices_sem_nome <- which(is.na(nomes) | !nzchar(nomes))
    if (length(indices_sem_nome)) {
      erros <- c(erros, sprintf(
        "Há colunas sem nome nas posições: %s.", paste(indices_sem_nome, collapse = ", ")
      ))
    }
    nomes_duplicados <- unique(nomes[duplicated(nomes) & !is.na(nomes) & nzchar(nomes)])
    if (length(nomes_duplicados)) {
      erros <- c(erros, sprintf(
        "Há nomes de colunas duplicados: %s.", paste(nomes_duplicados, collapse = ", ")
      ))
    }
    if (exists("obter_variaveis_esperadas", mode = "function")) {
      esperadas <- obter_variaveis_esperadas()
      nomes_validos <- nomes[!is.na(nomes)]
      conflitos <- vapply(esperadas, function(esperada) {
        sum(tolower(nomes_validos) == esperada) > 1L
      }, logical(1))
      if (any(conflitos)) {
        detalhes <- vapply(esperadas[conflitos], function(esperada) {
          correspondentes <- nomes_validos[tolower(nomes_validos) == esperada]
          sprintf("%s (%s)", esperada, paste(correspondentes, collapse = ", "))
        }, character(1))
        erros <- c(erros, paste0(
          "Há múltiplas colunas correspondentes à mesma variável esperada: ",
          paste(detalhes, collapse = "; "), "."
        ))
      }
    } else {
      erros <- c(erros, "As regras de variáveis esperadas não foram carregadas.")
    }
  }

  if (!requireNamespace("stringi", quietly = TRUE)) {
    erros <- c(erros, "O pacote obrigatório 'stringi' não está instalado ou disponível.")
  }

  if (is.null(caminho_resolvido)) {
    caminho_resolvido <- tryCatch(
      localizar_referencia("codigos_municipios.csv"),
      error = function(e) {
        erros <<- c(erros, paste0("Referência municipal indisponível: ", conditionMessage(e)))
        NULL
      }
    )
  } else if (!file.exists(caminho_resolvido)) {
    erros <- c(erros, sprintf("O arquivo de referência municipal não existe: %s.",
                              caminho_resolvido))
    caminho_resolvido <- NULL
  }

  if (!is.null(caminho_resolvido)) {
    referencia <- tryCatch(
      utils::read.csv(caminho_resolvido, check.names = FALSE,
                      colClasses = c(codigo = "integer", tipo = "character")),
      error = function(e) {
        erros <<- c(erros, paste0("Falha ao ler a referência municipal: ",
                                  conditionMessage(e)))
        NULL
      }
    )
    if (!is.null(referencia)) {
      colunas_necessarias <- c("codigo", "tipo")
      faltantes <- setdiff(colunas_necessarias, names(referencia))
      if (length(faltantes)) {
        erros <- c(erros, sprintf(
          "A referência municipal não contém as colunas obrigatórias: %s.",
          paste(faltantes, collapse = ", ")
        ))
      } else {
        if (!nrow(referencia)) {
          erros <- c(erros, "A referência municipal está vazia.")
        }
        if (anyNA(referencia$codigo)) {
          erros <- c(erros, "A referência municipal contém códigos vazios ou não numéricos.")
        }
        if (anyDuplicated(referencia$codigo)) {
          erros <- c(erros, "A referência municipal contém códigos duplicados.")
        }
        codigos_municipios <- unique(referencia$codigo[!is.na(referencia$codigo)])
      }
    }
  }

  list(
    erros = unique(erros),
    caminho_municipios = caminho_resolvido,
    codigos_municipios = codigos_municipios
  )
}

# Normaliza códigos municipais e aceita, além da lista, a faixa reservada ao DF.
padronizar_codigo_municipio <- function(x, codigos_validos) {
  texto <- trimws(para_character_sem_na(x))
  numero <- suppressWarnings(as.numeric(texto))
  inteiro <- !is.na(numero) & is.finite(numero) & numero == trunc(numero)
  em_lista <- inteiro & numero %in% codigos_validos
  na_faixa_df <- inteiro & numero >= 530000 & numero <= 539999
  valido <- texto == "" | em_lista | na_faixa_df
  resultado <- rep("", length(texto))
  resultado[texto != "" & valido] <- sprintf("%.0f", numero[texto != "" & valido])
  list(
    valores = resultado,
    invalidos = sum(texto != "" & !valido),
    alterados = contar_diferencas(texto, resultado)
  )
}

# Implementação interna do pipeline. `contexto` é atualizado antes de cada
# etapa para que o invólucro externo consiga informar exatamente onde ocorreu
# uma exceção inesperada.
padronizar_base_impl <- function(base_bruta, codigos_municipios, codificacao_texto,
                                 contexto) {
  esperadas <- obter_variaveis_esperadas()
  dados <- base_bruta
  auditoria <- list()

  # 1. Normaliza a codificação de todas as colunas originalmente textuais.
  for (variavel in names(dados)) {
    if (!is.character(dados[[variavel]]) && !is.factor(dados[[variavel]])) next
    contexto$etapa <- "normalização da codificação de caracteres"
    contexto$variavel <- variavel
    resultado_codificacao <- normalizar_codificacao_texto(
      dados[[variavel]], codificacao = codificacao_texto
    )
    if (resultado_codificacao$invalidos > 0L) {
      stop(sprintf(
        "%s valor(es) não puderam ser convertidos de %s para UTF-8.",
        resultado_codificacao$invalidos,
        if (identical(codificacao_texto, "auto")) "Windows-1252 (modo automático)" else codificacao_texto
      ), call. = FALSE)
    }
    dados[[variavel]] <- resultado_codificacao$valores
    auditoria[[length(auditoria) + 1L]] <- linha_auditoria(
      variavel,
      paste0("normalizacao_codificacao_", codificacao_texto),
      resultado_codificacao$convertidos,
      resultado_codificacao$invalidos
    )
  }

  # 2. Uniformiza nomes conhecidos e registra quais existiam na origem.
  contexto$etapa <- "normalização dos nomes das variáveis"
  contexto$variavel <- NA_character_
  normalizacao <- normalizar_nomes_esperados(dados, esperadas)
  dados <- normalizacao$dados
  auditoria <- c(auditoria, normalizacao$auditoria)

  variaveis_originais <- names(dados)
  presentes <- esperadas[esperadas %in% names(dados)]
  ausentes <- setdiff(esperadas, names(dados))
  # 3. Cria campos esperados ausentes com vazio, preservando a lista de origem
  # para que as análises possam declarar regras não executáveis corretamente.
  if (length(ausentes)) {
    for (variavel in ausentes) {
      contexto$etapa <- "criação das variáveis esperadas ausentes"
      contexto$variavel <- variavel
      dados[[variavel]] <- rep("", nrow(dados))
      auditoria[[length(auditoria) + 1L]] <-
        linha_auditoria(variavel, "criacao_variavel_ausente", 0L, 0L)
    }
  }

  # 4. Converte campos de largura fixa para texto e aplica o limite declarado.
  for (largura_nome in names(larguras_campos)) {
    largura <- as.integer(largura_nome)
    for (variavel in larguras_campos[[largura_nome]]) {
      contexto$etapa <- sprintf("conversão para texto e truncamento em %s caracteres", largura)
      contexto$variavel <- variavel
      if (variavel %in% ausentes) {
        auditoria[[length(auditoria) + 1L]] <- linha_auditoria(
          variavel, sprintf("conversao_texto_truncamento_%s", largura), 0L, 0L
        )
        next
      }
      antes <- dados[[variavel]]
      convertido <- para_character_sem_na(antes)
      depois <- substr(convertido, 1L, largura)
      auditoria[[length(auditoria) + 1L]] <- linha_auditoria(
        variavel, sprintf("conversao_texto_truncamento_%s", largura),
        contar_diferencas(convertido, depois), 0L
      )
      dados[[variavel]] <- depois
    }
  }

  # 5. Limpa os campos textuais livres usados em comparações e relatórios.
  for (variavel in campos_abertos) {
    contexto$etapa <- "normalização de campo aberto"
    contexto$variavel <- variavel
    if (variavel %in% ausentes) {
      auditoria[[length(auditoria) + 1L]] <- linha_auditoria(
        variavel, "normalizacao_campo_aberto", 0L, 0L
      )
      next
    }
    antes <- dados[[variavel]]
    depois <- padronizar_campo_aberto(antes)
    auditoria[[length(auditoria) + 1L]] <- linha_auditoria(
      variavel, "normalizacao_campo_aberto", contar_diferencas(antes, depois), 0L
    )
    dados[[variavel]] <- depois
  }

  # 6. Converte datas e contabiliza valores preenchidos que não puderam ser lidos.
  total_datas_invalidas <- 0L
  for (variavel in variaveis_data) {
    contexto$etapa <- "conversão de data no padrão YMD"
    contexto$variavel <- variavel
    if (variavel %in% ausentes) {
      dados[[variavel]] <- rep(as.Date(NA), nrow(dados))
      auditoria[[length(auditoria) + 1L]] <- linha_auditoria(
        variavel, "conversao_data_ymd", nrow(dados), 0L
      )
      next
    }
    antes <- dados[[variavel]]
    resultado_data <- converter_data_ymd(antes, detalhes = TRUE)
    depois <- resultado_data$valores
    invalidos <- resultado_data$invalidos
    total_datas_invalidas <- total_datas_invalidas + invalidos
    auditoria[[length(auditoria) + 1L]] <- linha_auditoria(
      variavel, "conversao_data_ymd", resultado_data$alterados, invalidos
    )
    dados[[variavel]] <- depois
  }

  # 7. Valida domínios; códigos não previstos tornam-se vazios e são auditados.
  total_codigos_invalidos <- 0L
  for (variavel in names(dominios_categoricos)) {
    contexto$etapa <- "validação de domínio categórico"
    contexto$variavel <- variavel
    if (variavel %in% ausentes) {
      auditoria[[length(auditoria) + 1L]] <- linha_auditoria(
        variavel, "validacao_dominio", 0L, 0L
      )
      next
    }
    antes <- para_character_sem_na(dados[[variavel]])
    invalido <- antes != "" & !antes %in% dominios_categoricos[[variavel]]
    depois <- antes
    depois[invalido] <- ""
    quantidade <- sum(invalido)
    total_codigos_invalidos <- total_codigos_invalidos + quantidade
    auditoria[[length(auditoria) + 1L]] <- linha_auditoria(
      variavel, "validacao_dominio", contar_diferencas(antes, depois), quantidade
    )
    dados[[variavel]] <- depois
  }

  # 8. Formata campos numéricos de domínio específico com dois dígitos.
  for (variavel in names(dominios_formatados)) {
    contexto$etapa <- "formatação e validação de código"
    contexto$variavel <- variavel
    if (variavel %in% ausentes) {
      auditoria[[length(auditoria) + 1L]] <- linha_auditoria(
        variavel, "formatacao_validacao_codigo", 0L, 0L
      )
      next
    }
    resultado <- padronizar_codigo_formatado(dados[[variavel]], dominios_formatados[[variavel]])
    total_codigos_invalidos <- total_codigos_invalidos + resultado$invalidos
    auditoria[[length(auditoria) + 1L]] <- linha_auditoria(
      variavel, "formatacao_validacao_codigo", resultado$alterados, resultado$invalidos
    )
    dados[[variavel]] <- resultado$valores
  }

  # 9. Valida os municípios contra a referência territorial.
  total_municipios_invalidos <- 0L
  for (variavel in variaveis_municipio) {
    contexto$etapa <- "validação de código de município"
    contexto$variavel <- variavel
    if (variavel %in% ausentes) {
      auditoria[[length(auditoria) + 1L]] <- linha_auditoria(
        variavel, "validacao_codigo_municipio", 0L, 0L
      )
      next
    }
    resultado <- padronizar_codigo_municipio(dados[[variavel]], codigos_municipios)
    total_municipios_invalidos <- total_municipios_invalidos + resultado$invalidos
    auditoria[[length(auditoria) + 1L]] <- linha_auditoria(
      variavel, "validacao_codigo_municipio", resultado$alterados, resultado$invalidos
    )
    dados[[variavel]] <- resultado$valores
  }

  # 10. Consolida a base final, a auditoria e os totais consumidos pela interface.
  tabela_auditoria <- do.call(rbind, auditoria)
  rownames(tabela_auditoria) <- NULL
  tabela_ausentes <- data.frame(variavel = ausentes, stringsAsFactors = FALSE)

  list(
    base_padronizada = dados,
    variaveis_originais = variaveis_originais,
    variaveis_ausentes = tabela_ausentes,
    auditoria = tabela_auditoria,
    resumo = list(
      registros = nrow(dados),
      variaveis = ncol(dados),
      variaveis_esperadas = length(esperadas),
      variaveis_presentes = length(presentes),
      variaveis_ausentes = length(ausentes),
      codigos_invalidos = total_codigos_invalidos,
      datas_invalidas = total_datas_invalidas,
      municipios_invalidos = total_municipios_invalidos
    )
  )
}

# Ponto público de entrada. Valida primeiro e envolve qualquer falha posterior
# com etapa e variável, mantendo a mensagem útil para diagnóstico e manutenção.
padronizar_base <- function(base_bruta, caminho_municipios = NULL,
                            codificacao_texto = "auto") {
  validacao <- validar_pre_requisitos_padronizacao(
    base_bruta, caminho_municipios, codificacao_texto
  )
  if (length(validacao$erros)) interromper_padronizacao(validacao$erros)

  contexto <- new.env(parent = emptyenv())
  contexto$etapa <- "inicialização"
  contexto$variavel <- NA_character_

  tryCatch(
    padronizar_base_impl(
      base_bruta, validacao$codigos_municipios, codificacao_texto, contexto
    ),
    erro_padronizacao = function(e) stop(e),
    error = function(e) {
      alvo <- if (!is.na(contexto$variavel) && nzchar(contexto$variavel)) {
        sprintf(" na variável '%s'", contexto$variavel)
      } else {
        ""
      }
      interromper_padronizacao(sprintf(
        "Falha durante %s%s: %s", contexto$etapa, alvo, conditionMessage(e)
      ))
    }
  )
}
