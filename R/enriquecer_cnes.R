# Enriquecimento mensal com estabelecimentos do CNES -------------------------
#
# Determina uma única competência a partir da base padronizada, baixa a
# fotografia mensal CNES-ST e acrescenta somente o tipo da unidade. A referência
# estática `data/reference/cnes.xlsx` não participa deste fluxo.

# Converte anos preenchidos para quatro algarismos e distingue ausências de
# valores inválidos. O vetor normalizado é usado somente para escolher o período.
normalizar_anos_cnes <- function(x) {
  if (is.null(x)) x <- character()
  texto <- trimws(as.character(x))
  ausente <- is.na(x) | is.na(texto) | !nzchar(texto)
  texto[ausente] <- NA_character_
  texto <- sub("\\.0+$", "", texto)
  valido <- !is.na(texto) & grepl("^[0-9]{4}$", texto)
  normalizado <- rep(NA_character_, length(texto))
  normalizado[valido] <- texto[valido]
  list(
    valores = normalizado,
    ausentes = sum(ausente),
    invalidos = sum(!ausente & !valido)
  )
}

# Aceita as datas já convertidas pela padronização ou os dois formatos textuais
# admitidos defensivamente. Também conserva contagens separadas para a interface.
normalizar_datas_cnes <- function(x) {
  if (inherits(x, "Date")) {
    ausente <- is.na(x)
    return(list(
      valores = as.Date(x),
      ausentes = sum(ausente),
      invalidos = 0L,
      ausente = ausente,
      invalido = rep(FALSE, length(x))
    ))
  }

  texto <- trimws(as.character(x))
  ausente <- is.na(x) | is.na(texto) | !nzchar(texto)
  texto[ausente] <- NA_character_
  formato_compacto <- !is.na(texto) & grepl("^[0-9]{8}$", texto)
  formato_iso <- !is.na(texto) & grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", texto)
  datas <- rep(as.Date(NA), length(texto))
  datas[formato_compacto] <- as.Date(texto[formato_compacto], format = "%Y%m%d")
  datas[formato_iso] <- as.Date(texto[formato_iso], format = "%Y-%m-%d")
  invalido <- !ausente & is.na(datas)

  list(
    valores = datas,
    ausentes = sum(ausente),
    invalidos = sum(invalido),
    ausente = ausente,
    invalido = invalido
  )
}

# Função pura que escolhe primeiro o maior `nu_ano` válido e somente então o
# maior mês de `dt_notific` entre os registros daquele ano.
determinar_periodo_cnes <- function(base_padronizada) {
  if (!is.data.frame(base_padronizada)) {
    stop("A base padronizada deve ser uma tabela do tipo data.frame.", call. = FALSE)
  }
  if (!"nu_ano" %in% names(base_padronizada)) {
    stop(
      "Não foi possível determinar o período do CNES: a variável 'nu_ano' está ausente.",
      call. = FALSE
    )
  }

  anos <- normalizar_anos_cnes(base_padronizada$nu_ano)
  anos_validos <- anos$valores[!is.na(anos$valores)]
  if (!length(anos_validos)) {
    stop(
      "Não foi possível determinar o período do CNES: 'nu_ano' não possui nenhum ano válido.",
      call. = FALSE
    )
  }
  ano_referencia_texto <- max(anos_validos)
  ano_referencia <- as.integer(ano_referencia_texto)

  if (!"dt_notific" %in% names(base_padronizada)) {
    stop(sprintf(
      "Não foi possível determinar o mês do CNES para %s: a variável 'dt_notific' está ausente.",
      ano_referencia_texto
    ), call. = FALSE)
  }
  datas <- normalizar_datas_cnes(base_padronizada$dt_notific)
  no_ano_referencia <- !is.na(anos$valores) & anos$valores == ano_referencia_texto
  datas_referencia <- datas$valores[no_ano_referencia]
  datas_validas_referencia <- datas_referencia[!is.na(datas_referencia)]
  if (!length(datas_validas_referencia)) {
    stop(sprintf(
      "Não foi possível determinar o mês do CNES: não há 'dt_notific' válida entre os registros com 'nu_ano' igual a %s.",
      ano_referencia_texto
    ), call. = FALSE)
  }

  meses <- as.integer(format(datas_validas_referencia, "%m"))
  mes_referencia <- max(meses)
  anos_das_datas <- rep(NA_character_, length(datas$valores))
  datas_validas <- !is.na(datas$valores)
  anos_das_datas[datas_validas] <- format(datas$valores[datas_validas], "%Y")
  divergencia <- !is.na(anos$valores) & datas_validas &
    anos$valores != anos_das_datas

  list(
    ano_referencia = ano_referencia,
    mes_referencia = mes_referencia,
    competencia = sprintf("%04d%02d", ano_referencia, mes_referencia),
    anos_ausentes = anos$ausentes,
    anos_invalidos = anos$invalidos,
    datas_ausentes = datas$ausentes,
    datas_invalidas = datas$invalidos,
    datas_ausentes_ano_referencia = sum(datas$ausente[no_ano_referencia]),
    datas_invalidas_ano_referencia = sum(datas$invalido[no_ano_referencia]),
    divergencias_ano_data = sum(divergencia),
    divergencias_ano_data_ano_referencia = sum(divergencia & no_ano_referencia)
  )
}

# Normaliza códigos CNES numéricos ou textuais para sete posições. Conteúdos
# vazios ou incompatíveis tornam-se NA e nunca podem produzir correspondência.
normalizar_codigo_cnes <- function(x) {
  texto <- trimws(as.character(x))
  texto[is.na(x) | is.na(texto) | !nzchar(texto)] <- NA_character_
  texto <- sub("\\.0+$", "", texto)
  valido <- !is.na(texto) & grepl("^[0-9]{1,7}$", texto)
  resultado <- rep(NA_character_, length(texto))
  resultado[valido] <- sprintf("%07d", as.integer(texto[valido]))
  resultado
}

# Padroniza a competência devolvida pelo CNES para permitir a validação exata
# do único mês solicitado.
normalizar_competencia_cnes <- function(x) {
  texto <- trimws(as.character(x))
  texto[is.na(x) | is.na(texto) | !nzchar(texto)] <- NA_character_
  texto <- sub("\\.0+$", "", texto)
  valido <- !is.na(texto) & grepl("^[0-9]{6}$", texto)
  resultado <- rep(NA_character_, length(texto))
  resultado[valido] <- texto[valido]
  resultado
}

# Seleciona as três colunas solicitadas, valida a competência e reduz somente
# duplicações perfeitamente equivalentes. Conflitos de TP_UNID são impeditivos.
preparar_referencia_cnes <- function(dados_cnes, competencia_solicitada) {
  if (!is.data.frame(dados_cnes) || !nrow(dados_cnes)) {
    stop("O download do CNES não retornou nenhum estabelecimento.", call. = FALSE)
  }
  nomes <- toupper(trimws(names(dados_cnes)))
  esperadas <- c("CNES", "COMPETEN", "TP_UNID")
  ausentes <- setdiff(esperadas, nomes)
  if (length(ausentes)) {
    stop(sprintf(
      "O download do CNES não contém as variáveis obrigatórias: %s.",
      paste(ausentes, collapse = ", ")
    ), call. = FALSE)
  }
  if (anyDuplicated(nomes[nomes %in% esperadas])) {
    stop("O download do CNES contém nomes de variáveis obrigatórias duplicados.", call. = FALSE)
  }

  referencia <- data.frame(
    CNES = normalizar_codigo_cnes(dados_cnes[[match("CNES", nomes)]]),
    COMPETEN = normalizar_competencia_cnes(dados_cnes[[match("COMPETEN", nomes)]]),
    TP_UNID = trimws(as.character(dados_cnes[[match("TP_UNID", nomes)]])),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  referencia$TP_UNID[is.na(referencia$TP_UNID) | !nzchar(referencia$TP_UNID)] <- NA_character_

  competencia_solicitada <- as.character(competencia_solicitada)
  competencia_invalida <- is.na(referencia$COMPETEN) |
    referencia$COMPETEN != competencia_solicitada
  if (any(competencia_invalida)) {
    encontradas <- unique(referencia$COMPETEN[!is.na(referencia$COMPETEN)])
    detalhe <- if (length(encontradas)) paste(encontradas, collapse = ", ") else "nenhuma válida"
    stop(sprintf(
      "O CNES retornado não corresponde integralmente à competência %s (encontradas: %s).",
      competencia_solicitada, detalhe
    ), call. = FALSE)
  }

  cnes_invalidos <- sum(is.na(referencia$CNES))
  referencia_valida <- referencia[!is.na(referencia$CNES), , drop = FALSE]
  if (!nrow(referencia_valida)) {
    stop("O download do CNES não contém códigos CNES válidos.", call. = FALSE)
  }

  chaves <- paste(referencia_valida$CNES, referencia_valida$COMPETEN, sep = "|")
  grupos <- split(seq_len(nrow(referencia_valida)), chaves)
  conflitantes <- vapply(grupos, function(indices) {
    length(unique(ifelse(is.na(referencia_valida$TP_UNID[indices]),
                         "<AUSENTE>", referencia_valida$TP_UNID[indices]))) > 1L
  }, logical(1))
  if (any(conflitantes)) {
    codigos <- unique(referencia_valida$CNES[unlist(grupos[conflitantes], use.names = FALSE)])
    stop(sprintf(
      "Há valores conflitantes de TP_UNID para o mesmo CNES na competência %s: %s%s.",
      competencia_solicitada,
      paste(utils::head(codigos, 10L), collapse = ", "),
      if (length(codigos) > 10L) " (e outros)" else ""
    ), call. = FALSE)
  }

  manter <- !duplicated(chaves)
  duplicacoes_reduzidas <- sum(!manter)
  referencia_valida <- referencia_valida[manter, esperadas, drop = FALSE]
  rownames(referencia_valida) <- NULL

  list(
    referencia = referencia_valida,
    cnes_invalidos_descartados = cnes_invalidos,
    duplicacoes_reduzidas = duplicacoes_reduzidas
  )
}

# Baixa exatamente uma competência CNES-ST. `funcao_download` permite testes
# determinísticos sem acesso à rede, mantendo a chamada de produção explícita.
baixar_referencia_cnes <- function(periodo, funcao_download = NULL) {
  if (is.null(funcao_download)) {
    if (!requireNamespace("microdatasus", quietly = TRUE)) {
      stop(
        "O pacote 'microdatasus' não está instalado. Execute renv::restore() antes de enriquecer a base.",
        call. = FALSE
      )
    }
    funcao_download <- microdatasus::fetch_datasus
  }

  dados <- funcao_download(
    year_start = periodo$ano_referencia,
    month_start = periodo$mes_referencia,
    year_end = periodo$ano_referencia,
    month_end = periodo$mes_referencia,
    uf = "all",
    information_system = "CNES-ST",
    vars = c("CNES", "COMPETEN", "TP_UNID"),
    stop_on_error = TRUE
  )
  preparar_referencia_cnes(dados, periodo$competencia)
}

# Faz o relacionamento exclusivamente por `id_unidade = CNES`, por meio de
# `match`, o que impede multiplicação e conserva rigorosamente a ordem original.
relacionar_base_cnes <- function(base_padronizada, referencia_cnes) {
  if (!is.data.frame(base_padronizada)) {
    stop("A base padronizada deve ser uma tabela do tipo data.frame.", call. = FALSE)
  }
  if (!"id_unidade" %in% names(base_padronizada)) {
    stop("Não foi possível enriquecer: a variável 'id_unidade' está ausente.", call. = FALSE)
  }
  if (!is.data.frame(referencia_cnes) ||
      !all(c("CNES", "COMPETEN", "TP_UNID") %in% names(referencia_cnes))) {
    stop("A referência CNES preparada está incompleta.", call. = FALSE)
  }
  if (anyDuplicated(referencia_cnes$CNES)) {
    stop("A referência CNES preparada ainda contém códigos duplicados.", call. = FALSE)
  }

  ids <- normalizar_codigo_cnes(base_padronizada$id_unidade)
  indice <- match(ids, referencia_cnes$CNES)
  indice[is.na(ids)] <- NA_integer_
  resultado <- base_padronizada
  resultado$tp_unid <- as.character(referencia_cnes$TP_UNID[indice])

  if (nrow(resultado) != nrow(base_padronizada)) {
    stop("O relacionamento com o CNES alterou indevidamente o número de registros.", call. = FALSE)
  }

  list(
    base_enriquecida = resultado,
    resumo = list(
      registros = nrow(resultado),
      unidades_localizadas = sum(!is.na(indice)),
      unidades_nao_localizadas = sum(!is.na(ids) & is.na(indice)),
      ids_ausentes_ou_invalidos = sum(is.na(ids))
    )
  )
}

# Orquestra a seleção, um único download e o vínculo da mesma fotografia CNES
# a todos os registros, sem considerar qualquer variável temporal no linkage.
enriquecer_base_cnes <- function(base_padronizada, funcao_download = NULL) {
  periodo <- determinar_periodo_cnes(base_padronizada)
  download <- baixar_referencia_cnes(periodo, funcao_download = funcao_download)
  relacionamento <- relacionar_base_cnes(base_padronizada, download$referencia)
  relacionamento$resumo$competencia <- periodo$competencia
  relacionamento$resumo$cnes_invalidos_descartados <- download$cnes_invalidos_descartados
  relacionamento$resumo$duplicacoes_reduzidas <- download$duplicacoes_reduzidas
  relacionamento$resumo$anos_ausentes <- periodo$anos_ausentes
  relacionamento$resumo$anos_invalidos <- periodo$anos_invalidos
  relacionamento$resumo$datas_ausentes <- periodo$datas_ausentes
  relacionamento$resumo$datas_invalidas <- periodo$datas_invalidas
  relacionamento$resumo$divergencias_ano_data <- periodo$divergencias_ano_data
  relacionamento$periodo <- periodo
  relacionamento$referencia_cnes <- download$referencia
  relacionamento
}
