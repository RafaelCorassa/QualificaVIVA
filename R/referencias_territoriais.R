# Referências territoriais e de estabelecimentos ------------------------------
#
# Centraliza leitura, validação e relacionamento das tabelas auxiliares de UF,
# município e CNES. Os códigos permanecem como texto para preservar zeros à
# esquerda e evitar alterações em identificadores administrativos.

# Limpa um código, aceita a representação numérica terminada em `.0`, completa
# zeros à esquerda e devolve NA quando o conteúdo ou a largura são inválidos.
normalizar_codigo_territorial <- function(x, largura) {
  valor <- trimws(as.character(x))
  valor[is.na(x) | !nzchar(valor)] <- NA_character_
  valor <- sub("\\.0+$", "", valor)
  somente_digitos <- !is.na(valor) & grepl("^[0-9]+$", valor)
  valor[!somente_digitos] <- NA_character_
  valor[somente_digitos] <- sprintf(paste0("%0", largura, "d"), as.integer(valor[somente_digitos]))
  valor[nchar(valor) != largura] <- NA_character_
  valor
}

# Confirma o esquema mínimo de uma tabela de referência.
validar_colunas_referencia <- function(dados, esperadas, nome) {
  ausentes <- setdiff(esperadas, names(dados))
  if (length(ausentes)) {
    stop(sprintf(
      "A referência %s não contém: %s.", nome, paste(ausentes, collapse = ", ")
    ), call. = FALSE)
  }
  invisible(dados)
}

# Lê CSV em UTF-8, forçando texto para não perder zeros de códigos.
ler_referencia_csv <- function(caminho, esperadas, nome) {
  if (!file.exists(caminho)) {
    stop(sprintf("Arquivo de referência não encontrado: %s.", caminho), call. = FALSE)
  }
  dados <- utils::read.csv(
    caminho, stringsAsFactors = FALSE, check.names = FALSE,
    colClasses = "character", fileEncoding = "UTF-8"
  )
  validar_colunas_referencia(dados, esperadas, nome)
  dados
}

# Indica se existe uma lista CNES utilizável para traduzir códigos em nomes.
referencia_cnes_disponivel <- function(referencias) {
  !is.null(referencias$cnes) && is.data.frame(referencias$cnes) && nrow(referencias$cnes) > 0L
}

# Localiza preferencialmente `cnes.xlsx` e, na ausência, `cnes.csv`.
# Aceita nomes alternativos de colunas, mas produz sempre `id_unidade` e
# `nome_unidade`. Linhas completamente vazias são ignoradas; códigos preenchidos
# devem ser válidos e únicos. Nome vazio é permitido e será sinalizado no resumo.
ler_referencia_cnes <- function(diretorio) {
  candidatos <- file.path(diretorio, c("cnes.xlsx", "cnes.csv"))
  caminho <- candidatos[file.exists(candidatos)][1]
  if (is.na(caminho)) {
    return(list(dados = NULL, arquivo = NULL))
  }

  extensao <- tolower(tools::file_ext(caminho))
  if (identical(extensao, "xlsx")) {
    # Todas as colunas são lidas como texto para manter CNES de sete dígitos.
    if (!requireNamespace("readxl", quietly = TRUE)) {
      stop("A leitura de cnes.xlsx requer a biblioteca readxl.", call. = FALSE)
    }
    planilhas <- readxl::excel_sheets(caminho)
    indice_cnes <- match("cnes", tolower(planilhas))
    planilha <- if (is.na(indice_cnes)) planilhas[[1]] else planilhas[[indice_cnes]]
    dados <- as.data.frame(
      readxl::read_excel(caminho, sheet = planilha, col_types = "text", na = ""),
      stringsAsFactors = FALSE, check.names = FALSE
    )
  } else {
    dados <- utils::read.csv(
      caminho, stringsAsFactors = FALSE, check.names = FALSE,
      colClasses = "character", fileEncoding = "UTF-8"
    )
  }

  names(dados) <- tolower(trimws(names(dados)))
  coluna_codigo <- intersect(c("cnes", "id_unidade", "codigo_cnes"), names(dados))
  coluna_nome <- intersect(
    c("fantasia", "nome_estabelecimento", "nome_unidade", "estabelecimento"),
    names(dados)
  )
  if (!length(coluna_codigo) || !length(coluna_nome)) {
    stop(sprintf(
      "%s deve conter as colunas 'cnes' e 'fantasia'.",
      basename(caminho)
    ), call. = FALSE)
  }

  # `preenchidas` exclui somente linhas sem código e sem nome; não confunde o
  # número da linha da planilha com o próprio código CNES.
  codigo_original <- trimws(as.character(dados[[coluna_codigo[[1]]]]))
  fantasia <- trimws(as.character(dados[[coluna_nome[[1]]]]))
  codigo_original[is.na(codigo_original)] <- ""
  fantasia[is.na(fantasia)] <- ""
  preenchidas <- nzchar(codigo_original) | nzchar(fantasia)
  codigo <- normalizar_codigo_territorial(codigo_original, 7L)
  invalidas <- which(preenchidas & is.na(codigo))
  if (length(invalidas)) {
    linhas <- paste(utils::head(invalidas + 1L, 10L), collapse = ", ")
    sufixo <- if (length(invalidas) > 10L) " (e outras)" else ""
    stop(sprintf(
      "%s contém linhas com código CNES inválido: %s%s.",
      basename(caminho), linhas, sufixo
    ), call. = FALSE)
  }

  fantasia[!nzchar(fantasia)] <- NA_character_

  cnes <- data.frame(
    id_unidade = codigo[preenchidas],
    nome_unidade = fantasia[preenchidas],
    stringsAsFactors = FALSE
  )
  duplicados <- unique(cnes$id_unidade[duplicated(cnes$id_unidade)])
  if (length(duplicados)) {
    stop(sprintf(
      "%s contém códigos CNES duplicados: %s.",
      basename(caminho), paste(utils::head(duplicados, 10L), collapse = ", ")
    ), call. = FALSE)
  }
  list(dados = cnes, arquivo = normalizePath(caminho, winslash = "/", mustWork = TRUE))
}

# Carrega e valida o conjunto completo de referências.
# A malha geográfica é opcional: só é lida quando solicitada, existente e com o
# pacote `sf` disponível. As tabelas cadastrais, por sua vez, são obrigatórias.
carregar_referencias_territoriais <- function(
    diretorio = file.path("data", "reference"), carregar_geografia = TRUE,
    carregar_cnes = TRUE) {
  ufs <- ler_referencia_csv(
    file.path(diretorio, "ufs.csv"),
    c("codigo_uf", "sigla_uf", "nome_uf", "codigo_regiao", "nome_regiao"),
    "de UFs"
  )
  municipios <- ler_referencia_csv(
    file.path(diretorio, "municipios.csv"),
    c("codigo_sinan_6", "codigo_ibge_7", "nome_municipio", "codigo_uf",
      "sigla_uf", "codigo_regiao", "nome_regiao"),
    "de municípios"
  )

  ufs$codigo_uf <- normalizar_codigo_territorial(ufs$codigo_uf, 2L)
  ufs$codigo_regiao <- normalizar_codigo_territorial(ufs$codigo_regiao, 1L)
  municipios$codigo_sinan_6 <- normalizar_codigo_territorial(municipios$codigo_sinan_6, 6L)
  municipios$codigo_ibge_7 <- normalizar_codigo_territorial(municipios$codigo_ibge_7, 7L)
  municipios$codigo_uf <- normalizar_codigo_territorial(municipios$codigo_uf, 2L)
  municipios$codigo_regiao <- normalizar_codigo_territorial(municipios$codigo_regiao, 1L)

  if (anyDuplicated(ufs$codigo_uf)) {
    stop("A referência de UFs contém códigos duplicados.", call. = FALSE)
  }
  if (anyDuplicated(municipios$codigo_sinan_6)) {
    stop("A referência de municípios contém códigos SINAN duplicados.", call. = FALSE)
  }
  if (any(!municipios$codigo_uf %in% ufs$codigo_uf)) {
    stop("A referência de municípios contém UFs inexistentes em ufs.csv.", call. = FALSE)
  }

  # Ordenação determinística melhora a consistência de tabelas e gráficos.
  ordem_regioes <- c("1", "2", "3", "4", "5")
  ufs <- ufs[order(match(ufs$codigo_regiao, ordem_regioes), as.integer(ufs$codigo_uf)), , drop = FALSE]
  municipios <- municipios[order(
    as.integer(municipios$codigo_uf), municipios$nome_municipio,
    municipios$codigo_sinan_6
  ), , drop = FALSE]

  resultado_cnes <- if (isTRUE(carregar_cnes)) {
    ler_referencia_cnes(diretorio)
  } else {
    list(dados = NULL, arquivo = NULL)
  }
  cnes <- resultado_cnes$dados

  geografia <- NULL
  caminho_geo <- file.path(diretorio, "geografia", "ufs.geojson")
  if (isTRUE(carregar_geografia) && file.exists(caminho_geo) && requireNamespace("sf", quietly = TRUE)) {
    geografia <- suppressWarnings(sf::st_read(caminho_geo, quiet = TRUE, stringsAsFactors = FALSE))
    coluna_geo <- intersect(c("codigo_uf", "codarea", "CD_UF", "id"), names(geografia))
    if (length(coluna_geo)) {
      geografia$codigo_uf <- normalizar_codigo_territorial(geografia[[coluna_geo[[1]]]], 2L)
    } else {
      stop("A malha de UFs não possui uma coluna de código reconhecida.", call. = FALSE)
    }
  }

  list(
    ufs = ufs, municipios = municipios, cnes = cnes,
    arquivo_cnes = resultado_cnes$arquivo, geografia_ufs = geografia
  )
}

# Produz as opções municipais reconhecidas e presentes na base atual. O vetor
# nomeado é adequado ao selectize: o valor é o código SINAN e o rótulo permite
# pesquisar por código, nome e sigla da UF sem ambiguidades.
listar_municipios_disponiveis <- function(base_padronizada, referencias) {
  if (!is.data.frame(base_padronizada) || !"id_municip" %in% names(base_padronizada)) {
    return(stats::setNames(character(), character()))
  }
  codigos <- unique(normalizar_codigo_territorial(base_padronizada$id_municip, 6L))
  municipios <- referencias$municipios[
    referencias$municipios$codigo_sinan_6 %in% codigos, , drop = FALSE
  ]
  municipios <- municipios[order(
    as.integer(municipios$codigo_uf), municipios$nome_municipio,
    municipios$codigo_sinan_6
  ), , drop = FALSE]
  stats::setNames(
    municipios$codigo_sinan_6,
    paste0(
      municipios$codigo_sinan_6, " — ", municipios$nome_municipio,
      " (", municipios$sigla_uf, ")"
    )
  )
}

# Relaciona cada registro analisado às referências de UF, município e CNES.
# Também anexa os indicadores de validade, produzindo a tabela-base usada por
# todas as agregações territoriais do relatório.
preparar_registros_territoriais <- function(base_padronizada, resultado_validade,
                                             referencias, incluir_regras = TRUE) {
  if (!is.data.frame(base_padronizada) || !is.list(resultado_validade)) {
    stop("Base padronizada ou resultado de validade inválido.", call. = FALSE)
  }
  por_registro <- resultado_validade$por_registro
  if (!is.data.frame(por_registro) || nrow(por_registro) != nrow(base_padronizada)) {
    stop("O resultado de validade não corresponde à base padronizada atual.", call. = FALSE)
  }
  obrigatorias <- c("sg_uf_not", "id_municip", "id_unidade")
  ausentes <- setdiff(obrigatorias, names(base_padronizada))
  if (length(ausentes)) {
    stop(sprintf(
      "O relatório territorial requer as variáveis: %s.", paste(ausentes, collapse = ", ")
    ), call. = FALSE)
  }

  uf <- normalizar_codigo_territorial(base_padronizada$sg_uf_not, 2L)
  municipio <- normalizar_codigo_territorial(base_padronizada$id_municip, 6L)
  unidade <- normalizar_codigo_territorial(base_padronizada$id_unidade, 7L)
  indice_uf <- match(uf, referencias$ufs$codigo_uf)
  indice_municipio <- match(municipio, referencias$municipios$codigo_sinan_6)
  possui_cnes <- referencia_cnes_disponivel(referencias)
  indice_cnes <- if (!possui_cnes) rep(NA_integer_, length(unidade)) else {
    match(unidade, referencias$cnes$id_unidade)
  }

  # A expressão inclui regras numéricas e sufixos alfabéticos, se existirem.
  regras <- if (isTRUE(incluir_regras)) {
    grep("^regra_[0-9]+[a-z]?$", names(por_registro), value = TRUE)
  } else {
    character()
  }
  registros <- data.frame(
    id_registro = seq_len(nrow(base_padronizada)),
    codigo_uf = uf,
    sigla_uf = referencias$ufs$sigla_uf[indice_uf],
    nome_uf = referencias$ufs$nome_uf[indice_uf],
    codigo_regiao = referencias$ufs$codigo_regiao[indice_uf],
    nome_regiao = referencias$ufs$nome_regiao[indice_uf],
    codigo_sinan_6 = municipio,
    codigo_ibge_7 = referencias$municipios$codigo_ibge_7[indice_municipio],
    nome_municipio = referencias$municipios$nome_municipio[indice_municipio],
    codigo_uf_municipio = referencias$municipios$codigo_uf[indice_municipio],
    id_unidade = unidade,
    nome_unidade = if (!possui_cnes) rep(NA_character_, length(unidade)) else {
      referencias$cnes$nome_unidade[indice_cnes]
    },
    problema_validade = por_registro$numero_problemas_validade > 0L,
    numero_problemas_validade = as.integer(por_registro$numero_problemas_validade),
    stringsAsFactors = FALSE
  )
  for (regra in regras) registros[[regra]] <- as.logical(por_registro[[regra]])
  registros$uf_localizada <- !is.na(indice_uf)
  registros$municipio_localizado <- !is.na(indice_municipio)
  registros$municipio_compativel_uf <- registros$municipio_localizado & registros$uf_localizada &
    registros$codigo_uf_municipio == registros$codigo_uf
  registros$cnes_localizado <- if (!possui_cnes) NA else !is.na(indice_cnes)
  registros
}

# Resume a qualidade dos relacionamentos para a seção metodológica do relatório.
resumir_relacionamentos_territoriais <- function(registros, referencias) {
  contar <- function(condicao) sum(condicao, na.rm = TRUE)
  possui_cnes <- referencia_cnes_disponivel(referencias)
  arquivo_cnes <- if (!is.null(referencias$arquivo_cnes)) basename(referencias$arquivo_cnes) else NULL
  data.frame(
    relacionamento = c(
      "UF da notificação localizada", "UF da notificação não localizada",
      "Município da notificação localizado", "Município da notificação não localizado",
      "Município compatível com a UF da notificação", "Município divergente da UF da notificação",
      if (possui_cnes) c(
        "CNES localizado", "CNES não localizado", "CNES localizado sem nome fantasia"
      ) else "Referência CNES"
    ),
    registros = c(
      contar(registros$uf_localizada), contar(!registros$uf_localizada),
      contar(registros$municipio_localizado), contar(!registros$municipio_localizado),
      contar(registros$municipio_compativel_uf),
      contar(registros$municipio_localizado & registros$uf_localizada & !registros$municipio_compativel_uf),
      if (possui_cnes) c(
        contar(registros$cnes_localizado), contar(!registros$cnes_localizado),
        contar(registros$cnes_localizado & is.na(registros$nome_unidade))
      ) else nrow(registros)
    ),
    observacao = c(
      rep("", 6L),
      if (possui_cnes) c("", "", "Código CNES exibido quando o nome não está informado") else if (!is.null(arquivo_cnes)) {
        sprintf("%s ainda não contém registros; códigos exibidos sem nomes", arquivo_cnes)
      } else {
        "cnes.xlsx/cnes.csv ausente; códigos exibidos sem nomes"
      }
    ),
    stringsAsFactors = FALSE
  )
}
