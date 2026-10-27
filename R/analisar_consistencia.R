# Motor da análise de consistência --------------------------------------------
#
# Cada avaliador implementa uma regra do catálogo e devolve um vetor lógico com
# um valor por notificação. O orquestrador só executa uma regra quando todas as
# variáveis declaradas em `regras_consistencia.R` estavam na base original.

# Localiza uma coluna sem diferenciar maiúsculas e minúsculas.
localizar_coluna_consistencia <- function(nomes, variavel) {
  which(tolower(nomes) == tolower(variavel))
}

# Separa dependências encontradas, ausentes e ambíguas para cada regra.
mapear_dependencias_consistencia <- function(base_padronizada, variaveis_originais,
                                              dependencias) {
  nomes_base <- names(base_padronizada)
  ausentes <- character()
  ambiguas <- character()
  mapeamento <- character()

  for (variavel in dependencias) {
    origem <- localizar_coluna_consistencia(variaveis_originais, variavel)
    if (!length(origem)) {
      ausentes <- c(ausentes, variavel)
      next
    }
    if (length(origem) > 1L) {
      ambiguas <- c(ambiguas, variavel)
      next
    }
    indice_base <- localizar_coluna_consistencia(nomes_base, variavel)
    if (length(indice_base) != 1L) {
      if (!length(indice_base)) ausentes <- c(ausentes, variavel) else ambiguas <- c(ambiguas, variavel)
      next
    }
    mapeamento[[variavel]] <- nomes_base[[indice_base]]
  }

  list(
    colunas = mapeamento,
    ausentes = unique(ausentes),
    ambiguas = unique(ambiguas)
  )
}

# Normaliza valores para comparações textuais e representa ausências por vazio.
texto_consistencia <- function(x) {
  resultado <- if (is.character(x)) x else as.character(x)
  if (anyNA(resultado)) resultado[is.na(resultado)] <- ""
  resultado
}

# Conversão numérica tolerante usada principalmente nos códigos compostos de idade.
numero_consistencia <- function(x) suppressWarnings(as.numeric(texto_consistencia(x)))

# Acessores locais evitam repetir a resolução do nome físico das colunas.
obter_valores_consistencia <- function(base, colunas) {
  function(variavel) texto_consistencia(base[[colunas[[variavel]]]])
}

# Cria um acessor equivalente para comparações que exigem valores numéricos.
obter_numeros_consistencia <- function(base, colunas) {
  function(variavel) numero_consistencia(base[[colunas[[variavel]]]])
}

# Operações vetoriais compartilhadas por regras com vários campos binários.
algum_igual_consistencia <- function(obter, variaveis, valor = "1") {
  Reduce(`|`, lapply(variaveis, function(variavel) obter(variavel) == valor))
}

# Confirma que nenhuma das variáveis informadas contém o valor procurado.
todos_diferentes_consistencia <- function(obter, variaveis, valor = "1") {
  Reduce(`&`, lapply(variaveis, function(variavel) obter(variavel) != valor))
}

# Regras 1 e 2 — datas de ocorrência/notificação iguais ao nascimento.
avaliar_regra_consistencia_1 <- function(base, colunas) {
  ocorrencia <- texto_consistencia(base[[colunas[["dt_ocor"]]]])
  nascimento <- texto_consistencia(base[[colunas[["dt_nasc"]]]])
  ocorrencia != "" & nascimento != "" & ocorrencia == nascimento
}

# Regra 2: data de notificação igual à data de nascimento.
avaliar_regra_consistencia_2 <- function(base, colunas) {
  notificacao <- texto_consistencia(base[[colunas[["dt_notific"]]]])
  nascimento <- texto_consistencia(base[[colunas[["dt_nasc"]]]])
  notificacao != "" & nascimento != "" & notificacao == nascimento
}

# Regras 3 a 10 — aplicabilidade de gestação, escolaridade, situação conjugal,
# ocupação, orientação sexual e identidade de gênero conforme sexo e idade.
avaliar_regra_consistencia_3 <- function(base, colunas) {
  obter <- obter_valores_consistencia(base, colunas)
  idade <- numero_consistencia(base[[colunas[["nu_idade_n"]]]])
  obter("cs_sexo") == "F" & !is.na(idade) & idade >= 4010 & idade <= 4059 &
    obter("cs_gestant") == "6"
}

# Regra 4: homem com gestação preenchida como condição aplicável.
avaliar_regra_consistencia_4 <- function(base, colunas) {
  obter <- obter_valores_consistencia(base, colunas)
  obter("cs_sexo") == "M" & !obter("cs_gestant") %in% c("", "6", "9")
}

# Regra 5: códigos de escolaridade, local ou motivação com menos de dois dígitos.
avaliar_regra_consistencia_5 <- function(base, colunas) {
  obter <- obter_valores_consistencia(base, colunas)
  variaveis <- regras_consistencia[["5"]]$variaveis
  Reduce(`|`, lapply(variaveis, function(variavel) {
    valor <- obter(variavel)
    valor != "" & nchar(valor, type = "chars") < 2L
  }))
}

# Regra 6: escolaridade incompatível com criança de até seis anos.
avaliar_regra_consistencia_6 <- function(base, colunas) {
  obter <- obter_valores_consistencia(base, colunas)
  idade <- numero_consistencia(base[[colunas[["nu_idade_n"]]]])
  !is.na(idade) & idade <= 4006 & !obter("cs_escol_n") %in% c("00", "10", "01", "", "09")
}

# Regra 7: situação conjugal aplicável em criança menor de dez anos.
avaliar_regra_consistencia_7 <- function(base, colunas) {
  obter <- obter_valores_consistencia(base, colunas)
  idade <- numero_consistencia(base[[colunas[["nu_idade_n"]]]])
  !is.na(idade) & idade < 4010 & !obter("sit_conjug") %in% c("8", "", "9")
}

# Regra 8: gestação preenchida fora da faixa etária aplicável para mulher.
avaliar_regra_consistencia_8 <- function(base, colunas) {
  obter <- obter_valores_consistencia(base, colunas)
  idade <- numero_consistencia(base[[colunas[["nu_idade_n"]]]])
  obter("cs_sexo") == "F" & !is.na(idade) &
    (idade <= 4009 | (idade > 4069 & idade != 9999)) &
    !obter("cs_gestant") %in% c("6", "", "9")
}

# Regra 9: ocupação informada para pessoa com menos de 14 anos.
avaliar_regra_consistencia_9 <- function(base, colunas) {
  obter <- obter_valores_consistencia(base, colunas)
  idade <- numero_consistencia(base[[colunas[["nu_idade_n"]]]])
  excluidas <- c("", "998999", "999991", "999994", "999995", "000000",
                "XXX", "9999.9", "100.01")
  !is.na(idade) & idade < 4014 & !obter("id_ocupa_n") %in% excluidas
}

# Regra 10: orientação sexual ou identidade de gênero aplicável até nove anos.
avaliar_regra_consistencia_10 <- function(base, colunas) {
  obter <- obter_valores_consistencia(base, colunas)
  idade <- numero_consistencia(base[[colunas[["nu_idade_n"]]]])
  aplicavel <- function(x) !x %in% c("8", "", "9")
  !is.na(idade) & idade <= 4009 &
    (aplicavel(obter("orient_sex")) | aplicavel(obter("ident_gen")))
}

# Regras 11 a 13 — coerência entre o indicador geral, tipos e especificação de
# deficiência ou transtorno.
avaliar_regra_consistencia_11 <- function(base, colunas) {
  obter <- obter_valores_consistencia(base, colunas)
  obter("def_espec") != "" & obter("def_out") != "1"
}

# Regra 12: deficiência negada, embora algum tipo ou especificação esteja presente.
avaliar_regra_consistencia_12 <- function(base, colunas) {
  obter <- obter_valores_consistencia(base, colunas)
  tipos <- c("def_fisica", "def_mental", "def_visual", "def_auditi",
             "def_out", "tran_ment", "tran_comp")
  obter("def_trans") == "2" &
    (algum_igual_consistencia(obter, tipos) | obter("def_espec") != "")
}

# Regra 13: deficiência confirmada sem indicação de nenhum tipo.
avaliar_regra_consistencia_13 <- function(base, colunas) {
  obter <- obter_valores_consistencia(base, colunas)
  tipos <- c("def_fisica", "def_mental", "def_visual", "def_auditi",
             "tran_ment", "tran_comp", "def_out")
  # A descrição da regra exige DEF_TRANS = 1 e ausência de qualquer tipo
  # confirmado. Os parênteses do .do não expressam essa conjunção corretamente.
  nenhum_confirmado <- Reduce(`&`, lapply(tipos, function(variavel) {
    obter(variavel) %in% c("2", "9", "")
  }))
  obter("def_trans") == "1" & nenhum_confirmado
}

# Regras 15 a 22 — relações lógicas entre lesão autoprovocada, autoria, idade,
# meios de agressão, número de envolvidos, sexo e ciclo de vida.
avaliar_regra_consistencia_15 <- function(base, colunas) {
  obter <- obter_valores_consistencia(base, colunas)
  obter("les_autop") == "1" & obter("rel_propri") != "1"
}

# Regra 16: autoria própria informada sem lesão autoprovocada.
avaliar_regra_consistencia_16 <- function(base, colunas) {
  obter <- obter_valores_consistencia(base, colunas)
  obter("les_autop") != "1" & obter("rel_propri") == "1"
}

# Regra 17: lesão autoprovocada em criança menor de um ano.
avaliar_regra_consistencia_17 <- function(base, colunas) {
  obter <- obter_valores_consistencia(base, colunas)
  idade <- numero_consistencia(base[[colunas[["nu_idade_n"]]]])
  obter("les_autop") == "1" & !is.na(idade) & idade < 4001
}

# Regra 18: lesão autoprovocada junto a uma natureza incompatível.
avaliar_regra_consistencia_18 <- function(base, colunas) {
  obter <- obter_valores_consistencia(base, colunas)
  tipos <- c("viol_psico", "viol_tort", "viol_sexu", "viol_traf",
             "viol_finan", "viol_infan", "viol_legal")
  obter("les_autop") == "1" & algum_igual_consistencia(obter, tipos)
}

# Regra 19: ameaça indicada como meio de agressão em lesão autoprovocada.
avaliar_regra_consistencia_19 <- function(base, colunas) {
  obter <- obter_valores_consistencia(base, colunas)
  obter("les_autop") == "1" & obter("ag_ameaca") == "1"
}

# Regra 20: quantidade de envolvidos incompatível com lesão autoprovocada.
avaliar_regra_consistencia_20 <- function(base, colunas) {
  obter <- obter_valores_consistencia(base, colunas)
  obter("les_autop") == "1" & !obter("num_envolv") %in% c("1", "", "9")
}

# Regra 21: sexo do autor incompatível quando a autoria é da própria vítima.
avaliar_regra_consistencia_21 <- function(base, colunas) {
  obter <- obter_valores_consistencia(base, colunas)
  autor <- obter("autor_sexo")
  vitima <- obter("cs_sexo")
  obter("les_autop") == "1" & obter("rel_propri") == "1" &
    !autor %in% c("9", "") &
    ((vitima == "M" & autor != "1") | (vitima == "F" & autor != "2"))
}

# Regra 22: ciclo de vida do autor incompatível com a idade da própria vítima.
avaliar_regra_consistencia_22 <- function(base, colunas) {
  obter <- obter_valores_consistencia(base, colunas)
  idade <- numero_consistencia(base[[colunas[["nu_idade_n"]]]])
  ciclo <- obter("cicl_vid")
  faixa_incompativel <- !is.na(idade) & (
    (idade <= 4009 & ciclo != "1") |
      (idade >= 4010 & idade <= 4019 & ciclo != "2") |
      (idade >= 4020 & idade <= 4024 & ciclo != "3") |
      (idade >= 4025 & idade <= 4059 & ciclo != "4") |
      (idade >= 4060 & idade != 9999 & ciclo != "5")
  )
  obter("les_autop") == "1" & obter("rel_propri") == "1" &
    faixa_incompativel & !ciclo %in% c("9", "")
}

# Regras 23 a 27B — perfil e vínculo do provável autor e trabalho infantil.
avaliar_regra_consistencia_23 <- function(base, colunas) {
  obter <- obter_valores_consistencia(base, colunas)
  obter("viol_legal") == "1" & obter("rel_pol") != "1"
}

# Regra 24: trabalho infantil assinalado para pessoa maior de 17 anos.
avaliar_regra_consistencia_24 <- function(base, colunas) {
  obter <- obter_valores_consistencia(base, colunas)
  idade <- numero_consistencia(base[[colunas[["nu_idade_n"]]]])
  obter("viol_infan") == "1" & !is.na(idade) & idade > 4017 & idade != 9999
}

# Regra 25: mãe/madrasta identificada como provável autor do sexo masculino.
avaliar_regra_consistencia_25 <- function(base, colunas) {
  obter <- obter_valores_consistencia(base, colunas)
  (obter("rel_mae") == "1" | obter("rel_mad") == "1") & obter("autor_sexo") == "1"
}

# Regra 26: pai/padrasto identificado como provável autor do sexo feminino.
avaliar_regra_consistencia_26 <- function(base, colunas) {
  obter <- obter_valores_consistencia(base, colunas)
  (obter("rel_pai") == "1" | obter("rel_pad") == "1") & obter("autor_sexo") == "2"
}

# Regra 27A: vínculo conjugal ou de filho indicado para menor de 16 anos.
avaliar_regra_consistencia_27a <- function(base, colunas) {
  obter <- obter_valores_consistencia(base, colunas)
  idade <- numero_consistencia(base[[colunas[["nu_idade_n"]]]])
  # O .do anexado omite o parêntese final; a condição textual é inequívoca.
  !is.na(idade) & idade < 4016 &
    algum_igual_consistencia(obter, c("rel_conj", "rel_excon", "rel_filho"))
}

# Regra 27B: patrão/chefe indicado para menor de 14 sem trabalho infantil.
avaliar_regra_consistencia_27b <- function(base, colunas) {
  obter <- obter_valores_consistencia(base, colunas)
  idade <- numero_consistencia(base[[colunas[["nu_idade_n"]]]])
  !is.na(idade) & idade < 4014 & obter("rel_patrao") == "1" &
    obter("viol_infan") != "1"
}

# Regras 28 a 30 — coerência entre violência sexual, seus subtipos e idade.
avaliar_regra_consistencia_28 <- function(base, colunas) {
  obter <- obter_valores_consistencia(base, colunas)
  tipos <- c("sex_assedi", "sex_estupr", "sex_pudor", "sex_porno",
             "sex_explo", "sex_outro")
  obter("viol_sexu") == "1" & todos_diferentes_consistencia(obter, tipos)
}

# Regra 29: pornografia infantil registrada em pessoa maior de 19 anos.
avaliar_regra_consistencia_29 <- function(base, colunas) {
  obter <- obter_valores_consistencia(base, colunas)
  idade <- numero_consistencia(base[[colunas[["nu_idade_n"]]]])
  obter("sex_porno") == "1" & !is.na(idade) & idade > 4019 & idade != 9999
}

# Regra 30: subtipo sexual marcado sem confirmação de violência sexual.
avaliar_regra_consistencia_30 <- function(base, colunas) {
  obter <- obter_valores_consistencia(base, colunas)
  tipos <- c("sex_assedi", "sex_estupr", "sex_pudor", "sex_porno",
             "sex_explo", "sex_outro")
  obter("viol_sexu") != "1" & algum_igual_consistencia(obter, tipos)
}

# Regras 31 a 33 — compatibilidade de encaminhamentos, idade, sexo e trabalho.
avaliar_regra_consistencia_31 <- function(base, colunas) {
  obter <- obter_valores_consistencia(base, colunas)
  obter("cs_sexo") == "M" &
    (obter("atend_mulh") == "1" | obter("deleg_mulh") == "1")
}

# Regra 32: adulto encaminhado a órgão da infância e juventude.
avaliar_regra_consistencia_32 <- function(base, colunas) {
  obter <- obter_valores_consistencia(base, colunas)
  idade <- numero_consistencia(base[[colunas[["nu_idade_n"]]]])
  !is.na(idade) & idade > 4019 & idade != 9999 &
    (obter("infan_juv") == "1" | obter("cons_tutel") == "1")
}

# Regra 33: CAT emitida em caso não relacionado ao trabalho.
avaliar_regra_consistencia_33 <- function(base, colunas) {
  obter <- obter_valores_consistencia(base, colunas)
  obter("rel_cat") == "1" & obter("rel_trab") != "1"
}

# Regras 34 e 35 — UF e município devem estar ambos preenchidos ou ambos vazios.
avaliar_regra_consistencia_34 <- function(base, colunas) {
  obter <- obter_valores_consistencia(base, colunas)
  xor(obter("sg_uf_ocor") != "", obter("id_mn_ocor") != "")
}

# Regra 35: presença de UF e município de residência deve ser coerente.
avaliar_regra_consistencia_35 <- function(base, colunas) {
  obter <- obter_valores_consistencia(base, colunas)
  xor(obter("sg_uf") != "", obter("id_mn_resi") != "")
}

# Registro central que liga cada código de regra ao respectivo avaliador.
funcoes_regras_consistencia <- list(
  `1` = avaliar_regra_consistencia_1,
  `2` = avaliar_regra_consistencia_2,
  `3` = avaliar_regra_consistencia_3,
  `4` = avaliar_regra_consistencia_4,
  `5` = avaliar_regra_consistencia_5,
  `6` = avaliar_regra_consistencia_6,
  `7` = avaliar_regra_consistencia_7,
  `8` = avaliar_regra_consistencia_8,
  `9` = avaliar_regra_consistencia_9,
  `10` = avaliar_regra_consistencia_10,
  `11` = avaliar_regra_consistencia_11,
  `12` = avaliar_regra_consistencia_12,
  `13` = avaliar_regra_consistencia_13,
  `15` = avaliar_regra_consistencia_15,
  `16` = avaliar_regra_consistencia_16,
  `17` = avaliar_regra_consistencia_17,
  `18` = avaliar_regra_consistencia_18,
  `19` = avaliar_regra_consistencia_19,
  `20` = avaliar_regra_consistencia_20,
  `21` = avaliar_regra_consistencia_21,
  `22` = avaliar_regra_consistencia_22,
  `23` = avaliar_regra_consistencia_23,
  `24` = avaliar_regra_consistencia_24,
  `25` = avaliar_regra_consistencia_25,
  `26` = avaliar_regra_consistencia_26,
  `27A` = avaliar_regra_consistencia_27a,
  `27B` = avaliar_regra_consistencia_27b,
  `28` = avaliar_regra_consistencia_28,
  `29` = avaliar_regra_consistencia_29,
  `30` = avaliar_regra_consistencia_30,
  `31` = avaliar_regra_consistencia_31,
  `32` = avaliar_regra_consistencia_32,
  `33` = avaliar_regra_consistencia_33,
  `34` = avaliar_regra_consistencia_34,
  `35` = avaliar_regra_consistencia_35
)

# Agrega regras nos oito grupos temáticos. Um registro conta uma única vez no
# grupo, ainda que tenha sido sinalizado por várias regras daquele grupo.
resumir_grupos_consistencia <- function(indicadores, tabela_regras) {
  linhas <- lapply(seq_len(nrow(grupos_consistencia)), function(i) {
    grupo_id_atual <- grupos_consistencia$grupo_id[[i]]
    regras_grupo <- tabela_regras[
      tabela_regras$grupo_id == grupo_id_atual, , drop = FALSE
    ]
    indicadores_grupo <- regras_grupo$indicador[regras_grupo$status == "Executada"]
    status <- if (!length(indicadores_grupo)) {
      "Não executável"
    } else if (all(regras_grupo$status == "Executada")) {
      "Executado"
    } else {
      "Parcial"
    }
    if (!length(indicadores_grupo)) {
      sinalizados <- NA_integer_
      ocorrencias <- NA_integer_
    } else {
      contagem <- integer(nrow(indicadores))
      for (indicador in indicadores_grupo) {
        valores <- indicadores[[indicador]]
        contagem <- contagem + as.integer(!is.na(valores) & valores)
      }
      sinalizados <- sum(contagem > 0L)
      ocorrencias <- sum(contagem)
    }
    total <- nrow(indicadores)
    data.frame(
      grupo_id = grupo_id_atual,
      grupo = grupos_consistencia$grupo[[i]],
      status = status,
      registros_sinalizados = sinalizados,
      percentual_total = if (is.na(sinalizados)) NA_real_ else if (total) 100 * sinalizados / total else 0,
      ocorrencias = ocorrencias,
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, linhas)
}

# Orquestra requisitos, execução e consolidação em níveis de registro, regra e
# grupo. Também preserva avisos e mapeamentos para diagnóstico na interface.
executar_analise_consistencia <- function(base_padronizada, variaveis_originais) {
  if (!is.data.frame(base_padronizada)) {
    stop("A base padronizada não está disponível como data.frame.", call. = FALSE)
  }
  if (is.null(variaveis_originais)) {
    stop("A relação de variáveis existentes na base original não está disponível.", call. = FALSE)
  }

  numero_registros <- nrow(base_padronizada)
  indicadores <- data.frame(id_registro = seq_len(numero_registros))
  resumo_regras <- list()
  mapeamentos <- list()
  avisos <- character()
  metadados <- obter_metadados_regras_consistencia()
  numero_problemas <- integer(numero_registros)
  indice_regra <- 0L

  # Uma regra impedida recebe NA em todos os registros; as demais continuam.
  for (id in names(regras_consistencia)) {
    indice_regra <- indice_regra + 1L
    regra <- regras_consistencia[[id]]
    mapa <- mapear_dependencias_consistencia(
      base_padronizada, variaveis_originais, regra$variaveis
    )
    mapeamentos[[id]] <- mapa$colunas
    impedimentos <- c(
      if (length(mapa$ausentes)) paste0("Ausentes na base original: ", paste(mapa$ausentes, collapse = ", ")),
      if (length(mapa$ambiguas)) paste0("Nomes ambíguos: ", paste(mapa$ambiguas, collapse = ", "))
    )

    if (length(impedimentos)) {
      indicador <- rep(NA, numero_registros)
      status <- "Não executável"
      detalhe <- paste(impedimentos, collapse = "; ")
      avisos <- c(avisos, sprintf("Regra %s não executável — %s.", id, detalhe))
    } else {
      indicador <- funcoes_regras_consistencia[[id]](base_padronizada, mapa$colunas)
      indicador[is.na(indicador)] <- FALSE
      status <- "Executada"
      detalhe <- ""
    }

    indicadores[[regra$indicador]] <- indicador
    if (identical(status, "Executada")) {
      numero_problemas <- numero_problemas + as.integer(indicador)
    }
    sinalizados <- if (all(is.na(indicador))) NA_integer_ else sum(indicador, na.rm = TRUE)
    percentual <- if (is.na(sinalizados) || numero_registros == 0L) {
      if (is.na(sinalizados)) NA_real_ else 0
    } else {
      100 * sinalizados / numero_registros
    }
    meta <- metadados[metadados$regra == regra$id, , drop = FALSE]
    resumo_regras[[id]] <- data.frame(
      regra = regra$id,
      indicador = regra$indicador,
      descricao = regra$descricao,
      grupo_id = regra$grupo_id,
      grupo = meta$grupo,
      status = status,
      detalhe = detalhe,
      registros_sinalizados = sinalizados,
      percentual_total = percentual,
      stringsAsFactors = FALSE
    )
    rm(indicador, mapa, regra)
    if (indice_regra %% 5L == 0L) invisible(gc(verbose = FALSE, full = FALSE))
  }

  indicadores$numero_problemas_consistencia <- as.integer(numero_problemas)
  indicadores$problema_consistencia <- numero_problemas > 0L

  tabela_regras <- do.call(rbind, resumo_regras)
  rownames(tabela_regras) <- NULL
  tabela_grupos <- resumir_grupos_consistencia(indicadores, tabela_regras)
  registros_problematicos <- sum(indicadores$problema_consistencia)
  ocorrencias <- sum(indicadores$numero_problemas_consistencia)

  list(
    por_registro = indicadores,
    por_regra = tabela_regras,
    por_grupo = tabela_grupos,
    resumo = list(
      notificacoes = numero_registros,
      registros_problematicos = registros_problematicos,
      percentual_problematicos = if (numero_registros) 100 * registros_problematicos / numero_registros else 0,
      ocorrencias = ocorrencias
    ),
    avisos = unique(avisos),
    mapeamentos = mapeamentos
  )
}

# Prepara a tabela detalhada para uma regra selecionada ou para todos os casos.
obter_registros_consistencia <- function(base_padronizada, resultado, selecao = "todos") {
  por_registro <- resultado$por_registro
  if (identical(selecao, "todos")) {
    filtro <- por_registro$problema_consistencia
    variaveis <- c("nu_notific", "dt_notific", "nm_pacient", "cs_sexo", "nu_idade_n")
    # Evita duplicar dezenas de vetores lógicos na tabela geral. Ao escolher uma
    # regra, seu indicador e suas variáveis específicas continuam sendo exibidos.
    indicadores_mostrar <- character()
  } else {
    regra <- regras_consistencia[[as.character(selecao)]]
    if (is.null(regra)) stop("Regra de consistência desconhecida.", call. = FALSE)
    indicador <- por_registro[[regra$indicador]]
    filtro <- !is.na(indicador) & indicador
    variaveis <- unique(c("nu_notific", "dt_notific", regra$variaveis))
    indicadores_mostrar <- regra$indicador
  }

  indices <- which(filtro)
  saida <- data.frame(id_registro = por_registro$id_registro[indices])
  for (variavel in variaveis) {
    coluna <- localizar_coluna_consistencia(names(base_padronizada), variavel)
    if (length(coluna) == 1L) saida[[variavel]] <- base_padronizada[[coluna]][indices]
  }
  for (indicador in indicadores_mostrar) {
    saida[[indicador]] <- por_registro[[indicador]][indices]
  }
  saida$numero_problemas_consistencia <- por_registro$numero_problemas_consistencia[indices]
  saida
}
