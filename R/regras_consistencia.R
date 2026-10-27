# Catálogo das regras de consistência -----------------------------------------
#
# Este arquivo centraliza a documentação estrutural das regras. A avaliação de
# cada condição está em `analisar_consistencia.R`. Manter descrição, variáveis e
# grupo aqui evita divergências entre processamento, interface e relatório.

# Grupos temáticos usados para agregar resultados e organizar seções expansíveis.
grupos_consistencia <- data.frame(
  grupo_id = 1:8,
  grupo = c(
    "Integridade cadastral, temporal e territorial",
    "Aplicabilidade sociodemográfica",
    "Trabalho, ocupação e trabalho infantil",
    "Deficiência e transtornos",
    "Lesão autoprovocada",
    "Perfil e vínculo do provável autor",
    "Violência sexual",
    "Encaminhamentos à rede de proteção"
  ),
  stringsAsFactors = FALSE
)

# Construtor padronizado de uma regra; gera também o nome da coluna indicadora.
nova_regra_consistencia <- function(id, descricao, variaveis, grupo_id) {
  list(
    id = as.character(id),
    indicador = paste0("regra_", tolower(id)),
    descricao = descricao,
    variaveis = variaveis,
    grupo_id = as.integer(grupo_id)
  )
}

# Regras individuais e respectivos requisitos mínimos de variáveis.
regras_consistencia <- list(
  `1` = nova_regra_consistencia(
    "1", "Data de ocorrência igual à data de nascimento",
    c("dt_ocor", "dt_nasc"), 1
  ),
  `2` = nova_regra_consistencia(
    "2", "Data de notificação igual à data de nascimento",
    c("dt_notific", "dt_nasc"), 1
  ),
  `3` = nova_regra_consistencia(
    "3", "Mulher de 10 a 59 anos com gestação marcada como não se aplica",
    c("cs_sexo", "nu_idade_n", "cs_gestant"), 2
  ),
  `4` = nova_regra_consistencia(
    "4", "Homem com campo de gestação diferente de não se aplica",
    c("cs_sexo", "cs_gestant"), 2
  ),
  `5` = nova_regra_consistencia(
    "5", "Escolaridade, local de ocorrência ou motivação com menos de dois dígitos",
    c("cs_escol_n", "local_ocor", "viol_motiv"), 1
  ),
  `6` = nova_regra_consistencia(
    "6", "Criança de até 6 anos com escolaridade incompatível",
    c("nu_idade_n", "cs_escol_n"), 2
  ),
  `7` = nova_regra_consistencia(
    "7", "Criança menor de 10 anos com situação conjugal aplicável",
    c("nu_idade_n", "sit_conjug"), 2
  ),
  `8` = nova_regra_consistencia(
    "8", "Mulher de até 9 anos ou maior de 69 anos com dados de gestação",
    c("cs_sexo", "nu_idade_n", "cs_gestant"), 2
  ),
  `9` = nova_regra_consistencia(
    "9", "Ocupação informada para menor de 14 anos",
    c("nu_idade_n", "id_ocupa_n"), 3
  ),
  `10` = nova_regra_consistencia(
    "10", "Criança de até 9 anos com orientação sexual ou identidade de gênero aplicável",
    c("nu_idade_n", "orient_sex", "ident_gen"), 2
  ),
  `11` = nova_regra_consistencia(
    "11", "Outra deficiência especificada sem indicação de outra deficiência",
    c("def_espec", "def_out"), 4
  ),
  `12` = nova_regra_consistencia(
    "12", "Deficiência/transtorno marcada como não, mas algum tipo foi informado",
    c("def_trans", "def_fisica", "def_mental", "def_visual", "def_auditi",
      "def_out", "tran_ment", "tran_comp", "def_espec"), 4
  ),
  `13` = nova_regra_consistencia(
    "13", "Deficiência/transtorno marcada como sim sem nenhum tipo confirmado",
    c("def_trans", "def_fisica", "def_mental", "def_visual", "def_auditi",
      "tran_ment", "tran_comp", "def_out"), 4
  ),
  `15` = nova_regra_consistencia(
    "15", "Lesão autoprovocada com autor diferente da própria pessoa",
    c("les_autop", "rel_propri"), 5
  ),
  `16` = nova_regra_consistencia(
    "16", "Lesão não marcada como autoprovocada com autor igual à própria pessoa",
    c("les_autop", "rel_propri"), 5
  ),
  `17` = nova_regra_consistencia(
    "17", "Lesão autoprovocada em menor de 1 ano",
    c("les_autop", "nu_idade_n"), 5
  ),
  `18` = nova_regra_consistencia(
    "18", "Lesão autoprovocada incompatível com o tipo de violência",
    c("les_autop", "viol_psico", "viol_tort", "viol_sexu", "viol_traf",
      "viol_finan", "viol_infan", "viol_legal"), 5
  ),
  `19` = nova_regra_consistencia(
    "19", "Lesão autoprovocada com ameaça como meio de agressão",
    c("les_autop", "ag_ameaca"), 5
  ),
  `20` = nova_regra_consistencia(
    "20", "Lesão autoprovocada com número de envolvidos diferente de um",
    c("les_autop", "num_envolv"), 5
  ),
  `21` = nova_regra_consistencia(
    "21", "Lesão autoprovocada com sexo do autor diferente do sexo da vítima",
    c("les_autop", "rel_propri", "autor_sexo", "cs_sexo"), 5
  ),
  `22` = nova_regra_consistencia(
    "22", "Lesão autoprovocada com ciclo de vida do autor incompatível com a idade da vítima",
    c("les_autop", "rel_propri", "nu_idade_n", "cicl_vid"), 5
  ),
  `23` = nova_regra_consistencia(
    "23", "Intervenção legal com provável autor diferente de policial/agente da lei",
    c("viol_legal", "rel_pol"), 6
  ),
  `24` = nova_regra_consistencia(
    "24", "Trabalho infantil em pessoa maior de 17 anos",
    c("viol_infan", "nu_idade_n"), 3
  ),
  `25` = nova_regra_consistencia(
    "25", "Mãe ou madrasta indicada como provável autora do sexo masculino",
    c("rel_mae", "rel_mad", "autor_sexo"), 6
  ),
  `26` = nova_regra_consistencia(
    "26", "Pai ou padrasto indicado como provável autor do sexo feminino",
    c("rel_pai", "rel_pad", "autor_sexo"), 6
  ),
  `27A` = nova_regra_consistencia(
    "27A", "Menor de 16 anos com agressor cônjuge, ex-cônjuge ou filho",
    c("nu_idade_n", "rel_conj", "rel_excon", "rel_filho"), 6
  ),
  `27B` = nova_regra_consistencia(
    "27B", "Menor de 14 anos com patrão/chefe como agressor sem trabalho infantil",
    c("nu_idade_n", "rel_patrao", "viol_infan"), 3
  ),
  `28` = nova_regra_consistencia(
    "28", "Violência sexual sem nenhum subtipo marcado como sim",
    c("viol_sexu", "sex_assedi", "sex_estupr", "sex_pudor", "sex_porno",
      "sex_explo", "sex_outro"), 7
  ),
  `29` = nova_regra_consistencia(
    "29", "Pornografia infantil em pessoa maior de 19 anos",
    c("sex_porno", "nu_idade_n"), 7
  ),
  `30` = nova_regra_consistencia(
    "30", "Subtipo de violência sexual informado sem violência sexual marcada como sim",
    c("viol_sexu", "sex_assedi", "sex_estupr", "sex_pudor", "sex_porno",
      "sex_explo", "sex_outro"), 7
  ),
  `31` = nova_regra_consistencia(
    "31", "Homem encaminhado à delegacia ou centro de referência da mulher",
    c("cs_sexo", "atend_mulh", "deleg_mulh"), 8
  ),
  `32` = nova_regra_consistencia(
    "32", "Adulto encaminhado ao Conselho Tutelar ou à Vara da Infância/Juventude",
    c("nu_idade_n", "infan_juv", "cons_tutel"), 8
  ),
  `33` = nova_regra_consistencia(
    "33", "CAT emitida em caso não marcado como relacionado ao trabalho",
    c("rel_cat", "rel_trab"), 3
  ),
  `34` = nova_regra_consistencia(
    "34", "Município e UF de ocorrência com preenchimento incompatível",
    c("sg_uf_ocor", "id_mn_ocor"), 1
  ),
  `35` = nova_regra_consistencia(
    "35", "Município e UF de residência com preenchimento incompatível",
    c("sg_uf", "id_mn_resi"), 1
  )
)

# Converte o catálogo em tabela e associa a descrição do grupo temático.
obter_metadados_regras_consistencia <- function() {
  linhas <- lapply(regras_consistencia, function(regra) {
    data.frame(
      regra = regra$id,
      indicador = regra$indicador,
      descricao = regra$descricao,
      grupo_id = regra$grupo_id,
      stringsAsFactors = FALSE
    )
  })
  resultado <- do.call(rbind, linhas)
  resultado$grupo <- grupos_consistencia$grupo[
    match(resultado$grupo_id, grupos_consistencia$grupo_id)
  ]
  rownames(resultado) <- NULL
  resultado
}
