# Catálogo das regras de validade ---------------------------------------------
#
# Cada entrada informa o identificador exibido no relatório, a coluna indicadora
# gerada na análise, a descrição legível e as variáveis mínimas para execução.
# A lógica que marca cada registro está em `analisar_validade.R`.

regras_validade <- list(
  `1` = list(
    id = 1L,
    indicador = "regra_1",
    descricao = "Nenhuma natureza de violência preenchida com código 1",
    variaveis = c(
      "les_autop", "viol_fisic", "viol_psico", "viol_tort", "viol_sexu",
      "viol_traf", "viol_finan", "viol_negli", "viol_infan", "viol_legal",
      "viol_outr"
    )
  ),
  `3` = list(
    id = 3L,
    indicador = "regra_3",
    descricao = "Nome do paciente inicia por FETO, ABORTO, NATIMORT ou NASCITUR",
    variaveis = "nm_pacient"
  ),
  `4` = list(
    id = 4L,
    indicador = "regra_4",
    descricao = "Homem de 20 a 59 anos fora da definição de caso especificada",
    variaveis = c(
      "cs_sexo", "nu_idade_n", "cs_raca", "orient_sex", "ident_gen",
      "def_trans", "def_fisica", "def_mental", "def_visual", "def_auditi",
      "tran_ment", "tran_comp", "def_out", "les_autop", "viol_sexu",
      "viol_tort", "viol_traf", "viol_legal", "rel_pai", "rel_mae",
      "rel_pad", "rel_mad", "rel_conj", "rel_excon", "rel_namo",
      "rel_exnam", "rel_filho", "rel_irmao", "rel_cuida", "rel_pol",
      "rel_propri"
    )
  )
)

# Transforma o catálogo em tabela para menus, indicadores e relatório.
obter_metadados_regras_validade <- function() {
  do.call(rbind, lapply(regras_validade, function(regra) {
    data.frame(
      regra = regra$id,
      indicador = regra$indicador,
      descricao = regra$descricao,
      stringsAsFactors = FALSE
    )
  }))
}
