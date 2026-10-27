# Especificação da padronização ------------------------------------------------
#
# Este arquivo é o dicionário de dados da base VIVA/SINAN. Ele não transforma
# registros diretamente: declara larguras, campos de data, domínios válidos e
# conjuntos auxiliares consumidos por `padronizar_base.R`. Para manutenção, uma
# variável nova deve ser incluída na estrutura correspondente ao seu tipo.

# Relação entre a largura final, em caracteres, e os campos submetidos a ela.
larguras_campos <- list(
  `1` = c(
    "tp_uni_ext", "tp_not", "cs_sexo", "cs_gestant", "cs_raca", "nduplic",
    "sit_conjug", "orient_sex", "ident_gen", "def_trans", "def_fisica",
    "def_mental", "def_visual", "def_auditi", "tran_ment", "tran_comp",
    "def_out", "zona", "zona_ocor", "out_vezes", "les_autop", "viol_fisic",
    "viol_psico", "viol_tort", "viol_sexu", "viol_traf", "viol_finan",
    "viol_negli", "viol_infan", "viol_legal", "viol_outr", "ag_forca",
    "ag_enfor", "ag_objeto", "ag_corte", "ag_quente", "ag_enven", "ag_fogo",
    "ag_ameaca", "ag_outros", "sex_assedi", "sex_estupr", "sex_pudor",
    "sex_porno", "sex_explo", "sex_outro", "pen_oral", "pen_anal",
    "pen_vagina", "proc_dst", "proc_hiv", "proc_hepb", "proc_sang",
    "proc_semen", "proc_vagin", "proc_contr", "proc_abort", "cons_abort",
    "cons_grav", "cons_dst", "cons_suic", "cons_ment", "cons_comp",
    "cons_estre", "cons_outr", "num_envolv", "rel_sexual", "rel_pai",
    "rel_mae", "rel_pad", "rel_mad", "rel_conj", "rel_excon", "rel_namo",
    "rel_exnam", "rel_filho", "rel_desco", "rel_irmao", "rel_conhec",
    "rel_cuida", "rel_patrao", "rel_inst", "rel_pol", "rel_propri",
    "rel_outros", "autor_sexo", "autor_alco", "cicl_vid", "enc_saude",
    "enc_tutela", "enc_vara", "enc_abrigo", "enc_sentin", "enc_deam",
    "enc_dpca", "enc_deleg", "enc_mpu", "enc_mulher", "enc_creas",
    "enc_iml", "enc_outr", "rede_sau", "assist_soc", "rede_educa",
    "atend_mulh", "cons_tutel", "cons_ido", "deleg_idos", "dir_human",
    "mpu", "deleg_cria", "deleg_mulh", "deleg", "infan_juv", "defen_publ",
    "rel_trab", "rel_cat", "classi_fin", "evolucao", "ident_micr", "tpuninot"
  ),
  `2` = c("cs_escol_n", "ddd", "sg_uf_not", "sg_uf", "sg_uf_ocor",
          "local_ocor", "lesao_nat", "lesao_corp", "viol_motiv"),
  `4` = c("id_agravo", "id_pais", "nu_ano", "id_regiona", "id_rg_resi"),
  `5` = c("hora_ocor", "circ_lesao"),
  `6` = c("sem_not", "sem_pri", "id_ocupa_n", "id_municip", "id_mn_resi",
          "nu_numero", "id_mn_ocor", "nm_log_res"),
  `7` = c("nu_notific", "id_unidade", "nu_lote_h", "nu_lote_v"),
  `8` = c("id_geo1", "id_geo2", "id_geo3", "id_geo4", "id_dis_oco",
          "id_ba_ocor", "id_log_oco", "id_bairro", "id_logrado", "nu_cep"),
  `9` = c("fone", "id_distrit"),
  `15` = c("nm_uni_ext", "co_uni_ext", "id_cns_sus"),
  `30` = c("ds_compl", "def_espec", "ds_comp_oc", "local_espe", "viol_espec",
           "ag_espec", "sex_espec", "cons_espec", "lesao_espe", "rel_espec",
           "enc_espec"),
  `50` = "no_log_oco",
  `60` = c("nm_bairro", "nm_logrado", "nm_ba_ocor"),
  `70` = c("nm_mae_pac", "ds_ref_res", "nm_pacient", "ds_ref_oco"),
  `254` = "ds_obs"
)

# Campos interpretados como datas no padrão utilizado pelo SINAN.
variaveis_data <- c(
  "dt_notific", "dt_ocor", "dt_nasc", "dt_obito", "dt_encerra", "dt_invest",
  "dt_digita", "dt_transus", "dt_transdm", "dt_transsm", "dt_transrm",
  "dt_transrs", "dt_transse"
)

# Campos textuais livres que passam por limpeza adicional de caracteres.
campos_abertos <- c(
  "nm_pacient", "nm_mae_pac", "def_espec", "local_espe", "viol_espec",
  "ag_espec", "sex_espec", "rel_espec", "ds_obs"
)

# Domínios explícitos de variáveis categóricas que não compartilham um padrão.
dominios_categoricos <- list(
  cs_sexo = c("F", "M", "I"),
  cs_raca = c("1", "2", "3", "4", "5", "9"),
  sit_conjug = c("1", "2", "3", "4", "8", "9"),
  cs_gestant = c("1", "2", "3", "4", "5", "6", "9"),
  orient_sex = c("1", "2", "3", "8", "9"),
  ident_gen = c("1", "2", "3", "8", "9"),
  zona = c("1", "2", "3", "9"),
  zona_ocor = c("1", "2", "3", "9"),
  autor_sexo = c("1", "2", "3", "9"),
  cicl_vid = c("1", "2", "3", "4", "5", "9")
)

# Campos binários/categóricos cujo domínio permitido é 1, 2 ou 9.
grupo_dominio_129 <- c(
  "def_trans", "out_vezes", "les_autop", "viol_fisic", "viol_psico",
  "viol_tort", "viol_sexu", "viol_traf", "viol_finan", "viol_negli",
  "viol_infan", "viol_legal", "viol_outr", "ag_forca", "ag_enfor",
  "ag_objeto", "ag_corte", "ag_quente", "ag_enven", "ag_fogo", "ag_ameaca",
  "ag_outros", "rel_pai", "rel_mae", "rel_pad", "rel_mad", "rel_conj",
  "rel_excon", "rel_namo", "rel_exnam", "rel_filho", "rel_desco",
  "rel_irmao", "rel_conhec", "rel_cuida", "rel_patrao", "rel_inst",
  "rel_pol", "rel_propri", "rel_outros", "rede_sau", "assist_soc",
  "rede_educa", "atend_mulh", "cons_tutel", "cons_ido", "deleg_idos",
  "dir_human", "mpu", "deleg_cria", "deleg_mulh", "deleg", "infan_juv",
  "defen_publ", "num_envolv", "autor_alco", "rel_trab"
)

# Campos categóricos cujo domínio permitido é 1, 2, 8 ou 9.
grupo_dominio_1289 <- c(
  "def_fisica", "def_mental", "def_visual", "def_auditi", "tran_ment",
  "tran_comp", "def_out", "sex_assedi", "sex_estupr", "sex_pudor",
  "sex_porno", "sex_explo", "sex_outro", "proc_dst", "proc_hiv",
  "proc_hepb", "proc_sang", "proc_semen", "proc_vagin", "proc_contr",
  "proc_abort", "rel_cat"
)

# Incorpora os dois grupos acima ao dicionário geral de domínios.
dominios_categoricos <- c(
  dominios_categoricos,
  stats::setNames(rep(list(c("1", "2", "9")), length(grupo_dominio_129)),
                  grupo_dominio_129),
  stats::setNames(rep(list(c("1", "2", "8", "9")), length(grupo_dominio_1289)),
                  grupo_dominio_1289)
)

# Domínios numéricos que devem ser armazenados com dois caracteres.
dominios_formatados <- list(
  local_ocor = c(1L:9L, 99L),
  cs_escol_n = 0L:10L,
  viol_motiv = c(1L:9L, 88L, 99L)
)

# Códigos de município que recebem tratamento territorial específico.
variaveis_municipio <- c("id_mn_resi", "id_mn_ocor")

# Consolida, sem duplicações, todas as variáveis conhecidas pela padronização.
obter_variaveis_esperadas <- function() {
  unique(c(
    unlist(larguras_campos, use.names = FALSE),
    variaveis_data,
    names(dominios_categoricos),
    names(dominios_formatados),
    variaveis_municipio
  ))
}
