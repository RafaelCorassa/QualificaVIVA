# Análise integral do Script_Validade.do

Fonte: `C:/Users/PC/Documents/Script_Validade.do`, com três regras efetivas e
identificadores não sequenciais 1, 3 e 4.

## Regra 1

Sinaliza o registro quando nenhuma destas 11 variáveis é igual a `"1"`:
`les_autop`, `viol_fisic`, `viol_psico`, `viol_tort`, `viol_sexu`, `viol_traf`,
`viol_finan`, `viol_negli`, `viol_infan`, `viol_legal`, `viol_outr`.
O comentário do script fala em 12 naturezas, mas a lista contém 11 variáveis.
Valores vazios não incrementam `n_sim`; portanto, se todas estiverem vazias, o
registro é sinalizado.

## Regra 3

Sinaliza `nm_pacient` quando a string começa, com comparação sensível a
maiúsculas/minúsculas, por `FETO `, `ABORTO`, `NATIMORT` ou `NASCITUR`. Como a
base padronizada já converte o campo para maiúsculas, a expressão é aplicada
diretamente ao valor padronizado.

## Regra 4

Sinaliza quando todas as condições são verdadeiras:

- `cs_sexo == "M"`;
- `nu_idade_n` entre 4020 e 4059, inclusive;
- `cs_raca != "5"`;
- `orient_sex` e `ident_gen` fora de 1, 2 e 3;
- nenhum caractere `"1"` nas oito variáveis de deficiência enumeradas;
- nenhum caractere `"1"` em `les_autop`, `viol_sexu`, `viol_tort`, `viol_traf`,
  `viol_legal`;
- nenhum caractere `"1"` nas 13 relações com o autor enumeradas.

## Ausências e ambiguidades

- Há um comando incompleto `foreach var of varlist` imediatamente antes da
  geração de `nr4`; ele não possui corpo e não foi traduzido.
- O script usa `tran_men`, enquanto a preparação usa `tran_ment`. Por decisão
  posterior do responsável metodológico, a implementação R adota `tran_ment`
  como dependência da regra 4.
- `nu_idade_n` não aparece no script de formatação, mas é uma dependência
  explícita da regra 4; sua ausência original também torna a regra não
  executável.
- O comentário da regra 1 diz 12 naturezas, enquanto o macro possui 11. A lista
  executável do macro foi preservada.
- No Stata, `gen nr*=1 if ...` deixa os demais registros como missing. No módulo
  analítico R, regras executadas são representadas por `TRUE/FALSE`; `NA` fica
  reservado a regras não executáveis por ausência ou ambiguidade de variável.
