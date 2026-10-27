# Análise da rotina Stata de formatação

Fonte analisada: `C:/Users/PC/Documents/Script formatacao.txt`, com 491 linhas.
O pedido menciona `Script_formatacao.do`, mas esse arquivo não estava na pasta do
projeto; a implementação usa o conteúdo integral do TXT fornecido.

## Inventário e operações

A rotina referencia 197 variáveis distintas. Destas, 184 aparecem nos grupos de
largura: 121 com largura 1; 9 com largura 2; 5 com largura 4; 2 com largura 5;
8 com largura 6; 4 com largura 7; 10 com largura 8; 2 com largura 9; 3 com
largura 15; 11 com largura 30; uma com largura 50; 3 com largura 60; 4 com
largura 70; e uma com largura 254. A lista completa e executável está em
`R/regras_padronizacao.R`.

As operações identificadas foram:

1. conversão dos grupos `str*` para texto e truncamento à largura declarada;
2. normalização de nove campos abertos (`nm_pacient`, `nm_mae_pac`,
   `def_espec`, `local_espe`, `viol_espec`, `ag_espec`, `sex_espec`,
   `rel_espec`, `ds_obs`): maiúsculas, decomposição Unicode, remoção de
   diacríticos, remoção de caracteres fora de `A-Z`, `0-9` e espaço, contração
   e aparo de espaços;
3. conversão YMD de 13 variáveis para data;
4. validação de 89 variáveis categóricas com domínios explícitos;
5. conversão, validação e apresentação em dois dígitos de `local_ocor`,
   `cs_escol_n` e `viol_motiv`;
6. validação de `id_mn_resi` e `id_mn_ocor` por lista explícita e exceções.

## Domínios

- `cs_sexo`: F, M, I.
- `cs_raca`: 1, 2, 3, 4, 5, 9.
- `sit_conjug`: 1, 2, 3, 4, 8, 9.
- `cs_gestant`: 1, 2, 3, 4, 5, 6, 9.
- `orient_sex`, `ident_gen`: 1, 2, 3, 8, 9.
- `zona`, `zona_ocor`, `autor_sexo`: 1, 2, 3, 9.
- `cicl_vid`: 1, 2, 3, 4, 5, 9.
- grupo de 57 indicadores: 1, 2, 9.
- grupo de 22 indicadores: 1, 2, 8, 9.
- `local_ocor`: 1–9 e 99, apresentado com dois dígitos.
- `cs_escol_n`: 0–10, apresentado com dois dígitos.
- `viol_motiv`: 1–9, 88 e 99, apresentado com dois dígitos.

Valores vazios são aceitos. Valores fora do domínio são esvaziados somente na
base padronizada e contabilizados na auditoria.

## Municípios

Foram extraídos 5.596 códigos explícitos únicos: 5.570 municípios e 26 códigos
agregados de UF terminados em `0000`. O código `510183` aparece duas vezes no
script e foi deduplicado na referência. Também são aceitos todos os inteiros da
faixa 530000–539999. A referência está em
`inst/referencias/codigos_municipios.csv`.

## Ambiguidades preservadas e decisões

- O macro `str11` contém as 13 datas, mas não há laço `recast str11`; em R elas
  são tratadas diretamente pela conversão YMD.
- No Stata, a apresentação de dois dígitos nos três códigos especiais só é
  executada quando o `assert` encontra ao menos um inválido. Em R a apresentação
  é aplicada sempre, de modo determinístico, conforme solicitado para a base
  padronizada.
- A faixa 530000–539999 aceita códigos que não correspondem necessariamente a
  municípios individualizados. A faixa foi preservada sem restrição adicional.
- Os grupos `enc_*` são dimensionados, enquanto o grupo de domínio 1/2/9 usa
  principalmente variáveis mais novas (`rede_*`, `deleg_*` etc.). Ambos os
  conjuntos foram mantidos como escritos.
- `tpuninot` é distinto de `tp_uni_ext` e `tp_not` no script; os três foram
  mantidos como variáveis esperadas.
- Nomes esperados são convertidos para minúsculas sem alterar nomes adicionais.
  Se a entrada contiver simultaneamente duas colunas que diferem apenas por
  maiúsculas/minúsculas, a coluna já escrita exatamente no nome canônico tem
  precedência; a outra é preservada como coluna adicional.

## Codificação de caracteres na implementação R

Antes das regras Stata, a cópia padronizada passa por uma camada de codificação.
No modo automático, sequências UTF-8 válidas são preservadas e sequências
inválidas são decodificadas como Windows-1252. O usuário pode substituir essa
decisão por Windows-1252, ISO-8859-1 ou UTF-8. A conversão é feita antes do
truncamento para impedir erros de string multibyte; a base bruta não é alterada
e as quantidades convertidas são registradas na auditoria.
