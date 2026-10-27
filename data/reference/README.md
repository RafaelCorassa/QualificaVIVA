# Referências territoriais

Os arquivos `ufs.csv`, `municipios.csv` e `geografia/ufs.geojson` são derivados das APIs públicas de Localidades e Malhas do IBGE. O script `scripts/atualizar_referencias_territoriais.R` recompõe esses arquivos a partir das fontes oficiais.

- UFs: `https://servicodados.ibge.gov.br/api/v1/localidades/estados`
- Municípios: `https://servicodados.ibge.gov.br/api/v1/localidades/municipios`
- Malha simplificada das UFs: `https://servicodados.ibge.gov.br/api/v3/malhas/paises/BR?formato=application/vnd.geo+json&qualidade=minima&intrarregiao=UF`

O código SINAN de seis dígitos é derivado dos seis primeiros dígitos do código IBGE de sete dígitos.

## Estabelecimentos CNES

O arquivo `cnes.xlsx` é a referência atualizável dos estabelecimentos de saúde. Preencha a aba `CNES`, mantendo exatamente estas colunas:

- `cnes`: código CNES, tratado como texto e completado com zeros à esquerda até sete dígitos;
- `fantasia`: nome fantasia que será exibido nas tabelas e nos gráficos.

Use uma linha por estabelecimento. Códigos duplicados ou inválidos interrompem a geração do relatório e produzem uma mensagem específica. Quando um CNES válido não possui nome fantasia, o registro é aceito e o gráfico exibe o próprio código acompanhado da indicação de que o nome não está disponível. O aplicativo prioriza `cnes.xlsx`; por compatibilidade, também aceita `cnes.csv` com as mesmas colunas quando o Excel não está presente.
