# QualificaVIVA

Aplicativo Shiny para importação, padronização e avaliação da qualidade dos
dados do VIVA-Sinan.

## Funcionalidades

- importação de DBF, DTA, XLS, XLSX e CSV;
- leitura de formatos recentes do Stata por meio do `haven`;
- tratamento de codificações de texto e preservação da base original;
- padronização auditável de campos, códigos e datas;
- enriquecimento opcional com o tipo da unidade da competência mensal CNES-ST;
- descarte incremental de bases, agregações e artefatos temporários para reduzir o uso de RAM;
- análise de validade com indicadores por registro e regra;
- análise de consistência com indicadores por registro, regra e grupo temático;
- referências atualizáveis de UF, município e CNES;
- mapas, rankings, Paretos e matrizes de prioridade interativas;
- relatórios analíticos em HTML e PDF;
- seleção independente de Brasil, regiões, UFs e municípios para detalhamento
  territorial;
- testes automatizados dos módulos e dos relatórios.

## Manutenção simplificada

As personalizações cotidianas estão centralizadas em:

```text
R/configuracao_aplicativo.R
```

Esse arquivo contém títulos, cores, tamanhos de figuras, limites de rankings,
nomes dos relatórios e outras opções que podem ser ajustadas sem modificar os
motores analíticos.

As instruções completas estão em [GUIA_DE_MANUTENCAO.md](GUIA_DE_MANUTENCAO.md).

## Estrutura

```text
app.R                                   Interface e servidor Shiny
R/configuracao_aplicativo.R             Configuração editável central
R/modo_desenvolvimento.R                Recarga seletiva durante edições
R/gerenciamento_memoria.R               Liberação de RAM e arquivos temporários

R/importar_*.R                          Importação dos formatos suportados
R/regras_padronizacao.R                 Definições da padronização
R/padronizar_base.R                     Motor de tratamento e auditoria
R/enriquecer_cnes.R                     Seleção, download e vínculo mensal CNES-ST

R/regras_validade.R                     Catálogo das regras de validade
R/analisar_validade.R                   Execução da validade
R/regras_consistencia.R                 Catálogo e grupos de consistência
R/analisar_consistencia.R               Execução da consistência

R/referencias_territoriais.R            Relacionamentos territoriais
R/indicadores_relatorio_validade.R       Agregações do relatório de validade
R/indicadores_relatorio_consistencia.R   Agregações do relatório de consistência
R/priorizacao_relatorio.R                Modelo comum de prioridade e interação
R/visualizacoes_relatorio.R              Gráficos e mapas compartilhados
R/visualizacoes_relatorio_consistencia.R Gráficos específicos de consistência
R/relatorio_validade.R                  HTML/PDF de validade
R/relatorio_consistencia.R              HTML/PDF de consistência

data/reference/                         UF, municípios, CNES e malha geográfica
scripts/                                Rotinas de atualização e homologação
tests/testthat/                         Testes automatizados
docs/                                   Documentação técnica e histórica
renv.lock                               Dependências reproduzíveis
GUIA_DE_MANUTENCAO.md                   Manual para atualização do projeto
```

Durante ajustes, o modo de desenvolvimento pode ser ativado no início de
`R/configuracao_aplicativo.R`. Ele recarrega os módulos seguros no próximo
comando, preservando a base já carregada. Ao concluir, use `ativo = FALSE`.
Consulte o guia de manutenção para saber quais módulos são recarregados.

## Executar

Com o R aberto na raiz do projeto:

```r
renv::restore()
shiny::runApp()
```

Ou pelo terminal do Windows, quando o `Rscript` estiver no `PATH`:

```powershell
Rscript -e "renv::restore(); shiny::runApp()"
```

## Testes

```r
testthat::test_dir("tests/testthat", reporter = "summary")
```

Para gerar relatórios de homologação com a base DTA mantida em `Dados`:

```r
source("scripts/validar_relatorio_validade.R")
source("scripts/validar_relatorio_consistencia.R")
```

## Dependências principais

- `shiny` e `DT`: interface;
- `haven`, `foreign`, `readxl` e `readr`: importação;
- `microdatasus`: download da fotografia mensal de estabelecimentos do CNES;
- `ggplot2`: gráficos;
- `sf`: mapas;
- `htmltools`, `jsonlite` e `base64enc`: relatórios HTML;
- Microsoft Edge ou Google Chrome: impressão em PDF;
- `testthat`: testes automatizados.

As versões reproduzíveis ficam registradas em `renv.lock`.

O enriquecimento requer internet apenas no momento em que o usuário aciona
**Enriquecer**. A referência estática `data/reference/cnes.xlsx`, usada para
nomes fantasia nos relatórios, permanece independente desse download mensal.

As matrizes interativas usam SVG e JavaScript incorporados ao próprio HTML e não
exigem internet para abrir o relatório; a versão estática do PDF continua sendo
produzida com `ggplot2` a partir da mesma classificação.

## Abrangências dos relatórios

Nos relatórios de Validade e Consistência, Brasil, regiões, UFs e municípios são
opções independentes. Nenhum nível territorial é acrescentado implicitamente.
É possível gerar desde um relatório exclusivamente nacional até um documento
contendo somente municípios ou qualquer combinação entre os quatro níveis.

O seletor municipal apresenta apenas códigos `id_municip` reconhecidos na
referência oficial e presentes na base atual. Dentro de uma seção municipal, a
priorização é feita entre os estabelecimentos notificadores daquele município;
o mesmo CNES registrado em municípios diferentes é agregado separadamente. A
metodologia e a auditoria territorial permanecem em todos os relatórios.

## Autoria e citação

- **Autor(es):** [preencher nome do autor ou equipe]
- **Instituição:** [preencher instituição]
- **Contato:** [preencher contato]
- **Ano:** [preencher ano]
- **Versão:** [preencher versão]
- **Licença:** [definir licença de uso e distribuição]

O QualificaVIVA foi concebido e desenvolvido por seus autores. O OpenAI Codex foi
utilizado como ferramenta de apoio à implementação, revisão e teste do código.
As decisões metodológicas, a validação dos resultados e a responsabilidade pelo
aplicativo são de seus autores.

Os mesmos dados devem ser preenchidos na seção `aplicativo$autoria` do arquivo
`R/configuracao_aplicativo.R`. Para citações, utilize e complete o arquivo
`CITATION.cff` disponibilizado na raiz do projeto.
