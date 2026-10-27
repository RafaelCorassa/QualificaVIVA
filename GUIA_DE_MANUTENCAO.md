# Guia de manutenção do QualificaVIVA

Este guia foi escrito para permitir ajustes cotidianos sem exigir conhecimento
prévio de toda a programação. Ele indica quais arquivos podem ser modificados,
quais devem ser tratados como internos e como conferir uma mudança antes de
utilizar o aplicativo com uma base oficial.

## 1. Regra principal

Para alterações de títulos, cores, tamanhos de figuras, quantidade de itens nos
rankings ou nome do aplicativo, comece sempre por:

```text
R/configuracao_aplicativo.R
```

Esse arquivo possui uma **área de edição segura**. Na maioria das mudanças
visuais, nenhum outro script deve ser alterado.

Antes de editar:

1. faça uma cópia do arquivo que será modificado;
2. altere apenas os valores à direita do sinal `=`;
3. preserve aspas, vírgulas, parênteses e nomes à esquerda do sinal `=`;
4. salve o arquivo em UTF-8;
5. execute os testes descritos na seção 8.

## 2. Organização do projeto

O aplicativo está dividido em três camadas. Essa separação evita que uma mudança
de aparência altere acidentalmente uma regra epidemiológica.

### Camada A — configuração editável

| Arquivo | Finalidade |
|---|---|
| `R/configuracao_aplicativo.R` | Títulos, cores, dimensões, limites e identidade visual |
| `R/modo_desenvolvimento.R` | Recarga seletiva de módulos durante edições e testes |
| `R/gerenciamento_memoria.R` | Descarte de objetos grandes e artefatos temporários |
| `data/reference/` | Planilhas e arquivos territoriais atualizáveis |
| `GUIA_DE_MANUTENCAO.md` | Instruções para manutenção |

### Camada B — regras analíticas

| Arquivo | Finalidade |
|---|---|
| `R/regras_padronizacao.R` | Variáveis, comprimentos e domínios da padronização |
| `R/regras_validade.R` | Catálogo e requisitos das regras de validade |
| `R/analisar_validade.R` | Condições lógicas das regras de validade |
| `R/regras_consistencia.R` | Catálogo, descrições e grupos das regras de consistência |
| `R/analisar_consistencia.R` | Condições lógicas das regras de consistência |

Altere essa camada somente quando a definição da análise realmente mudar.

### Camada C — mecanismos internos

| Grupo de arquivos | Finalidade |
|---|---|
| `R/importar_*.R` | Leitura de DBF, DTA, Excel e CSV |
| `R/padronizar_base.R` | Tratamento e auditoria da base |
| `R/enriquecer_cnes.R` | Seleção da competência, download e vínculo mensal CNES-ST |
| `R/referencias_territoriais.R` | Relacionamento com UF, município e CNES |
| `R/indicadores_relatorio_*.R` | Agregações usadas nos relatórios |
| `R/priorizacao_relatorio.R` | Classificação, tabela e interação das matrizes de prioridade |
| `R/visualizacoes_relatorio*.R` | Construção técnica de gráficos e mapas |
| `R/relatorio_*.R` | Montagem de HTML e PDF |
| `app.R` | Interface, estados da sessão e ligação entre os módulos |

Esses arquivos normalmente não precisam ser modificados para ajustes visuais.

## 3. Onde fazer cada mudança comum

| Quero modificar | Local em `R/configuracao_aplicativo.R` |
|---|---|
| Nome do aplicativo | `aplicativo$nome` |
| Subtítulo da página inicial | `aplicativo$subtitulo` |
| Limite máximo do arquivo importado | `aplicativo$limite_upload_mib` |
| Título geral de um relatório | `relatorios$validade$titulo_documento` ou `relatorios$consistencia$titulo_documento` |
| Nome dos arquivos HTML/PDF | `nome_arquivo` do respectivo relatório |
| Título “Mapas” | `titulos$mapas` do respectivo relatório; a numeração é automática |
| Outro título de seção | Lista `titulos` do respectivo relatório |
| Largura e altura dos mapas | `relatorios$geral$figuras$mapas` |
| Mapas lado a lado ou verticais | `figuras$mapas$disposicao` |
| Tamanho dos Paretos | `figuras$pareto` |
| Tamanho dos gráficos de grupos | `figuras$grupos` |
| Tamanho da matriz estática do PDF | `figuras$matriz_prioridade` |
| Percentis da priorização | `priorizacao$percentil_absoluto`, `percentil_absoluto2` e `percentil_relativo` |
| Mínimo da Prioridade 3 | `priorizacao$minimo_problemas_prioridade_3` |
| Escala inicial das matrizes | `priorizacao$escala_padrao` |
| Transformações dos eixos | `priorizacao$transformacao_x` e `priorizacao$transformacao_y` |
| Cores das prioridades | `priorizacao$categorias` |
| Quantidade nos rankings | `relatorios$geral$limites$ranking` |
| Quantidade nos Paretos | `relatorios$geral$limites$pareto` |
| Cor principal do aplicativo e dos relatórios | `relatorios$geral$cores$principal` |
| Cores dos mapas | `cores$mapa_baixo`, `cores$mapa_alto` e `cores$mapa_ausente` |
| Largura máxima do relatório HTML | `layout$largura_conteudo_px` |
| Altura máxima de figuras no PDF | `layout$altura_maxima_figura_pdf_px` |
| Textos dos botões de expandir regras | `relatorios$consistencia$controles` |

### Exemplo: alterar o título e aumentar os mapas

No bloco `relatorios$consistencia$titulos`, altere somente:

```r
mapas = "Distribuição geográfica das inconsistências"
```

No bloco `relatorios$geral$figuras$mapas`, altere:

```r
mapas = list(
  largura = 11,
  altura = 7,
  disposicao = "vertical"
)
```

As dimensões são dadas em polegadas. A disposição aceita exatamente
`"lado_a_lado"` ou `"vertical"`.

### Ajustar o modelo de prioridade

O bloco `relatorios$geral$priorizacao` concentra todos os parâmetros comuns aos
relatórios de Validade e Consistência. Os valores atualmente definidos são:

- P95 para o indicador absoluto e para o indicador relativo, além do corte
  absoluto adicional P98 usado pela Prioridade 1;
- mínimo de 30 registros problemáticos para a Prioridade 3;
- método de quantil 7 do R;
- escala `"pseudo_log"` nos dois eixos;
- nomes e cores das cinco categorias. Todas as categorias usam bolhas circulares;
  a área da bolha representa o número absoluto de registros problemáticos.

Os percentis são calculados em cada universo territorial (municípios ou
estabelecimentos da UF/município exibido), considerando somente localidades com pelo
menos um problema. O total de notificações aparece como informação de contexto,
mas não participa da classificação. Localidades sem problemas permanecem na
tabela completa e não alteram os cortes.

As transformações aceitas são `"pseudo_log"`, `"log1p"`, `"sqrt"` e
`"linear"`. Elas mudam apenas o espaçamento dos pontos: valores originais,
percentis e prioridades não são modificados. Depois de alterar qualquer corte,
execute especialmente `tests/testthat/test-priorizacao.R` e gere os dois
relatórios de homologação.

Na homologação com Minas Gerais e Roraima foram comparadas escala linear,
`log1p`, raiz quadrada, posições percentuais e valores divididos pelos cortes.
A pseudo-logarítmica foi mantida nos dois eixos porque preserva o zero, amplia a
separação perto da origem, comprime os extremos suavemente e permite rotular o
eixo com os valores reais. `log1p` produziu resultado próximo no eixo absoluto,
mas tem transição mais abrupta junto ao zero; a raiz quadrada comprimiu menos os
extremos; posições percentuais esconderam diferenças de magnitude e criaram
mais empates; dividir pelo corte apenas mudou a unidade e não resolveu a
assimetria. Essa escolha é visual e não participa do algoritmo de prioridade.

O código do modelo fica em `R/priorizacao_relatorio.R`. O HTML interativo usa
SVG e JavaScript incorporados, sem acesso à internet. O PDF usa um gráfico
estático criado por `grafico_matriz_prioridade()` em
`R/visualizacoes_relatorio.R`; ambos recebem a mesma tabela classificada.

O HTML prepara antecipadamente uma camada na escala original e outra na escala
transformada. Os botões apenas alternam a camada visível, sem recalcular a
análise. A escala escolhida na tela antes de gerar o relatório é usada como
escala inicial do HTML e como escala da figura estática do PDF.

Para manutenção visual do SVG, procure em `javascript_matrizes_prioridade()`:

- `passosLineares()`: intervalos regulares dos eixos na escala original;
- `posicionarRotulo()`: mantém nomes críticos dentro do painel;
- `aplicarDestaque()` e `limparDestaque()`: hover e clique nas bolhas;
- `mostrarTooltip()`: conteúdo e posição do quadro de indicadores.

A versão estática equivalente é mantida por `grafico_matriz_prioridade()` em
`R/visualizacoes_relatorio.R`, incluindo o reposicionamento de rótulos perto das
bordas e as marcas regulares quando o PDF usa a escala original.

## 4. Manutenção das regras de consistência

As regras são separadas em catálogo e avaliação para facilitar auditoria.

### Alterar descrição, variáveis necessárias ou grupo

Edite `R/regras_consistencia.R`. Cada regra informa:

- identificador;
- descrição exibida;
- variáveis necessárias;
- grupo temático.

### Alterar a condição lógica

Edite a função correspondente em `R/analisar_consistencia.R`. Por exemplo, a
função `avaliar_regra_consistencia_16()` contém a condição da Regra 16.

O registro que associa identificadores às funções avaliadoras está no mesmo
arquivo, na seção comentada como **Registro central**.

### Incluir uma regra

1. cadastre a regra em `R/regras_consistencia.R`;
2. implemente uma função avaliadora em `R/analisar_consistencia.R`;
3. inclua a nova função no registro central de avaliadores;
4. crie testes em `tests/testthat/test-consistencia.R`;
5. execute toda a suíte de testes;
6. gere um relatório de homologação.

### Remover uma regra

1. remova-a do catálogo;
2. remova-a do registro central de avaliadores;
3. mantenha ou remova a função antiga de forma consciente;
4. atualize os testes e a documentação metodológica;
5. confira se o grupo temático ainda possui regras.

Não reutilize o identificador de uma regra removida para uma condição diferente,
pois isso prejudica comparações históricas.

## 5. Manutenção das regras de validade

O procedimento é equivalente:

- catálogo e dependências: `R/regras_validade.R`;
- condições lógicas: `R/analisar_validade.R`;
- testes: `tests/testthat/test-validade.R`.

## 6. Atualização das referências territoriais

Os arquivos ficam em `data/reference/`:

| Arquivo | Conteúdo |
|---|---|
| `ufs.csv` | Códigos, siglas, nomes e regiões das UFs |
| `municipios.csv` | Códigos e nomes dos municípios |
| `cnes.xlsx` | Código CNES e nome fantasia dos estabelecimentos |
| `geografia/ufs.geojson` | Malha usada nos mapas |

O campo CNES deve permanecer com sete caracteres e ser tratado como texto para
preservar zeros à esquerda. Após atualizar uma referência, execute os testes e
gere um relatório de homologação.

### 6.1 Enriquecimento mensal CNES-ST

O arquivo `cnes.xlsx` acima continua sendo a referência estática de nomes
fantasia dos relatórios. A ação **Enriquecer**, disponível depois da
padronização, é um fluxo separado: ela escolhe o maior `nu_ano`, encontra o
último mês válido de `dt_notific` somente nesse ano e baixa uma única fotografia
CNES-ST por meio do pacote `microdatasus`.

Essa fotografia é vinculada a toda a base exclusivamente por
`id_unidade = CNES`, inclusive aos registros de anos anteriores. Apenas
`tp_unid` é acrescentada à base; `CNES` e `COMPETEN` permanecem na referência
baixada. O download exige acesso à internet e conflitos de `TP_UNID` para o
mesmo estabelecimento interrompem a operação sem alterar a base carregada.

### 6.2 Abrangências territoriais dos relatórios

Os painéis de Validade e Consistência permitem escolher independentemente:

- Brasil;
- uma ou mais regiões;
- uma ou mais UFs;
- um ou mais municípios presentes na base e reconhecidos em `municipios.csv`.

Selecionar um nível não acrescenta outro automaticamente. O atalho para
selecionar as UFs de uma região é uma ação explícita e o botão de limpeza
desmarca Brasil e esvazia os três seletores. Pelo menos uma abrangência deve ser
informada.

Os municípios são identificados pelo `id_municip` normalizado com seis dígitos.
Na seção municipal, o próprio município é o território analisado e os
estabelecimentos notificadores são priorizados internamente. A pré-agregação
municipal usa a chave município + CNES; assim, um código de estabelecimento que
apareça em municípios diferentes não mistura totais, grupos ou regras. A
agregação estadual por UF + CNES permanece separada e preserva o comportamento
dos relatórios de UF.

Os títulos em `configuracao_aplicativo.R` não devem conter números fixos. A
montagem atribui números contínuos apenas às seções geradas e mantém metodologia
e auditoria como a última seção.

## 7. Como executar o aplicativo

Abra o R ou o RStudio na pasta principal do projeto e execute:

```r
renv::restore()
shiny::runApp()
```

`renv::restore()` é necessário apenas na primeira utilização ou quando houver
mudança de dependências.

## 8. Modo de desenvolvimento: testar alterações sem reiniciar

O interruptor está no início de `R/configuracao_aplicativo.R`, no bloco
`desenvolvimento`. Para ativá-lo, use:

```r
desenvolvimento = list(
  ativo = TRUE,
  mostrar_mensagens = TRUE
)
```

Com `ativo = TRUE`, o aplicativo consulta novamente esse bloco antes de cada
comando e recarrega somente os scripts seguros da etapa solicitada. Assim, não
é necessário fechar o aplicativo nem importar novamente a base a cada ajuste:

| Comando no aplicativo | O que é recarregado | O que é preservado |
|---|---|---|
| Padronizar base | Regras e motor da padronização | Base bruta durante a execução; prévia após o sucesso |
| Enriquecer | Seleção, download e vínculo mensal CNES-ST | Base padronizada |
| Executar validade | Regras e motor de validade | Base padronizada |
| Executar consistência | Regras e motor de consistência | Base padronizada |
| Gerar relatório de validade | Referências, indicadores, gráficos e relatório | Resultado da validade |
| Gerar relatório de consistência | Referências, indicadores, gráficos e relatório | Resultado da consistência |

Mudanças em `app.R`, nos scripts de importação ou na instalação de pacotes ainda
exigem reiniciar o aplicativo. Se uma regra analítica for alterada, clique em
**Executar** novamente antes de gerar o relatório; a geração do relatório não
recalcula a análise automaticamente.

### 8.1 Uso de memória

Depois de uma padronização bem-sucedida, a base bruta completa é descartada e
somente sua prévia de 20 linhas permanece na tela. Para aplicar novamente a
padronização, importe o arquivo original outra vez. O resultado da padronização
guarda apenas auditoria e metadados; a base padronizada possui uma única cópia
persistente na sessão.

Durante as análises, totais e grupos são calculados incrementalmente, sem criar
uma matriz adicional com todas as regras. Os relatórios agregam um nível
territorial de cada vez e limitam municípios e estabelecimentos à união das UFs
e dos municípios explicitamente selecionados. Depois da gravação do HTML, as tabelas e o documento
em memória são liberados antes da geração do PDF. Ao gerar um novo relatório ou
encerrar a sessão, o diretório temporário anterior também é removido.

Alterações de títulos, cores e dimensões dos **relatórios** passam a valer na
próxima geração. Elementos da própria interface já montada — como nome do app,
subtítulo, estilos da tela e limite de upload — exigem reinício, mesmo estando
no arquivo de configuração.

Quando terminar as edições, retorne ao funcionamento normal mudando somente:

```r
ativo = FALSE
```

Salve o arquivo. A desativação passa a valer no próximo comando, mesmo que o
aplicativo permaneça aberto. O modo normal é recomendado para uso rotineiro,
pois evita leituras extras dos scripts.

## 9. Como executar os testes

No R ou no RStudio:

```r
testthat::test_dir("tests/testthat", reporter = "summary")
```

O resultado esperado deve terminar sem falhas nem erros. Um teste reprovado
significa que a alteração precisa ser revista antes do uso em produção.

Para validar os relatórios com a base DTA da pasta `Dados`:

```r
source("scripts/validar_relatorio_validade.R")
source("scripts/validar_relatorio_consistencia.R")
```

Esses scripts geram HTML e PDF em `output/` para inspeção.

## 10. Lista de verificação dos relatórios

Depois de alterar títulos, figuras ou regras, confirme:

- se o HTML abre sem mensagem de erro;
- se o PDF foi criado;
- se somente as abrangências escolhidas aparecem e se a ordem é Brasil, regiões,
  UFs, municípios e metodologia;
- se a numeração principal é contínua mesmo quando Brasil não foi selecionado;
- se a seção municipal contém apenas o resumo, os resultados analíticos e a
  priorização dos estabelecimentos daquele município;
- se somente os territórios selecionados possuem seção detalhada;
- se mapas, rankings e Paretos estão legíveis;
- se as matrizes exibem bolhas circulares proporcionais ao absoluto, cores de
  prioridade e tooltip com os valores originais;
- se os rótulos permanecem dentro do painel e hover/clique destacam uma bolha
  enquanto tornam as demais mais pálidas, sem sombra;
- se a escala original possui marcas regulares sem sobreposição e a escala
  transformada preserva suas marcas próprias;
- se filtros, busca, zoom e legenda das matrizes funcionam sem internet;
- se HTML e PDF atribuem as mesmas categorias às localidades;
- se tabelas não ultrapassam as páginas;
- se grupos e regras podem ser expandidos no HTML;
- se o PDF apresenta todas as regras expandidas;
- se os totais permanecem iguais aos resultados exibidos no aplicativo.

## 11. Quando uma mudança exige programação interna

Uma mudança não deve ser feita apenas no arquivo de configuração quando envolve:

- nova regra analítica;
- mudança da lógica de uma regra;
- novo formato de importação;
- nova dimensão territorial;
- novo tipo de gráfico;
- mudança do processo de padronização;
- alteração da estrutura das tabelas do relatório.

Nesses casos, identifique primeiro o módulo responsável, preserve testes
existentes e acrescente testes para o comportamento novo.

## 12. Como ler os comentários dos scripts

Os scripts usam três níveis de comentários:

1. o cabeçalho explica a responsabilidade do arquivo;
2. títulos com traços separam grandes etapas;
3. comentários imediatamente anteriores às funções explicam sua finalidade.

Nos arquivos de regras, os comentários indicam o número e o tema de cada regra.
Nos relatórios, os comentários distinguem preparação de dados, construção de
figuras, montagem das seções, estilos e gravação dos arquivos.

Se uma função for criada futuramente, ela deve receber um comentário anterior
explicando o que recebe, o que produz e qualquer decisão importante para sua
manutenção.
