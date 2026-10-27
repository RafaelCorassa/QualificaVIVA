# Atualização das referências territoriais ------------------------------------
#
# Script administrativo executado manualmente para baixar do IBGE as UFs, os
# municípios e a malha geográfica. Ele substitui os arquivos em `data/reference`;
# por isso, deve ser executado apenas quando houver intenção de atualizar a base.

# Diretórios de destino das tabelas e da malha GeoJSON.
diretorio <- file.path("data", "reference")
diretorio_geo <- file.path(diretorio, "geografia")
dir.create(diretorio_geo, recursive = TRUE, showWarnings = FALSE)

# Endereços oficiais da API de localidades e malhas do IBGE.
url_ufs <- "https://servicodados.ibge.gov.br/api/v1/localidades/estados"
url_municipios <- "https://servicodados.ibge.gov.br/api/v1/localidades/municipios"
url_geografia <- paste0(
  "https://servicodados.ibge.gov.br/api/v3/malhas/paises/BR?",
  "formato=application/vnd.geo+json&qualidade=minima&intrarregiao=UF"
)

# Converte a resposta de UFs no esquema interno usado pelo relatório.
ufs_json <- jsonlite::fromJSON(url_ufs, simplifyVector = FALSE)
ufs <- do.call(rbind, lapply(ufs_json, function(x) data.frame(
  codigo_uf = sprintf("%02d", as.integer(x$id)),
  sigla_uf = x$sigla,
  nome_uf = x$nome,
  codigo_regiao = as.character(x$regiao$id),
  nome_regiao = x$regiao$nome,
  stringsAsFactors = FALSE
)))
ufs <- ufs[order(as.integer(ufs$codigo_uf)), ]

# Converte municípios e deriva o código SINAN de seis dígitos do código IBGE.
municipios_json <- jsonlite::fromJSON(url_municipios, simplifyVector = FALSE)
municipios <- do.call(rbind, lapply(municipios_json, function(x) {
  uf <- x$microrregiao$mesorregiao$UF
  if (is.null(uf)) uf <- x[["regiao-imediata"]][["regiao-intermediaria"]]$UF
  codigo_ibge <- sprintf("%07d", as.integer(x$id))
  data.frame(
    codigo_sinan_6 = substr(codigo_ibge, 1L, 6L),
    codigo_ibge_7 = codigo_ibge,
    nome_municipio = x$nome,
    codigo_uf = sprintf("%02d", as.integer(uf$id)),
    sigla_uf = uf$sigla,
    codigo_regiao = as.character(uf$regiao$id),
    nome_regiao = uf$regiao$nome,
    stringsAsFactors = FALSE
  )
}))
municipios <- municipios[order(as.integer(municipios$codigo_uf), municipios$nome_municipio), ]

# Grava as referências textuais em UTF-8 e baixa a malha em formato binário.
utils::write.csv(ufs, file.path(diretorio, "ufs.csv"), row.names = FALSE,
                 fileEncoding = "UTF-8", na = "")
utils::write.csv(municipios, file.path(diretorio, "municipios.csv"), row.names = FALSE,
                 fileEncoding = "UTF-8", na = "")
utils::download.file(url_geografia, file.path(diretorio_geo, "ufs.geojson"),
                     mode = "wb", quiet = FALSE)

message(sprintf("Referências atualizadas: %d UFs e %d municípios.", nrow(ufs), nrow(municipios)))
