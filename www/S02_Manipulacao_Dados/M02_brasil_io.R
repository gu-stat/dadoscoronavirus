# ************************************************************************* ----
# Manipulacao Dados - Brasil.IO                                             ----
# ************************************************************************* ----

# |_ Dados Originais ===========================================================

# Fonte: https://brasil.io/dataset/covid19/caso

#arquivo <- "https://raw.githubusercontent.com/gu-stat/dados_covid19_br/master/covid19-brasil_io.csv"

#arquivo <- "https://raw.githubusercontent.com/gu-stat/dados_covid19_br/master/covid19-brasil_io_novo.csv"

# salvar_como <- "./www/S00_Dados_Brutos/covid19-brasil_io_novo.csv"

salvar_como <- "./www/S00_Dados_Brutos/covid19-brasil_io.csv"

# |_ Download ==================================================================

# downloadGithubData <- function() {
#   tmp <- read.csv(
#     file = arquivo,
#     stringsAsFactors = FALSE
#   )
#   
#   write.csv(
#     x = tmp,
#     file = salvar_como
#   )
# }

# |_ Update ====================================================================

# Update a cada dia

# updateData <- function() {
#   if (!dir.exists("./www/S00_Dados_Brutos")) {
#     
#     dir.create("./www/S00_Dados_Brutos")
#     
#     downloadGithubData()
#     
#   } else if ((!file.exists(salvar_como)) || 
#              (as.double(
#                Sys.time() - file_info(salvar_como)$change_time, 
#                units = "days") > 1
#              )) {
#     downloadGithubData()
#   }
# }
# 
# # Fazer update assim que o app abrir
# 
# updateData()

dados_originais_br_io <- read.csv(
  file = salvar_como,
  stringsAsFactors = FALSE
)

# |_ Dados Selecionados ========================================================

dados_selecionados_cidade <-
  dados_originais_br_io %>%
  tibble::as_tibble() %>%
  rename(
    "uf"                 = "state",
    "municipio"          = "city",
    "cod_municipio"      = "city_ibge_code"
  ) %>%
  mutate(
    dia = as.Date(date),
    uf_num = substr(cod_municipio, 1, 2),
    casos_confirmados  = as.numeric(confirmed),
    mortes_confirmadas = as.numeric(deaths),
    confirmed_per_100k_inhabitants = round(as.numeric(confirmed_per_100k_inhabitants), 2)
  ) %>%
  mutate(
    uf_nome = case_when(
      uf_num == 11 ~ 'Rondônia',
      uf_num == 12 ~ 'Acre',
      uf_num == 13 ~ 'Amazonas',
      uf_num == 14 ~ 'Roraima',
      uf_num == 15 ~ 'Pará',
      uf_num == 16 ~ 'Amapá',
      uf_num == 17 ~ 'Tocantins',
      uf_num == 21 ~ 'Maranhão',
      uf_num == 22 ~ 'Piauí',
      uf_num == 23 ~ 'Ceará',
      uf_num == 24 ~ 'Rio Grande do Norte',
      uf_num == 25 ~ 'Paraíba',
      uf_num == 26 ~ 'Pernambuco',
      uf_num == 27 ~ 'Alagoas',
      uf_num == 28 ~ 'Sergipe',
      uf_num == 29 ~ 'Bahia',
      uf_num == 31 ~ 'Minas Gerais',
      uf_num == 32 ~ 'Espírito Santo',
      uf_num == 33 ~ 'Rio de Janeiro',
      uf_num == 35 ~ 'São Paulo',
      uf_num == 41 ~ 'Paraná',
      uf_num == 42 ~ 'Santa Catarina',
      uf_num == 43 ~ 'Rio Grande do Sul',
      uf_num == 50 ~ 'Mato Grosso do Sul',
      uf_num == 51 ~ 'Mato Grosso',
      uf_num == 52 ~ 'Goiás',
      uf_num == 53 ~ 'Distrito Federal'
    ),
    regiao = case_when(
      uf_num == 11 ~ 'Norte',
      uf_num == 12 ~ 'Norte',
      uf_num == 13 ~ 'Norte',
      uf_num == 14 ~ 'Norte',
      uf_num == 15 ~ 'Norte',
      uf_num == 16 ~ 'Norte',
      uf_num == 17 ~ 'Norte',
      uf_num == 21 ~ 'Nordeste',
      uf_num == 22 ~ 'Nordeste',
      uf_num == 23 ~ 'Nordeste',
      uf_num == 24 ~ 'Nordeste',
      uf_num == 25 ~ 'Nordeste',
      uf_num == 26 ~ 'Nordeste',
      uf_num == 27 ~ 'Nordeste',
      uf_num == 28 ~ 'Nordeste',
      uf_num == 29 ~ 'Nordeste',
      uf_num == 31 ~ 'Sudeste',
      uf_num == 32 ~ 'Sudeste',
      uf_num == 33 ~ 'Sudeste',
      uf_num == 35 ~ 'Sudeste',
      uf_num == 41 ~ 'Sul',
      uf_num == 42 ~ 'Sul',
      uf_num == 43 ~ 'Sul',
      uf_num == 50 ~ 'Centro-Oeste',
      uf_num == 51 ~ 'Centro-Oeste',
      uf_num == 52 ~ 'Centro-Oeste',
      uf_num == 53 ~ 'Centro-Oeste'
    )
  ) %>%
  select(
    dia,
    is_last,
    regiao,
    uf_nome,
    uf,
    uf_num,
    municipio,
    cod_municipio,
    place_type,
    casos_confirmados,
    mortes_confirmadas,
    confirmed_per_100k_inhabitants,
    estimated_population_2019
  ) %>%
  arrange(uf_num, cod_municipio, dia)

# \___ Corrige Dados Municipio (inclui datas faltantes) e Inclui #Casos Dia ----

# dados_selecionados_cidade2 <- fcn_corrige_mun(DF = dados_selecionados_cidade)
# 
# dados_selecionados_cidade2 <- 
#   dados_selecionados_cidade2 %>%
#   mutate(
#     uf_num = case_when(
#       uf == 'RO' ~ 11,
#       uf == 'AC' ~ 12,
#       uf == 'AM' ~ 13,
#       uf == 'RR' ~ 14,
#       uf == 'PA' ~ 15,
#       uf == 'AP' ~ 16,
#       uf == 'TO' ~ 17,
#       uf == 'MA' ~ 21,
#       uf == 'PI' ~ 22,
#       uf == 'CE' ~ 23,
#       uf == 'RN' ~ 24,
#       uf == 'PB' ~ 25,
#       uf == 'PE' ~ 26,
#       uf == 'AL' ~ 27,
#       uf == 'SE' ~ 28,
#       uf == 'BA' ~ 29,
#       uf == 'MG' ~ 31,
#       uf == 'ES' ~ 32,
#       uf == 'RJ' ~ 33,
#       uf == 'SP' ~ 35,
#       uf == 'PR' ~ 41,
#       uf == 'SC' ~ 42,
#       uf == 'RS' ~ 43,
#       uf == 'MS' ~ 50,
#       uf == 'MT' ~ 51,
#       uf == 'GO' ~ 52,
#       uf == 'DF' ~ 53
#     ),
#     uf_nome = case_when(
#       uf_num == 11 ~ 'Rondônia',
#       uf_num == 12 ~ 'Acre',
#       uf_num == 13 ~ 'Amazonas',
#       uf_num == 14 ~ 'Roraima',
#       uf_num == 15 ~ 'Pará',
#       uf_num == 16 ~ 'Amapá',
#       uf_num == 17 ~ 'Tocantins',
#       uf_num == 21 ~ 'Maranhão',
#       uf_num == 22 ~ 'Piauí',
#       uf_num == 23 ~ 'Ceará',
#       uf_num == 24 ~ 'Rio Grande do Norte',
#       uf_num == 25 ~ 'Paraíba',
#       uf_num == 26 ~ 'Pernambuco',
#       uf_num == 27 ~ 'Alagoas',
#       uf_num == 28 ~ 'Sergipe',
#       uf_num == 29 ~ 'Bahia',
#       uf_num == 31 ~ 'Minas Gerais',
#       uf_num == 32 ~ 'Espírito Santo',
#       uf_num == 33 ~ 'Rio de Janeiro',
#       uf_num == 35 ~ 'São Paulo',
#       uf_num == 41 ~ 'Paraná',
#       uf_num == 42 ~ 'Santa Catarina',
#       uf_num == 43 ~ 'Rio Grande do Sul',
#       uf_num == 50 ~ 'Mato Grosso do Sul',
#       uf_num == 51 ~ 'Mato Grosso',
#       uf_num == 52 ~ 'Goiás',
#       uf_num == 53 ~ 'Distrito Federal'
#     ),
#     regiao = case_when(
#       uf_num == 11 ~ 'Norte',
#       uf_num == 12 ~ 'Norte',
#       uf_num == 13 ~ 'Norte',
#       uf_num == 14 ~ 'Norte',
#       uf_num == 15 ~ 'Norte',
#       uf_num == 16 ~ 'Norte',
#       uf_num == 17 ~ 'Norte',
#       uf_num == 21 ~ 'Nordeste',
#       uf_num == 22 ~ 'Nordeste',
#       uf_num == 23 ~ 'Nordeste',
#       uf_num == 24 ~ 'Nordeste',
#       uf_num == 25 ~ 'Nordeste',
#       uf_num == 26 ~ 'Nordeste',
#       uf_num == 27 ~ 'Nordeste',
#       uf_num == 28 ~ 'Nordeste',
#       uf_num == 29 ~ 'Nordeste',
#       uf_num == 31 ~ 'Sudeste',
#       uf_num == 32 ~ 'Sudeste',
#       uf_num == 33 ~ 'Sudeste',
#       uf_num == 35 ~ 'Sudeste',
#       uf_num == 41 ~ 'Sul',
#       uf_num == 42 ~ 'Sul',
#       uf_num == 43 ~ 'Sul',
#       uf_num == 50 ~ 'Centro-Oeste',
#       uf_num == 51 ~ 'Centro-Oeste',
#       uf_num == 52 ~ 'Centro-Oeste',
#       uf_num == 53 ~ 'Centro-Oeste'
#     )
#   )

# \___ Dados Estados -----------------------------------------------------------

dados_estados <- 
  dados_selecionados_cidade %>%
  filter(place_type == "state") %>%
  mutate(
    confirmados_dia = fcn_dia_uf(., variavel = "casos_confirmados"),
    mortes_dia      = fcn_dia_uf(., variavel = "mortes_confirmadas"),
    confirmed_per_100k_inhabitants = prettyNum(
      round(confirmed_per_100k_inhabitants, 2),
      big.mark = ".", 
      decimal.mark = ","
    )
  )

# \___ Dados Brasil ------------------------------------------------------------

populacao_2019 <- 
  dados_estados %>%
  group_by(dia, uf) %>%
  summarise(
    estimated_population_2019 = mean(estimated_population_2019)
  ) %>%
  ungroup() %>%
  distinct(estimated_population_2019) %>% 
  sum()

dados_brasil <- 
  dados_estados %>%
  group_by(dia) %>%
  summarise(
    confirmados_dia      = sum(confirmados_dia, na.rm = TRUE),
    mortes_dia           = sum(mortes_dia, na.rm = TRUE)
  ) %>%
  mutate(
    casos_confirmados  = cumsum(confirmados_dia),
    mortes_confirmadas = cumsum(mortes_dia),
    confirmed_per_100k_inhabitants = casos_confirmados/populacao_2019*100000,
    confirmed_per_100k_inhabitants = prettyNum(
      round(confirmed_per_100k_inhabitants, 2),
      big.mark = ".", 
      decimal.mark = ","
    )
  )

# |_ Dados Selecionados ========================================================

# |_ Datas =====================================================================

# \___ Atualizacao Base --------------------------------------------------------

data_atualizacao <- file_info(salvar_como)$modification_time

data_atualizacao_br <- format(as.POSIXlt(data_atualizacao), "%d/%m/%Y, %H:%M")

# \___ Datas das Observacoes ---------------------------------------------------

data_range <- 
  dados_selecionados_cidade %>%
  select(dia) %>%
  distinct() %>%
  arrange(desc(dia))

data_comeco <- ymd(data_range$dia[dim(data_range)[1]])

data_final  <- ymd(data_range$dia[1])

data_comeco_br <- format(as.Date(data_comeco), "%d/%m/%Y")

data_final_br  <- format(as.Date(data_final), "%d/%m/%Y")

# ************************************************************************* ####
# FIM                                                                       ####
# **************************************************************************** #