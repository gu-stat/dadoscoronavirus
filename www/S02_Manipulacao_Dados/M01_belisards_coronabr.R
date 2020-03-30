# ************************************************************************* ----
# Manipulacao Dados - belisards/coronabr                                    ----
# ************************************************************************* ----

# |_ Dados Originais ===========================================================

# Fonte: https://github.com/belisards/coronabr

arquivo <- "https://raw.githubusercontent.com/belisards/coronabr/master/dados/corona_brasil.csv"

salvar_como <- "./www/S00_Dados_Brutos/corona_brasil.csv"

# |_ Download ==================================================================

downloadGithubData <- function() {
  tmp <- read.csv(
    file = arquivo,
    stringsAsFactors = FALSE
  )
  
  write.csv(
    x = tmp,
    file = salvar_como
  )
}

# |_ Update ====================================================================

# Update a cada dia

updateData <- function() {
  if (!dir.exists("./www/S00_Dados_Brutos")) {
    
    dir.create("./www/S00_Dados_Brutos")
    
    downloadGithubData()
    
  } else if ((!file.exists(salvar_como)) || 
             (as.double(
               Sys.time() - file_info(salvar_como)$change_time, 
               units = "days") > 1
             )) {
    downloadGithubData()
  }
}

# Fazer update assim que o app abrir

updateData()

dados_originais <- read.csv(
  file = salvar_como,
  stringsAsFactors = FALSE
)

# |_ Dados Selecionados ========================================================

# Dicionario
# uid = Número de identificação da UF
# suspects = Casos suspeitos
# refuses = Descartados
# confirmado = A coluna não é utilizada até o momento
# deads = Mortes
# local = Aparentemente, não é utilizada. Vide as observações.
# cases = Casos confirmados
# comments = Comentário sobre os dados (Ex: "Transmissão comunitária no município do Rio de Janeiro" ou "1 Portador assintomático")
# broadcast = ?
# date = Data de registro dos dados (%dd/%mm/%yyyy)
# time = Hora do registro dos dados (%hh:%mm)
# uf = A coluna NÃO CONSTAVA no registro do Ministério da Saúde, sendo adicionada pelo script, com a sigla da UF

dados_selecionados <-
  dados_originais %>%
  tibble::as_tibble() %>%
  filter(!is.na(uid)) %>%
  rename(
    "uf_num"             = "uid",
    "casos_suspeitos"    = "suspects",
    "casos_descartados"  = "refuses",
    "casos_confirmados"  = "cases",
    "mortes_confirmadas" = "deaths"
  ) %>%
  mutate(
    dia = as.Date(date),
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
    regiao,
    uf_nome,
    uf,
    uf_num,
    casos_suspeitos,
    casos_descartados,
    casos_confirmados,
    mortes_confirmadas
  ) %>%
  arrange(uf_num, dia)

# \___ Dados Estados -----------------------------------------------------------

dados_estados <- 
  dados_selecionados %>%
  mutate(
    confirmados_dia = fcn_dia_uf(dados_selecionados, "casos_confirmados"),
    mortes_dia      = fcn_dia_uf(dados_selecionados, "mortes_confirmadas")
  ) %>%
  rename(
    "suspeitos_acum"   = "casos_suspeitos",
    "descartados_acum" = "casos_descartados",
    "confirmados_acum" = "casos_confirmados",
    "mortes_acum"      = "mortes_confirmadas"
  ) %>%
  arrange(dia, uf_num)

# \___ Dados Brasil ------------------------------------------------------------

dados_brasil <- 
  dados_estados %>%
  group_by(dia) %>%
  summarise(
    suspeitos_acum   = sum(suspeitos_acum, na.rm = T),
    descartados_acum = sum(descartados_acum, na.rm = T),
    confirmados_acum = sum(confirmados_acum, na.rm = T), 
    mortes_acum      = sum(mortes_acum, na.rm = T),
    confirmados_dia  = sum(confirmados_dia, na.rm = T), 
    mortes_dia       = sum(mortes_dia, na.rm = T)
  )

# |_ Dados Selecionados ========================================================

# |_ Datas =====================================================================

# \___ Atualizacao Base --------------------------------------------------------

data_atualizacao <- file_info(salvar_como)$modification_time

data_atualizacao_br <- format(as.POSIXlt(data_atualizacao), "%d/%m/%Y, %H:%M")

# \___ Datas das Observacoes ---------------------------------------------------

data_range <- 
  dados_selecionados %>%
  select(dia) %>%
  distinct() %>%
  arrange(desc(dia))

data_comeco <- ymd(data_range$dia[dim(data_range)[1]])

data_final  <- ymd(data_range$dia[1])

data_comeco_br <- format(as.Date(data_comeco), "%d/%m/%Y")

data_final_br  <- format(as.Date(data_final), "%d/%m/%Y")

# \___ Datas das Previsoes -----------------------------------------------------

dias.a.frente <- 10

data_range_forecast <- format(
  c(data_final + days(dias.a.frente:1), c(data_range)$dia), 
  format = "%d/%m/%Y"
)

# ************************************************************************* ####
# FIM                                                                       ####
# **************************************************************************** #