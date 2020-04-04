# ************************************************************************* ----
# Pacotes                                                                   ----
# ************************************************************************* ----

library("shiny")

library("dplyr")

library("tidyr")

library("fs")

library("shinydashboard")

library("shinydashboardPlus")

library("shinyWidgets")

library("shinythemes")

library("highcharter")

library("lubridate")

library("geojsonio")

library("jsonlite")

library("brazilmaps")

library("shinycssloaders")

library("DT")

library("knitr")

library("rmarkdown") 

library("forecast")

library("zoo")

library("rvest")

library("stringr")

#library("viridisLite")

# ************************************************************************* ----
# Dados                                                                     ----
# ************************************************************************* ----

dados_originais_br_io <- reactiveFileReader(
  intervalMillis = 1.8e+6, 
  session = NULL, 
  filePath = "https://brasil.io/dataset/covid19/caso?format=csv", 
  readFunc = utils::read.csv
)

# ************************************************************************* ----
# Helpers                                                                   ----
# ************************************************************************* ----

source('./www/S01_Helpers/get_brmap_fixed.R', local = TRUE)
source('./www/S01_Helpers/escalas.R', local = TRUE)
source('./www/S01_Helpers/casos_novos_dia.R', local = TRUE)
source('./www/S01_Helpers/previsoes.R', local = TRUE)
source('./www/S01_Helpers/grafico_previsoes.R', local = TRUE)

# ************************************************************************* ----
# Modulos e UI                                                              ----
# ************************************************************************* ----

# |_ Secao: Resumo =============================================================

# \__ Modulos ------------------------------------------------------------------

source('./www/S03_Modulos/M01_Secao_Resumo/Mod01_Boxes_Casos.R', local = TRUE)
source('./www/S03_Modulos/M01_Secao_Resumo/Mod02_Mapas.R', local = TRUE)
source('./www/S03_Modulos/M01_Secao_Resumo/Mod03_Grafico_Total_Acumulado.R', local = TRUE)
source('./www/S03_Modulos/M01_Secao_Resumo/Mod04_Grafico_Novos_Casos.R', local = TRUE)
source('./www/S03_Modulos/M01_Secao_Resumo/Mod05_Grafico_Mais_Afetados.R', local = TRUE)

# \__ UI -----------------------------------------------------------------------

source('./www/S03_Modulos/M01_Secao_Resumo/UI01_resumo.R', local = TRUE)

# |_ Secao: Previsao ===========================================================

# \__ Modulos ------------------------------------------------------------------

source('./www/S03_Modulos/M02_Secao_Previsao/Mod01_Previsao.R', local = TRUE)

# \__ UI -----------------------------------------------------------------------

source('./www/S03_Modulos/M02_Secao_Previsao/UI01_previsao.R', local = TRUE)

# |_ Secao: Dados Brutos =======================================================

# \__ Modulos ------------------------------------------------------------------

source('./www/S03_Modulos/M03_Secao_Dados_Brutos/Mod01_Tabela_Dados.R', local = TRUE)

# \__ UI -----------------------------------------------------------------------

source('./www/S03_Modulos/M03_Secao_Dados_Brutos/UI01_dados_brutos.R', local = TRUE)

# |_ Secao: Fontes =============================================================

# \__ Modulos ------------------------------------------------------------------

source('./www/S03_Modulos/M04_Secao_Fontes/Mod01_Fontes_Dados.R', local = TRUE)

# \__ UI -----------------------------------------------------------------------

source('./www/S03_Modulos/M04_Secao_Fontes/UI01_fontes.R', local = TRUE)

# |_ CSS =======================================================================

#overallCSS <- includeCSS("./www/S05_CSS/overallCSS.css")

# |_ HTML ======================================================================


# ************************************************************************* ----
# Outras Opcoes                                                             ----
# ************************************************************************* ----

# |_ Cores =====================================================================

# \__ Boxes --------------------------------------------------------------------

cores_box_confirmado_caso  <- "aqua"
cores_box_confirmado_caso_100k <- "yellow"
cores_box_confirmado_morte <- "red"
cores_box_suspeito_caso    <- "darkgreen"
cores_box_descartado_caso  <- "green"
cores_box_taxa_mortalidade <- "purple"


# \__ Graficos/Mapas -----------------------------------------------------------

cores_graficos_confirmado_caso  <- "#068ad6"

cores_graficos_confirmado_morte <- "red"
cores_graficos_suspeito_caso    <- "darkgreen"
cores_graficos_descartado_caso  <- "green"
cores_graficos_taxa_mortalidade <- "purple"

cores_mapa_confirmado_caso <- "#0488d4"
cores_mapa_confirmado_caso_min <- "#f7f7f7"
  #"#d9eefa"

cores_mapa_confirmado_morte <- "red"
cores_mapa_confirmado_morte_min <- "#f7f7f7"
  #"#ffc4c6"

# As cores a seguir sao colorblind safe:
# https://gka.github.io/palettes/#/7|d|000000,ff0000,ffff00|90ee90,008000,068ad6|1|1
cores_previsao_95  <- '#5fb597'
cores_previsao_80  <- '#8fdc70'
cores_previsao_est <- '#ffff00'

cores_previsao_80_anterior  <- '#e38704'
cores_previsao_95_anterior  <- '#83330e'
cores_previsao_est_anterior <- '#000000'

# |_ Fontes ====================================================================

fonte_grafico_br <- "Dados: Brasil.io - https://brasil.io/dataset/covid19/caso"

# |_ Creditos ==================================================================

credito_grafico <- "Fonte: Dados Coronavírus - dadoscoronavirus.com/brasil"
credito_mapa <- "Fonte: Dados Coronavírus - dadoscoronavirus.com/brasil"

# |_ Titulos/Labels ============================================================

label_br <- "Brasil"

y_label_casos_confirmados       <- "Número de Casos Confirmados"
titulo_total_casos_confirmados  <- "Número Total de Casos Confirmados"
legenda_casos_confirmados       <- "Casos Confirmados"

label_total_mortes <- "Número Total de Mortes"
titulo_total_mortes_confirmadas  <- "Número Total de Mortes Confirmadas"

# |_ Opcoes Previsoes ==========================================================

data_ultima_previsao <- "2020-03-31"

horizonte.previsao <- 5

# ************************************************************************* ####
# FIM                                                                       ####
# **************************************************************************** #