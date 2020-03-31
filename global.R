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

# ************************************************************************* ----
# Helpers                                                                   ----
# ************************************************************************* ----

source('./www/S01_Helpers/get_brmap_fixed.R', local = TRUE)
source('./www/S01_Helpers/escalas.R', local = TRUE)
source('./www/S01_Helpers/casos_novos_dia.R', local = TRUE)

# ************************************************************************* ----
# Dados                                                                     ----
# ************************************************************************* ----

#source('./www/S02_Manipulacao_Dados/M01_belisards_coronabr.R', local = TRUE)
source('./www/S02_Manipulacao_Dados/M02_brasil_io.R', local = TRUE)

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
cores_box_confirmado_morte <- "red"
cores_box_suspeito_caso    <- "darkgreen"
cores_box_descartado_caso  <- "green"
cores_box_taxa_mortalidade <- "purple"

# \__ Graficos/Mapas -----------------------------------------------------------

cores_graficos_confirmado_caso  <- "#67a9cf"
cores_graficos_confirmado_morte <- "red"
cores_graficos_suspeito_caso    <- "darkgreen"
cores_graficos_descartado_caso  <- "green"
cores_graficos_taxa_mortalidade <- "purple"


# ************************************************************************* ####
# FIM                                                                       ####
# **************************************************************************** #