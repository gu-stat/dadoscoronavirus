# ************************************************************************* ----
# UI - Modulo                                                               ----
# ************************************************************************* ----

boxesModuleUI <- function(id) {
  ns <- NS(id)
  
  fluidRow(
    column(
      width = 12, 
      valueBoxOutput(ns("casos_confirmados"), width = 11) %>%
        withSpinner(type = 8)
    ),
    br(),
    column(
      width = 12, 
      valueBoxOutput(ns("casos_confirmados_100k"), width = 11) %>% 
        withSpinner(type = 8)
    ),
    br(),
    column(
      width = 12, 
      valueBoxOutput(ns("mortes_confirmadas"), width = 11) %>%
        withSpinner(type = 8)
    ),
    br(),
    column(
      width = 12, 
      valueBoxOutput(ns("taxa_mortalidade"), width = 11) %>%
        withSpinner(type = 8)
    )
  )
}

# ************************************************************************* ----
# Server - Modulo                                                           ----
# ************************************************************************* ----

boxesModule <- function(input, output, session, dados_analise){
  
  # |_ Casos Confirmados =======================================================

  output$casos_confirmados <- renderValueBox({
    
    dados_analise() %>%
      tail(1) %>%
      select(casos_confirmados) %>%
      round(digits = 0) %>%
      prettyNum(big.mark = ".", decimal.mark = ",") %>%
      valueBox(
        subtitle = "Casos Confirmados",
        color    = cores_box_confirmado_caso,
        width    = NULL,
        icon     = icon("plus-circle")
      )
  })
  
  # |_ Casos Confirmados Por 100k ==============================================
  
  output$casos_confirmados_100k <- renderValueBox({
    
    dados_analise() %>%
      tail(1) %>%
      select(confirmed_per_100k_inhabitants) %>%
      # round(digits = 2) %>%
      # prettyNum(big.mark = ".", decimal.mark = ",") %>%
      valueBox(
        subtitle = "Casos Confirmados por 100 Mil Habitantes",
        color    = cores_box_confirmado_caso_100k,
        width    = NULL,
        icon     = icon("user-plus")
      )
  })
  
  # |_ Mortes Confirmadas ======================================================
  
  output$mortes_confirmadas <- renderValueBox({
    
    dados_analise() %>%
      tail(1) %>%
      select(mortes_confirmadas) %>%
      round(digits = 0) %>%
      prettyNum(big.mark = ".", decimal.mark = ",") %>%
      valueBox(
        subtitle = "Mortes Confirmadas", 
        color    = cores_box_confirmado_morte,
        width    = NULL,
        icon     = icon("procedures") 
      )
  })
  
  # |_ Taxa de Mortalidade =====================================================
  
  output$taxa_mortalidade <- renderValueBox({
    dados_analise() %>%
      tail(1) %>%
      mutate(tx_mortalidade_br = mortes_confirmadas/casos_confirmados*100) %>%
      select(tx_mortalidade_br) %>%
      round(digits = 2) %>%
      prettyNum(big.mark = ".", decimal.mark = ",") %>%
      valueBox(
        subtitle = "Taxa de Mortalidade, %", 
        color    = cores_box_taxa_mortalidade,
        width    = 12,
        icon     = icon("percent")
      )
  })
  
}

# ************************************************************************* ####
# FIM                                                                       ####
# **************************************************************************** #