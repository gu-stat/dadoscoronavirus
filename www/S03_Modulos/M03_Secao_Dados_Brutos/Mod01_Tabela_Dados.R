# ************************************************************************* ----
# UI - Modulo                                                               ----
# ************************************************************************* ----

dadosModuleUI <- function(id) {
  ns <- NS(id)
  
  sidebarLayout(
    sidebarPanel(
      width = 2,
      # |_ Fonte Widget ========================================================
      
      uiOutput(ns("selecao_fonte")),
      hr(),
      # \__ Aviso --------------------------------------------------------------
      p("Para mais informações, veja a aba 'Fontes'.")
      # hr(),
      # # |_ Disclaimer Dados ==================================================
      # uiOutput("ultima_observacao"),
      # uiOutput("ultima_atualizacao")
    ),
    mainPanel(
      # |_ Painel Principal ====================================================
      id = "main-dados-brutos",
      width = 10,
      # \__ Tabela -------------------------------------------------------------
      DTOutput(ns('tabela_dados_brutos'))%>% withSpinner(type = 8)
    )
  )
  
}

# ************************************************************************* ----
# Server - Modulo                                                           ----
# ************************************************************************* ----

dadosModule <- function(input, output, session, dados_originais_br_io, 
                        dados_selecionados_cidade, dados_estados, dados_brasil){
  
  ns <- session$ns
  
  # |_ Selecao Fonte ===========================================================
  
  output$selecao_fonte <- renderUI({
    awesomeRadio(
      inputId  = ns("fonte_tabela"),
      label    = "",
      choices  = list(
        "Dados Originais - Fonte: Brasil.IO" = "fonte_o_brasil_io",
        "Dados Usados nos Gráficos: Municípios" = "fonte_m_brasil_io",
        "Dados Usados nos Gráficos: Estados" = "fonte_e_brasil_io",
        "Dados Usados nos Gráficos: Brasil" = "fonte_br_brasil_io"
        ),
      selected = "fonte_o_brasil_io",
      inline   = FALSE,
      status   = "danger",
      checkbox = TRUE
    )
  })
  
  # |_ Tabela - Dados Brutos ===================================================
  
  output$tabela_dados_brutos = renderDT(
    {
    
      req(input$fonte_tabela)
      
      if (input$fonte_tabela == "fonte_o_brasil_io") {
        tmp <- dados_originais_br_io()
      } else if (input$fonte_tabela == "fonte_m_brasil_io") {
        tmp <- dados_selecionados_cidade()
      } else if (input$fonte_tabela == "fonte_e_brasil_io") {
        tmp <- dados_estados()
      } else if (input$fonte_tabela == "fonte_br_brasil_io") {
        tmp <- dados_brasil()
      }
      
      tmp
      
    },
    filter = "top",
    extensions = 'Buttons',
    options = list(
      dom = 'Bfrtip',
      buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
    )
  )
  
}

# ************************************************************************* ####
# FIM                                                                       ####
# **************************************************************************** #