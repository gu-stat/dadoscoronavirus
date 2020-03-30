# ************************************************************************* ----
# UI                                                                        ----
# ************************************************************************* ----

fluidPage(
 
  # Titulo do App ==============================================================
  
  title = "Dashboard COVID-19 BRASIL",
  
  # Funcionalidades ============================================================
  
  # Allow to use functions from 'shinydashboard' into a classic 'shiny' app
  useShinydashboard(),
  
  # CSS ========================================================================

  overallCSS,       # See global.R
  # footerCSS,        # See global.R
  
 # Navegacao ===================================================================
  navbarPage(
    
    # \__ Titulo ---------------------------------------------------------------
    
    #title = "Coronavírus: Casos Observados e Previsões para Estados e Brasil",
    title = "Coronavírus: Casos Observados nos Estados e no Brasil",
    collapsible = TRUE,
    fluid = TRUE,
    
    # \__ Painel - Resumo ------------------------------------------------------
    tabPanel(title = "Resumo", pagina_resumo, value = "pagina-resumo"),
    
    # \__ Painel - Previsao ----------------------------------------------------
    tabPanel(title = "Previsões", previsao, value = "previsao"),
    
    # \__ Painel - Fontes ------------------------------------------------------
    tabPanel(title = "Fontes", fontes, value = "fontes"),
    
    # \__ Painel - Dados Brutos ------------------------------------------------
    tabPanel(title = "Dados Brutos", dados_brutos, value = "dados-brutos")
    
    
    
  ),
  
 # Footer ======================================================================
  hr(),
  div(
    class = "footer", 
    id = "myFooter",
    div(
      class = "container",
      fluidRow(
        # \__ Desenvolvimento --------------------------------------------------
        column(
          width = 6,
          a("Desenvolvido por: Gustavo Varela-Alvarenga - ",
            href = "https://www.ogustavo.com/pt/"),
          a("www.ogustavo.com", href = "https://www.ogustavo.com/pt/"),
          p("Versão Beta - v03.02")
        ),
        # \__ Comunicacao ------------------------------------------------------
        column(
          width = 6,
          p("Viu algum erro ou problema? Me mande uma mensagem:"),
          a("Twitter: @ogustavo_com", 
            href = "https://twitter.com/ogustavo_com"),
          br(),
          a("GitHub: gu-stat/dadoscoronavirus", 
            href = "https://github.com/gu-stat/dadoscoronavirus"),
          br(),
          p("E-mail: falecom at ogustavo.com")
        )
      )
    )
  )
  
)

# ************************************************************************* ####
# FIM                                                                       ####
# **************************************************************************** #