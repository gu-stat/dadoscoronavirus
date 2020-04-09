# ************************************************************************* ----
# UI - Modulo                                                               ----
# ************************************************************************* ----

sobreModuleUI <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    uiOutput(ns("markdown_sobre")) %>% withSpinner(type = 8)
  )
  
}

# ************************************************************************* ----
# Server - Modulo                                                           ----
# ************************************************************************* ----

sobreModule <- function(input, output, session){
  
  ns <- session$ns
  
  # |_ Markdown ================================================================
  
  
  output$markdown_sobre <- renderUI({
    
    sobre_html <- "./www/S03_Modulos/M05_Secao_Sobre/sobre.html"
    
    #if (!file.exists(sobre_html)) {
      rmarkdown::render(
        input = "./www/S03_Modulos/M05_Secao_Sobre/sobre.rmd",
        output_format = html_document(self_contained = TRUE),
        output_file = "sobre.html"
      )  
    #}

    shiny::includeHTML(sobre_html) 
 
  })
  
}

# ************************************************************************* ####
# FIM                                                                       ####
# **************************************************************************** #