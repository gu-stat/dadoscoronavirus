# ************************************************************************* ----
# UI - Modulo                                                               ----
# ************************************************************************* ----

fontesModuleUI <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    uiOutput(ns("markdown_fontes")) %>% withSpinner(type = 8)
  )
  
}

# ************************************************************************* ----
# Server - Modulo                                                           ----
# ************************************************************************* ----

fontesModule <- function(input, output, session){
  
  ns <- session$ns
  
  # |_ Markdown ================================================================
  
  
  output$markdown_fontes <- renderUI({
    
    fontes_html <- "./www/S03_Modulos/M04_Secao_Fontes/info_fontes.html"
    
    if (!file.exists(fontes_html)) {
      rmarkdown::render(
        input = "./www/S03_Modulos/M04_Secao_Fontes/info_fontes.rmd",
        output_format = html_document(self_contained = TRUE),
        output_file = "info_fontes.html"
      )  
    }

    shiny::includeHTML(fontes_html) 
 
  })
  
}

# ************************************************************************* ####
# FIM                                                                       ####
# **************************************************************************** #