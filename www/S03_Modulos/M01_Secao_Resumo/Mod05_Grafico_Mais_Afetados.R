# ************************************************************************* ----
# UI - Modulo                                                               ----
# ************************************************************************* ----

graficoMAModuleUI <- function(id) {
  ns <- NS(id)

  #fluidRow(
   # class = "row-eq-height",
    box(
      width = 12,
      highchartOutput(ns("barra_mais_afetados"))%>% withSpinner(type = 8),
      uiOutput(ns("variavel_mais_afetados"))
    )
  #)
  
}

# ************************************************************************* ----
# Server - Modulo                                                           ----
# ************************************************************************* ----

graficoMAModule <- function(input, output, session, dados_analise, local){
  
  ns <- session$ns
  
  # |_ Variavel de Analise =====================================================

  output$variavel_mais_afetados <- renderUI({
    awesomeRadio(
      inputId  = ns("variavel_bar_mais_afetados"),
      label    = "Variável",
      choices  = list(
        "Casos Confirmados" = "confirmados_mais_afetados",
        "Mortes Confirmadas" = "mortes_mais_afetados"
      ),
      selected = "confirmados_mais_afetados",
      inline   = TRUE,
      status   = "danger",
      checkbox = TRUE
    )
  })

  # |_ Grafico - Barras ========================================================

  output$barra_mais_afetados <- renderHighchart({

    req(input$variavel_bar_mais_afetados)
    
    # \__ Brasil ---------------------------------------------------------------

    if (local() == "Brasil") {

      tmp_dados_mais_afetados <-
        dados_estados %>%
        #filter(dia == data_final) %>%
        filter(is_last == "True") %>%
        group_by(uf_num)

      title_mais_afetados <- "Estados Mais Afetados"
      
      if (input$variavel_bar_mais_afetados == "confirmados_mais_afetados") {
        
        # \\____ Casos Confirmados ####
        
        tmp_mais_afetados <-
          tmp_dados_mais_afetados %>%
          rename(
            "variavel_mais_afetados" = casos_confirmados
          ) %>%
          arrange(desc(variavel_mais_afetados)) %>%
          head(n=10) %>%
          filter(variavel_mais_afetados > 0)
        
        tmp_cor_barra_mais_afetados <- cores_graficos_confirmado_caso
        
        tmp_name_mais_afetados <- "Total de Casos Confirmados"
        
      } else {
        
        # \\____ Mortes Confirmadas ####
        
        tmp_mais_afetados <-
          tmp_dados_mais_afetados %>%
          rename(
            "variavel_mais_afetados" = mortes_confirmadas
          ) %>%
          arrange(desc(variavel_mais_afetados)) %>%
          head(n=10) %>%
          filter(variavel_mais_afetados > 0)
        
        tmp_cor_barra_mais_afetados <- cores_graficos_confirmado_morte
        
        tmp_name_mais_afetados <- "Total de Mortes Confirmadas"
        
      }

    } else {
      
      # \__ Estados/Municipio -------------------------------------------------- 
      
      tmp_dados_mais_afetados <-
        dados_selecionados_cidade %>%
        filter(is_last == "True") %>%
        filter(uf_num == local()) %>%
        filter(place_type == "city")
        
      title_mais_afetados <- "Municípios Mais Afetados"
      
      if (input$variavel_bar_mais_afetados == "confirmados_mais_afetados") {
        
        # \\____ Casos Confirmados ####
        
        tmp_mais_afetados <-
          tmp_dados_mais_afetados %>%
          rename(
            "variavel_mais_afetados" = casos_confirmados
          ) %>%
          arrange(desc(variavel_mais_afetados)) %>%
          head(n=10) %>%
          filter(variavel_mais_afetados > 0)
        
        tmp_cor_barra_mais_afetados <- cores_graficos_confirmado_caso
        
        tmp_name_mais_afetados <- "Total de Casos Confirmados"
        
      } else {
        
        # \\____ Mortes Confirmadas ####
        
        tmp_mais_afetados <-
          tmp_dados_mais_afetados %>%
          rename(
            "variavel_mais_afetados" = mortes_confirmadas
          ) %>%
          arrange(desc(variavel_mais_afetados)) %>%
          head(n=10)
        #%>%
        #  filter(variavel_mais_afetados > 0)
        
        tmp_cor_barra_mais_afetados <- cores_graficos_confirmado_morte
        
        tmp_name_mais_afetados <- "Total de Mortes Confirmadas"
        
      }

    }
    
    # \__ Fonte e Label --------------------------------------------------------
    
    if (local() == "Brasil") {
      #tmp_fonte <- "Dados: https://github.com/belisards/coronabr"
      tmp_fonte <- "Dados: Brasil.io - https://brasil.io/dataset/covid19/caso"
      categoria_mais_afetados <- tmp_mais_afetados$uf
    } else {
      tmp_fonte <- "Dados: Brasil.io - https://brasil.io/dataset/covid19/caso"
      categoria_mais_afetados <- tmp_mais_afetados$municipio
    }
    
    # \__ Plotar Grafico -------------------------------------------------------

    highchart() %>%
      # \\____ Info na Tooltip ####
    hc_tooltip(
      crosshairs = TRUE, 
      shared = TRUE
      # ,
      # useHTML = TRUE,
      # headerFormat = "{point.municipio} </br>"
      #pointFormat = "{point.uf}: {point.value}"
    ) %>%
      # \\____ Zoom e Espacamento ####
    # Espacamento necessario para mostrar Fonte Dados e Credito Criacao
    hc_chart(zoomType = "xy", spacingBottom = 23) %>%
      # \\____ Incluir Categorias no Eixo X ####
    hc_xAxis(categories = categoria_mais_afetados) %>%
      # \\____ Incluir Escala no Eixo Y ####
    hc_yAxis(
      title = list(text = "Número Total")
    ) %>%
      # \\____ Casos Confirmados ####
    hc_add_series(
      data  = tmp_mais_afetados,
      type  = "column",
      hcaes(y = variavel_mais_afetados),
      color = tmp_cor_barra_mais_afetados,
      name  = tmp_name_mais_afetados
    ) %>%
      # \\____ Legenda ####
    hc_legend(
      enabled = FALSE
    ) %>%
      # \\____ Titulo ####
    hc_title(
      text  = title_mais_afetados,
      align ="left",
      style = list(fontSize= "16px")
    ) %>%
      # \\____ Fonte Dados ####
    hc_subtitle(
      text = tmp_fonte,
      verticalAlign = "bottom",
      align  = "right",
      style  = list(fontSize= "9px")
    ) %>%
      # \\____ Credito Criacao ####
    hc_credits(
      enabled = TRUE,
      text = "Gráfico: Gustavo Varela-Alvarenga - ogustavo.com/pt/",
      position = list(align = "right", y = -2)
      
    ) %>%
      # \\____ Botao Exportar Imagem ####
    hc_exporting(enabled = TRUE)

  })

  
}

# ************************************************************************* ####
# FIM                                                                       ####
# **************************************************************************** #