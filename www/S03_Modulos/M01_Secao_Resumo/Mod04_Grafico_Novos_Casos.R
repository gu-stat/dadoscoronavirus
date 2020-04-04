# ************************************************************************* ----
# UI - Modulo                                                               ----
# ************************************************************************* ----

graficoNCModuleUI <- function(id) {
  ns <- NS(id)
  #fluidRow(
    #class = "row-eq-height",
    box(
      width = 12,
      highchartOutput(ns("novos_casos"))%>% withSpinner(type = 8),
      uiOutput(ns("escala_novos_casos"))
    )
  #)
}

# ************************************************************************* ----
# Server - Modulo                                                           ----
# ************************************************************************* ----

graficoNCModule <- function(input, output, session, dados_analise, local){
  
  ns <- session$ns
  
  # |_ Escala ==================================================================
  
  output$escala_novos_casos <- renderUI({
    awesomeRadio(
      inputId  = ns("escala_novos_casos"),
      label    = "Escala",
      choices  = list("Unidades" = "unidades", "Log10" = "log10"),
      selected = "unidades",
      inline   = TRUE,
      status   = "danger",
      checkbox = TRUE
    )
  })
  
  # |_ Grafico - Barras ========================================================

  output$novos_casos <- renderHighchart({

    req(input$escala_novos_casos)
    
    # \__ Dados ----------------------------------------------------------------

    tmp <-
      dados_analise() %>%
      filter(dia >= '2020-02-26') %>%
      mutate(
        confirmados_analise = fcn_escala(
          confirmados_dia, input$escala_novos_casos
        ),
        mortes_analise = fcn_escala(mortes_dia, input$escala_novos_casos)
      )
    
    # \__ Fonte e Label --------------------------------------------------------
    
    if (local() == "Brasil") {
      tmp_uf <- "Brasil"
      #tmp_fonte <- "Dados: https://github.com/belisards/coronabr"
      tmp_fonte <- "Dados: Brasil.io - https://brasil.io/dataset/covid19/caso"
    } else {
      tmp_uf <- tmp %>% select(uf) %>% distinct() %>% unlist()
      tmp_fonte <- "Dados: Brasil.io - https://brasil.io/dataset/covid19/caso"
    }
    
    # \__ Plotar Grafico -------------------------------------------------------
    
    highchart() %>%
      # \\____ Info na Tooltip ####
    hc_tooltip(crosshairs = TRUE, shared = TRUE) %>%
      # \\____ Zoom e Espacamento ####
    # Espacamento necessario para mostrar Fonte Dados e Credito Criacao
    hc_chart(zoomType = "xy", spacingBottom = 23) %>%
      # \\____ Incluir Dia no Eixo X ####
    hc_xAxis(categories = format(as.Date(tmp$dia), "%d/%m")) %>%
      # \\____ Incluir Escala no Eixo Y ####
    hc_yAxis(
      title = list(text = fcn_escala_ylab(input$escala_novos_casos))
    ) %>%
      # \\____ Casos Confirmados ####
    hc_add_series(
      data  = tmp$confirmados_analise,
      type  = "areaspline",
      color = cores_graficos_confirmado_caso,
      name  = "Casos Confirmados"
    ) %>%
      # \\____ Mortes Confirmadas ####
    hc_add_series(
      data  = tmp$mortes_analise,
      type  = "line",
      color = cores_graficos_confirmado_morte,
      name  = "Mortes Confirmadas"
    ) %>%
      # \\____ Legenda ####
    hc_legend(
      align         = "center",
      verticalAlign = "bottom",
      layout        = "horizontal",
      itemDistance = 50
    ) %>%
      # \\____ Titulo ####
    hc_title(
      text = paste0("Número de Novos Casos por Dia - ", tmp_uf),
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
      text = credito_grafico,
      position = list(align = "right", y = -2)
      
    ) %>%
      # \\____ Botao Exportar Imagem ####
    hc_exporting(enabled = TRUE)

    # highchart() %>%
    #   # ADD ZOOM
    #   hc_chart(zoomType = "xy", spacingBottom = 23) %>%
    #   # BLANK PLOT
    #   hc_xAxis(categories = format(as.Date(tmp$dia), "%d/%m")) %>%
    #   # Casos Confirmados
    #   hc_add_series(
    #     data  = tmp$confirmados_analise,
    #     type  = "column",
    #     color = cores_graficos_confirmado_caso,
    #     name  = "Casos Confirmados"
    #   ) %>%
    #   # Casos Confirmados
    #   hc_add_series(
    #     data  = tmp$mortes_analise,
    #     type  = "column",
    #     color = cores_graficos_confirmado_morte,
    #     name  = "Mortes Confirmadas"
    #   ) %>%
    #   # LEGEND
    #   hc_legend(
    #     align         = "center",
    #     verticalAlign = "bottom",
    #     layout        = "horizontal",
    #     itemDistance = 50
    #   ) %>%
    #   # TITLE
    #   hc_title(
    #     text = paste0(
    #       "Número de Novos Casos por Dia - ", tmp_uf
    #     ),
    #     align ="left",
    #     style = list(fontSize= "16px")
    #   ) %>%
    #   hc_subtitle(
    #     text = tmp_fonte,
    #     verticalAlign = "bottom",
    #     align  = "right",
    #     style  = list(fontSize= "9px")
    #   ) %>%
    #   # TOOLTIP
    #   hc_tooltip(crosshairs = TRUE, shared = TRUE) %>%
    #   # AXIS
    #   hc_yAxis(
    #     title = list(text = fcn_escala_ylab(input$escala_novos_casos))
    #   ) %>%
    #   hc_credits(
    #     enabled = TRUE,
    #     text = "Gráfico: Gustavo Varela-Alvarenga - ogustavo.com/pt/",
    #     position = list(align = "right", y = -2)
    # 
    #   ) %>%
    #   # Exporting
    #   hc_exporting(enabled = TRUE)
  })
  
}

# ************************************************************************* ####
# FIM                                                                       ####
# **************************************************************************** #