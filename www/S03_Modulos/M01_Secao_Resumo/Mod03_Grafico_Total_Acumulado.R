# ************************************************************************* ----
# UI - Modulo                                                               ----
# ************************************************************************* ----

graficoTAModuleUI <- function(id) {
  ns <- NS(id)
  fluidRow(
    class = "row-eq-height",
    box(
      width = 11,
      highchartOutput(ns("total_acumulado"))%>% withSpinner(type = 8),
      uiOutput(ns("escala_acumulado"))
    )
  )
}

# ************************************************************************* ----
# Server - Modulo                                                           ----
# ************************************************************************* ----

graficoTAModule <- function(input, output, session, dados_analise, local){
  
  ns <- session$ns
  
  # |_ Escala ==================================================================
  
  output$escala_acumulado <- renderUI({
    awesomeRadio(
      inputId  = ns("escala_acumulado"),
      label    = "Escala",
      choices  = list("Unidades" = "unidades", "Log10" = "log10"),
      selected = "unidades",
      inline   = TRUE,
      status   = "danger",
      checkbox = TRUE
    )
  })
  
  # |_ Grafico - Linhas/Area ===================================================
  
  output$total_acumulado <- renderHighchart({

    req(input$escala_acumulado)
    
    # \__ Dados ----------------------------------------------------------------
    
    tmp <-
      dados_analise() %>%
      filter(dia >= '2020-02-26') %>%
      mutate(
        confirmados_analise = fcn_escala(
          casos_confirmados, 
          input$escala_acumulado
        ),
        mortes_analise = fcn_escala(mortes_confirmadas, input$escala_acumulado)
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
        title = list(text = fcn_escala_ylab(input$escala_acumulado))
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
        text = paste0("Número Total de Casos - ", tmp_uf),
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