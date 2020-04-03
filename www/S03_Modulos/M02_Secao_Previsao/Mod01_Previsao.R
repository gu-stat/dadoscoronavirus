# ************************************************************************* ----
# UI - Modulo                                                               ----
# ************************************************************************* ----

previsaoModuleUI <- function(id) {
  ns <- NS(id)
  
    mainPanel(
      # |_ Painel Previsao =====================================================
      id = "main-previsao",
      width = 10,
      # \__ Graficos -------------------------------------------------------------
      fluidRow(
        box(
          width = 12,
          highchartOutput(ns('previsao_aan')) %>%
            withSpinner(type = 8)
        )
      ),
      fluidRow(
        box(
          width = 12,
          highchartOutput(ns('previsao_mmn')) %>%
            withSpinner(type = 8)
        )
      ),
      fluidRow(
        box(
          width = 12,
          highchartOutput(ns('previsao_auto')) %>% 
            withSpinner(type = 8)
        )
      )
    )
  
}

# ************************************************************************* ----
# Server - Modulo                                                           ----
# ************************************************************************* ----

previsaoModule <- function(input, output, session){
  
  ns <- session$ns
  
  # |_ Variaveis Temporarias ===================================================
  
  tmp_variavel_analise = "casos_confirmados"
  
  tmp_data_ultima_previsao <- format(
    as.Date(data_ultima_previsao), 
    format = "%Y_%m_%d"
  )
 
  # |_ Previsao AAN ============================================================

  output$previsao_aan = renderHighchart({

    # |_ Variaveis Temporarias ===================================================

    tmp_variavel_analise <- "casos_confirmados"

    tmp_localidade       <- label_br

    tmp_fonte            <- fonte_grafico_br

    tmp_credito          <- credito_grafico

    tmp_y_label          <- y_label_casos_confirmados

    tmp_titulo           <- titulo_total_casos_confirmados

    tmp_legenda          <- legenda_casos_confirmados

    tmp_cor              <- cores_graficos_confirmado_caso

    tmp_data_ultima_previsao <- format(
      as.Date(data_ultima_previsao),
      format = "%Y_%m_%d"
    )

    resultados_previsoes <- fcn_previsoes(
      DF                     = dados_brasil,
      variavel_analise       = tmp_variavel_analise,
      modelo                 = "AAN",
      horizonte_previsao     = horizonte.previsao,
      exporta_dados          = FALSE,
      importa_dados          = TRUE,
      data_previsao_anterior = tmp_data_ultima_previsao
    )

    fcn_grafico_previsoes(
      DF                 = resultados_previsoes$data_plot,
      DF.anterior        = resultados_previsoes$data_plot_anterior,
      modelo             = resultados_previsoes$modelo_usado,
      variavel_analise   = tmp_variavel_analise,
      tmp_localidade     = label_br,
      tmp_fonte          = fonte_grafico_br,
      tmp_credito        = credito_grafico,
      tmp_y_label        = y_label_casos_confirmados,
      tmp_titulo         = titulo_total_casos_confirmados,
      tmp_legenda        = legenda_casos_confirmados,
      tmp_cor            = cores_graficos_confirmado_caso
    )

  })

  # |_ Previsao MMN ============================================================

  output$previsao_mmn = renderHighchart({
    
    tmp_variavel_analise <- "casos_confirmados"
    
    tmp_localidade       <- label_br
    
    tmp_fonte            <- fonte_grafico_br
    
    tmp_credito          <- credito_grafico
    
    tmp_y_label          <- y_label_casos_confirmados
    
    tmp_titulo           <- titulo_total_casos_confirmados
    
    tmp_legenda          <- legenda_casos_confirmados
    
    tmp_cor              <- cores_graficos_confirmado_caso
    
    tmp_data_ultima_previsao <- format(
      as.Date(data_ultima_previsao),
      format = "%Y_%m_%d"
    )
    
    resultados_previsoes <- fcn_previsoes(
      DF                     = dados_brasil,
      variavel_analise       = tmp_variavel_analise,
      modelo                 = "MMN",
      horizonte_previsao     = horizonte.previsao,
      exporta_dados          = FALSE,
      importa_dados          = TRUE,
      data_previsao_anterior = tmp_data_ultima_previsao
    )
    
    fcn_grafico_previsoes(
      DF                 = resultados_previsoes$data_plot,
      DF.anterior        = resultados_previsoes$data_plot_anterior,
      modelo             = resultados_previsoes$modelo_usado,
      variavel_analise   = tmp_variavel_analise,
      tmp_localidade     = label_br,
      tmp_fonte          = fonte_grafico_br,
      tmp_credito        = credito_grafico,
      tmp_y_label        = y_label_casos_confirmados,
      tmp_titulo         = titulo_total_casos_confirmados,
      tmp_legenda        = legenda_casos_confirmados,
      tmp_cor            = cores_graficos_confirmado_caso
    )

  })

  # |_ Previsao Automatico =====================================================

  output$previsao_auto = renderHighchart({
    
    resultados_previsoes <- fcn_previsoes(
      DF                     = dados_brasil,
      variavel_analise       = tmp_variavel_analise,
      modelo                 = "ZZZ",
      horizonte_previsao     = horizonte.previsao,
      exporta_dados          = FALSE,
      importa_dados          = TRUE,
      data_previsao_anterior = tmp_data_ultima_previsao
    )

    fcn_grafico_previsoes(
      DF                 = resultados_previsoes$data_plot,
      DF.anterior        = resultados_previsoes$data_plot_anterior,
      modelo             = resultados_previsoes$modelo_usado,
      variavel_analise   = tmp_variavel_analise,
      tmp_localidade     = label_br,
      tmp_fonte          = fonte_grafico_br,
      tmp_credito        = credito_grafico,
      tmp_y_label        = y_label_casos_confirmados,
      tmp_titulo         = titulo_total_casos_confirmados,
      tmp_legenda        = legenda_casos_confirmados,
      tmp_cor            = cores_graficos_confirmado_caso
    )
      
    
  })

}

# ************************************************************************* ####
# FIM                                                                       ####
# **************************************************************************** #