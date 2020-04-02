# ************************************************************************* ----
# Funcao para Plotar os Graficos das Previsoes                              ----
# ************************************************************************* ----

fcn_grafico_previsoes <- function(DF,
                                  DF.anterior,
                                  variavel_analise, 
                                  modelo, 
                                  tmp_localidade,
                                  tmp_fonte,
                                  tmp_credito,
                                  tmp_y_label,
                                  tmp_titulo,
                                  tmp_legenda,
                                  tmp_cor){
  
  tmp_data_plot <- DF
  tmp_data_plot_anterior <- DF.anterior
  # \__ Plotar Grafico ---------------------------------------------------------
  
  if (!is.null(tmp_data_plot_anterior)) {
    tmp_graf_previsao <- 
      highchart() %>%
      # \\____ Info na Tooltip ####
      hc_tooltip(crosshairs = TRUE, shared = TRUE) %>%
        # \\____ Zoom e Espacamento ####
      # Espacamento necessario para mostrar Fonte Dados e Credito Criacao
      hc_chart(zoomType = "xy", spacingBottom = 23) %>%
        # \\____ Incluir Dia no Eixo X ####
      hc_xAxis(categories = format(as.Date(tmp_data_plot$dia), "%d/%m")) %>%
        # \\____ Incluir Escala no Eixo Y ####
      hc_yAxis(
        title = tmp_y_label,
        floor = 0
      ) %>%
        # \\____ Anteriores Intervalos de Previsao - 95% ####
      hc_add_series(
        data  = tmp_data_plot_anterior,
        type  = "arearange",
        color = cores_previsao_95_anterior,
        name  = "Intervalo de Previsão - 95%, Dia 31/03",
        hcaes(
          low  = lo.95,
          high = hi.95
        ),
        legendIndex = 8,
        marker = list(enable = FALSE)
      ) %>%
        # \\____ Anteriores Intervalos de Previsao - 80% ####
      hc_add_series(
        data  = tmp_data_plot_anterior,
        type  = "arearange",
        color = cores_previsao_80_anterior,
        name  = "Intervalo de Previsão - 80%, Dia 31/03",
        hcaes(
          low  = lo.80,
          high = hi.80
        ),
        legendIndex = 7
      ) %>%
      # \\____ Anterior Previsao ####
      hc_add_series(
        data        = tmp_data_plot_anterior[, "previsao"],
        type        = "line",
        color       = cores_previsao_est_anterior,
        name        = "Previsão, Dia 31/03",
        legendIndex = 6
      )%>%  
      # \\____ Valores Observados ####
      hc_add_series(
        data  = c(tmp_data_plot[, variavel_analise])[[1]],
        type  = "line",
        color = tmp_cor,
        name  = tmp_legenda,
        legendIndex = 1
      ) %>%
        # \\____ Novos Intervalos de Previsao - 95% ####
      hc_add_series(
        data  = tmp_data_plot,
        type  = "arearange",
        color = cores_previsao_95,
        name  = "Intervalo de Previsão - 95%",
        hcaes(
          low  = lo.95,
          high = hi.95 
        ),
        legendIndex = 3
      ) %>%
        # \\____ Novos Intervalos de Previsao - 80% ####
      hc_add_series(
        data  = tmp_data_plot,
        type  = "arearange",
        color = cores_previsao_80,
        name  = "Intervalo de Previsão - 80%",
        hcaes(
          low  = lo.80,
          high = hi.80 
        ),
        legendIndex = 4
      ) %>%
        # \\____ Nova Previsao ####
      hc_add_series(
        data        = c(tmp_data_plot[, "previsao"])[[1]],
        type        = "line",
        color       = cores_previsao_est,
        name        = "Previsão",
        legendIndex = 2
      )
  } else {
    tmp_graf_previsao <- 
      highchart() %>%
      # \\____ Info na Tooltip ####
      hc_tooltip(crosshairs = TRUE, shared = TRUE) %>%
        # \\____ Zoom e Espacamento ####
      # Espacamento necessario para mostrar Fonte Dados e Credito Criacao
      hc_chart(zoomType = "xy", spacingBottom = 23) %>%
        # \\____ Incluir Dia no Eixo X ####
      hc_xAxis(categories = format(as.Date(tmp_data_plot$dia), "%d/%m")) %>%
        # \\____ Incluir Escala no Eixo Y ####
      hc_yAxis(
        title = tmp_y_label,
        floor = 0
      ) %>%  # \\____ Valores Observados ####
      hc_add_series(
        data  = c(tmp_data_plot[, variavel_analise])[[1]],
        type  = "line",
        color = tmp_cor,
        name  = tmp_legenda,
        legendIndex = 1
      ) %>%
        # \\____ Novos Intervalos de Previsao - 95% ####
      hc_add_series(
        data  = tmp_data_plot,
        type  = "arearange",
        color = cores_previsao_95,
        name  = "Intervalo de Previsão - 95%",
        hcaes(
          low  = lo.95,
          high = hi.95 
        ),
        legendIndex = 3
      ) %>%
        # \\____ Novos Intervalos de Previsao - 80% ####
      hc_add_series(
        data  = tmp_data_plot,
        type  = "arearange",
        color = cores_previsao_80,
        name  = "Intervalo de Previsão - 80%",
        hcaes(
          low  = lo.80,
          high = hi.80 
        ),
        legendIndex = 4
      ) %>%
        # \\____ Nova Previsao ####
      hc_add_series(
        data        = c(tmp_data_plot[, "previsao"])[[1]],
        type        = "line",
        color       = cores_previsao_est,
        name        = "Previsão",
        legendIndex = 2
      )
  }
  

  tmp_graf_previsao %>%
      # \\____ Legenda ####
    hc_legend(
      align         = "center",
      verticalAlign = "bottom",
      layout        = "horizontal",
      itemDistance  = 50
    ) %>%
      # \\____ Titulo ####
    hc_title(
      text = paste0("Modelo ", modelo,
                    ", Previsão: ", tmp_titulo, " - ", tmp_localidade),
      align ="left",
      style = list(fontSize= "16px")
    ) %>%
      # \\____ Fonte Dados ####
    hc_subtitle(
      text          = tmp_fonte,
      verticalAlign = "bottom",
      align         = "right",
      style         = list(fontSize= "9px")
    ) %>%
      # \\____ Credito Criacao ####
    hc_credits(
      enabled  = TRUE,
      text     = tmp_credito,
      position = list(align = "right", y = -2)
      
    ) %>%
      # \\____ Botao Exportar Imagem ####
    hc_exporting(enabled = TRUE)
  
}