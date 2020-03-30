# ************************************************************************* ----
# UI - Modulo                                                               ----
# ************************************************************************* ----

previsaoModuleUI <- function(id) {
  ns <- NS(id)
  
    mainPanel(
      # |_ Painel Principal ====================================================
      id = "main-previsao",
      width = 10,
      # \__ Tabela -------------------------------------------------------------
      fluidRow(
        box(
          width = 12,
          title = "Modelo AAN",
          highchartOutput(ns('previsao_aan')) %>% 
            withSpinner(type = 8)
        )
      ),
      fluidRow(
        box(
          width = 12,
          title = "Modelo MMN",
          highchartOutput(ns('previsao_mmn')) %>% 
            withSpinner(type = 8)
        )
      ),
      fluidRow(
        box(
          width = 12,
          title = "Modelo Selecionado Automaticamente",
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
  
  dias.a.frente <- 5
  
  # |_ Previsao AAN ============================================================
 
  output$previsao_aan = renderHighchart({
    
      #req(input$fonte_tabela)
    
      tmp_uf <- "Brasil"
      
      tmp_fonte <- "Dados: Brasil.io - https://brasil.io/dataset/covid19/caso"
    
      tmp <- dados_brasil %>%
        select(casos_confirmados) %>%
        as.zoo(order.by = dados_brasil$dia)
      
      # ANN Simple exponential smoothing with additive errors
      # MNN Simple exponential smoothing with MULTIPLICATIVE errors
      # MAN Additive Holt-Winters’ method with multiplicative errors 
      # AAN Additive Holt-Winters’ method with additive errors
      
      fit <- ets(tmp, model = "AAN")
      
      #fit <- ets(tmp, model = "ZZZ")
      
      #fit <- ets(tmp, model = "MMN")
      
      #fit <- holt(tmp, exponential=TRUE, biasadj = TRUE)
      
      #accuracy(fit)
  
      #checkresiduals(fit)
      
      forecast.fit <- forecast.ets(fit, h = 5, simulate = TRUE, boostrap = TRUE)
      
      tmp_data_observado <- dados_brasil %>%
        select(dia, casos_confirmados) %>%
        mutate(
          previsao = NA,
          lo.80    = NA,
          hi.80    = NA,
          lo.95    = NA,
          hi.95    = NA
        )
      
      tmp_data_previsao <- 
        data.frame(
          dia = c(data_final + days(1:dias.a.frente)),
          casos_confirmados = NA,
          previsao = forecast.fit$mean %>% c() %>% round(0),
          lo.80 = forecast.fit$lower[,1] %>% c() %>% round(0),
          hi.80 = forecast.fit$upper[,1] %>% c() %>% round(0),
          lo.95 = forecast.fit$lower[,2] %>% c() %>% round(0),
          hi.95 = forecast.fit$upper[,2] %>% c() %>% round(0)
        ) %>%
        as_tibble()
      
      tmp_data_plot <- rbind(tmp_data_observado, tmp_data_previsao)
      
      # \__ Plotar Grafico -----------------------------------------------------
      
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
        title = "Numero Total de Casos",
        floor = 0
      ) %>%
        # \\____ Casos Confirmados ####
      hc_add_series(
        data  = tmp_data_plot$casos_confirmados,
        type  = "line",
        color = cores_graficos_confirmado_caso,
        name  = "Casos Confirmados",
        legendIndex = 1
      ) %>%
        # \\____ Intervalos de Previsao - 95% ####
      hc_add_series(
        data  = tmp_data_plot,
        type  = "arearange",
        color = "#ef8a62",
        name  = "Intervalo de Predicao - 95%",
        hcaes(
          low  = lo.95,
          high = hi.95 
        ),
        legendIndex = 3
      ) %>%
      hc_add_series(
        data  = tmp_data_plot,
        type  = "arearange",
        color = "#fddbc7",
        name  = "Intervalo de Predicao - 80%",
        hcaes(
          low  = lo.80,
          high = hi.80 
        ),
        legendIndex = 3
      ) %>%
        # \\____ Previsao ####
      hc_add_series(
        data  = tmp_data_plot$previsao,
        type  = "line",
        color = "green",
        name  = "Previsao",
        legendIndex = 2
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
        text = paste0("Previsão: Número Total de Casos - ", tmp_uf),
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
  
  # |_ Previsao MMN ============================================================
  
  output$previsao_mmn = renderHighchart({
    
    #req(input$fonte_tabela)
    
    tmp_uf <- "Brasil"
    
    tmp_fonte <- "Dados: Brasil.io - https://brasil.io/dataset/covid19/caso"
    
    tmp <- dados_brasil %>%
      select(casos_confirmados) %>%
      as.zoo(order.by = dados_brasil$dia)
    
    # ANN Simple exponential smoothing with additive errors
    # MNN Simple exponential smoothing with MULTIPLICATIVE errors
    # MAN Additive Holt-Winters’ method with multiplicative errors 
    # AAN Additive Holt-Winters’ method with additive errors
    
    #fit <- ets(tmp, model = "AAN")
    
    #fit <- ets(tmp, model = "ZZZ")
    
    fit <- ets(tmp, model = "MMN")
    
    #fit <- holt(tmp, exponential=TRUE, biasadj = TRUE)
    
    #accuracy(fit)
    
    #checkresiduals(fit)
    
    forecast.fit <- forecast.ets(fit, h = 5, simulate = TRUE, boostrap = TRUE)
    
    tmp_data_observado <- dados_brasil %>%
      select(dia, casos_confirmados) %>%
      mutate(
        previsao = NA,
        lo.80    = NA,
        hi.80    = NA,
        lo.95    = NA,
        hi.95    = NA
      )
    
    tmp_data_previsao <- 
      data.frame(
        dia = c(data_final + days(1:dias.a.frente)),
        casos_confirmados = NA,
        previsao = forecast.fit$mean %>% c() %>% round(0),
        lo.80 = forecast.fit$lower[,1] %>% c() %>% round(0),
        hi.80 = forecast.fit$upper[,1] %>% c() %>% round(0),
        lo.95 = forecast.fit$lower[,2] %>% c() %>% round(0),
        hi.95 = forecast.fit$upper[,2] %>% c() %>% round(0)
      ) %>%
      as_tibble()
    
    tmp_data_plot <- rbind(tmp_data_observado, tmp_data_previsao)
    
    # \__ Plotar Grafico -----------------------------------------------------
    
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
      title = "Numero Total de Casos",
      floor = 0
    ) %>%
      # \\____ Casos Confirmados ####
    hc_add_series(
      data  = tmp_data_plot$casos_confirmados,
      type  = "line",
      color = cores_graficos_confirmado_caso,
      name  = "Casos Confirmados",
      legendIndex = 1
    ) %>%
      # \\____ Intervalos de Previsao - 95% ####
    hc_add_series(
      data  = tmp_data_plot,
      type  = "arearange",
      color = "#ef8a62",
      name  = "Intervalo de Predicao - 95%",
      hcaes(
        low  = lo.95,
        high = hi.95 
      ),
      legendIndex = 3
    ) %>%
      hc_add_series(
        data  = tmp_data_plot,
        type  = "arearange",
        color = "#fddbc7",
        name  = "Intervalo de Predicao - 80%",
        hcaes(
          low  = lo.80,
          high = hi.80 
        ),
        legendIndex = 3
      ) %>%
      # \\____ Previsao ####
    hc_add_series(
      data  = tmp_data_plot$previsao,
      type  = "line",
      color = "green",
      name  = "Previsao",
      legendIndex = 2
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
      text = paste0("Previsão: Número Total de Casos - ", tmp_uf),
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
  
  # |_ Previsao Automatico =====================================================
  
  output$previsao_auto = renderHighchart({
    
    #req(input$fonte_tabela)
    
    tmp_uf <- "Brasil"
    
    tmp_fonte <- "Dados: Brasil.io - https://brasil.io/dataset/covid19/caso"
    
    tmp <- dados_brasil %>%
      select(casos_confirmados) %>%
      as.zoo(order.by = dados_brasil$dia)
    
    # ANN Simple exponential smoothing with additive errors
    # MNN Simple exponential smoothing with MULTIPLICATIVE errors
    # MAN Additive Holt-Winters’ method with multiplicative errors 
    # AAN Additive Holt-Winters’ method with additive errors
    
    #fit <- ets(tmp, model = "AAN")
    
    fit <- ets(tmp, model = "ZZZ")
    
    #fit <- ets(tmp, model = "MMN")
    
    #fit <- holt(tmp, exponential=TRUE, biasadj = TRUE)
    
    #accuracy(fit)
    
    #checkresiduals(fit)
    
    forecast.fit <- forecast.ets(fit, h = 5, simulate = TRUE, boostrap = TRUE)
    
    tmp_data_observado <- dados_brasil %>%
      select(dia, casos_confirmados) %>%
      mutate(
        previsao = NA,
        lo.80    = NA,
        hi.80    = NA,
        lo.95    = NA,
        hi.95    = NA
      )
    
    tmp_data_previsao <- 
      data.frame(
        dia = c(data_final + days(1:dias.a.frente)),
        casos_confirmados = NA,
        previsao = forecast.fit$mean %>% c() %>% round(0),
        lo.80 = forecast.fit$lower[,1] %>% c() %>% round(0),
        hi.80 = forecast.fit$upper[,1] %>% c() %>% round(0),
        lo.95 = forecast.fit$lower[,2] %>% c() %>% round(0),
        hi.95 = forecast.fit$upper[,2] %>% c() %>% round(0)
      ) %>%
      as_tibble()
    
    tmp_data_plot <- rbind(tmp_data_observado, tmp_data_previsao)
    
    # \__ Plotar Grafico -----------------------------------------------------
    
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
      title = "Numero Total de Casos",
      floor = 0
    ) %>%
      # \\____ Casos Confirmados ####
    hc_add_series(
      data  = tmp_data_plot$casos_confirmados,
      type  = "line",
      color = cores_graficos_confirmado_caso,
      name  = "Casos Confirmados",
      legendIndex = 1
    ) %>%
      # \\____ Intervalos de Previsao - 95% ####
    hc_add_series(
      data  = tmp_data_plot,
      type  = "arearange",
      color = "#ef8a62",
      name  = "Intervalo de Predicao - 95%",
      hcaes(
        low  = lo.95,
        high = hi.95 
      ),
      legendIndex = 3
    ) %>%
      hc_add_series(
        data  = tmp_data_plot,
        type  = "arearange",
        color = "#fddbc7",
        name  = "Intervalo de Predicao - 80%",
        hcaes(
          low  = lo.80,
          high = hi.80 
        ),
        legendIndex = 3
      ) %>%
      # \\____ Previsao ####
    hc_add_series(
      data  = tmp_data_plot$previsao,
      type  = "line",
      color = "green",
      name  = "Previsao",
      legendIndex = 2
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
      text = paste0("Previsão: Número Total de Casos - ", tmp_uf),
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