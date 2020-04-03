# ************************************************************************* ----
# UI - Modulo                                                               ----
# ************************************************************************* ----

mapaModuleUI <- function(id) {
  ns <- NS(id)
  
  # fluidRow(
  #   id = id,
    box(
      width = 12,
      highchartOutput(ns("mapa_acumulado")) %>% withSpinner(type = 8),
      uiOutput(ns("variavel_mapa"))
    )
  #)
}

# ************************************************************************* ----
# Server - Modulo                                                           ----
# ************************************************************************* ----

mapaModule <- function(input, output, session, local){
  
  ns <- session$ns
  
  # |_ Variavel de Analise =====================================================
  
  output$variavel_mapa <- renderUI({
    awesomeRadio(
      inputId  = ns("variavel_mapa"),
      label    = "Variável",
      choices  = list(
        "Casos Confirmados" = "casos_confirmados_mapa",
        "Mortes Confirmadas" = "mortes_confirmadas_mapa"),
      selected = "casos_confirmados_mapa",
      inline   = TRUE,
      status   = "danger",
      checkbox = TRUE
    )
  })
  
  # |_ Mapa - JSON =============================================================
  
  mapa_json <- reactive({
    
    # \__ Brasil -------------------------------------------------------------
    
    if (local() == "Brasil") {
      
      mapa_sf <- get_brmap_fixed(
        geo = "State", 
        class = "SpatialPolygonsDataFrame"
      )
      
    } else {
      
      # \__ Estados/Municipio --------------------------------------------------  
      
      mapa_sf <- get_brmap_fixed(
        geo        = "City",
        geo.filter = list(State = local()),
        class      = "SpatialPolygonsDataFrame"
      )
      
    }
    
    # \__ Formato para Highcharter -------------------------------------------
    
    mapa_geojson <- geojson_json(mapa_sf)
    
    tmp_hcmap <- jsonlite::fromJSON(mapa_geojson, simplifyVector = FALSE)
    
    tmp_hcmap
    
  })
  
  # |_ Mapa - Dados ==========================================================
  
  mapa_dados <- reactive({
    
    req(input$variavel_mapa)
    
    # \__ Brasil -------------------------------------------------------------
    
    if (local() == "Brasil") {
      
      if (input$variavel_mapa == "casos_confirmados_mapa") {
        
        # \\____ Casos Confirmados ####
        
        tmp_map <-
          dados_estados %>%
          filter(is_last == "True") %>%
          select(
            uf_num, uf, uf_nome, casos_confirmados, 
            confirmed_per_100k_inhabitants
          ) %>%
          rename(
            "variavel_analise_mapa" = casos_confirmados
          )
        
        tmp_tooltip <- "{point.uf}: {point.value:,.0f} </br> Casos por 100 Mil Habitantes: {point.confirmed_per_100k_inhabitants}"
        
        tmp_min_mapa <- min(tmp_map$variavel_analise_mapa)
        
        tmp_max_mapa <- max(tmp_map$variavel_analise_mapa)
        
        if (tmp_max_mapa == 0) {
          tmp_max_color_mapa <- "#f7f7f7"
        } else {
          tmp_max_color_mapa <- cores_mapa_confirmado_caso
        }
        
        tmp_min_color_mapa <- "#f7f7f7"
        
        tmp_name_mapa <- "Total de Casos Confirmados"
        
        tmp_data_final  <- ymd(max(data_final))
        
        tmp_data_final_br  <- format(as.Date(tmp_data_final), "%d/%m/%Y")
        
        tmp_titulo_mapa <- paste0(
          "Número Total de Casos Confirmados até ", tmp_data_final_br," - Brasil"
        )
        
        # tmp_fonte_mapa <- 
        #   "Dados: https://github.com/belisards/coronabr"
        
        tmp_fonte_mapa <- 
          "Dados: Brasil.io - https://brasil.io/dataset/covid19/caso"
        
      } else {
        
        # \\____ Mortes Confirmadas ####
        
        tmp_map <-
          dados_estados %>%
          filter(is_last == "True") %>%
          select(uf_num, uf, uf_nome, mortes_confirmadas) %>%
          rename(
            "variavel_analise_mapa" = mortes_confirmadas
          )
        
        tmp_tooltip <- "{point.uf}: {point.value}"

        tmp_min_mapa <- min(tmp_map$variavel_analise_mapa, na.rm = TRUE)
        
        tmp_max_mapa <- max(tmp_map$variavel_analise_mapa, na.rm = TRUE)
        
        if (tmp_max_mapa == 0) {
          tmp_max_color_mapa <- "#f7f7f7"
        } else {
          tmp_max_color_mapa <- cores_mapa_confirmado_morte
        }
        
        tmp_min_color_mapa <- "white"
        
        tmp_name_mapa <- "Total de Mortes Confirmadas"
        
        tmp_data_final  <- ymd(max(data_final))
        
        tmp_data_final_br  <- format(as.Date(tmp_data_final), "%d/%m/%Y")
        
        tmp_titulo_mapa <- paste0(
          "Número Total de Mortes Confirmadas até ", tmp_data_final_br," - Brasil"
        )
        
        # tmp_fonte_mapa <- 
        #   "Dados: https://github.com/belisards/coronabr"
        
        tmp_fonte_mapa <- 
          "Dados: Brasil.io - https://brasil.io/dataset/covid19/caso"
        
      }
      
    } else {
      
      # \__ Estados/Municipio -------------------------------------------------- 
      
      # \\____ Casos Confirmados ####
      if (input$variavel_mapa == "casos_confirmados_mapa") {
        
        tmp_map <-
          dados_selecionados_cidade %>%
          filter(place_type == "city") %>%
          filter(uf_num == local()) %>%
          filter(is_last == "True") %>%
          rename(
            "variavel_analise_mapa" = casos_confirmados
          ) %>%
          mutate(
            confirmed_per_100k_inhabitants = prettyNum(
              confirmed_per_100k_inhabitants,
              big.mark = ".", 
              decimal.mark = ","
            )
          )
        
        tmp_tooltip <- "{point.municipio}: {point.value} </br> Casos por 100 Mil Habitantes: {point.confirmed_per_100k_inhabitants}"
        
        tmp_min_mapa <- min(tmp_map$variavel_analise_mapa)
        
        tmp_max_mapa <- max(tmp_map$variavel_analise_mapa)
        
        if (tmp_max_mapa == 0) {
          tmp_max_color_mapa <- "#f7f7f7"
        } else {
          tmp_max_color_mapa <- cores_mapa_confirmado_caso
        }
        
        # if (tmp_min_mapa == 0) {
        #   tmp_min_color_mapa <- "#f7f7f7"
        # } else {
        #   tmp_min_color_mapa <- cores_mapa_confirmado_caso_min
        # }
        
        tmp_min_color_mapa <- cores_mapa_confirmado_caso_min
        
        tmp_name_mapa <- "Total de Casos Confirmados"
        
        tmp_data_final  <- ymd(min(tmp_map$dia))
        
        tmp_data_final_mun  <- format(as.Date(tmp_data_final), "%d/%m/%Y")
        
        tmp_titulo_mapa <- paste0(
          "Número Total de Casos Confirmados até ", tmp_data_final_mun," - ",
          tmp_map %>% select(uf) %>% distinct() %>% unlist()
        )
        
        tmp_fonte_mapa <- 
          "Dados: Brasil.io - https://brasil.io/dataset/covid19/caso"
        
      } else {
        
        # \\____ Mortes Confirmadas ####
        
        tmp_map <-
          dados_selecionados_cidade %>%
          filter(place_type == "city") %>%
          filter(uf_num == local()) %>%
          filter(is_last == "True") %>%
          rename(
            "variavel_analise_mapa" = mortes_confirmadas
          )
        
        tmp_tooltip <- "{point.municipio}: {point.value}"
        
        tmp_min_mapa <- min(tmp_map$variavel_analise_mapa)
        
        tmp_max_mapa <- max(tmp_map$variavel_analise_mapa)
        
        if (tmp_max_mapa == 0) {
          tmp_max_color_mapa <- "#f7f7f7"
        } else {
          tmp_max_color_mapa <- cores_mapa_confirmado_morte
        }
        
        # if (tmp_min_mapa == 0) {
        #   tmp_min_color_mapa <- "#f7f7f7"
        # } else {
        #   tmp_min_color_mapa <- cores_mapa_confirmado_morte_min
        # }
        
        
        tmp_min_color_mapa <- cores_mapa_confirmado_morte_min
        
        tmp_name_mapa <- "Total de Mortes Confirmadas"
        
        tmp_data_final  <- ymd(min(tmp_map$dia))
        
        tmp_data_final_mun  <- format(as.Date(tmp_data_final), "%d/%m/%Y")
        
        tmp_titulo_mapa <- paste0(
          "Número Total de Mortes Confirmadas até ", tmp_data_final_mun," - ",
          tmp_map %>% select(uf) %>% distinct() %>% unlist()
        )
        
        tmp_fonte_mapa <- 
          "Dados: Brasil.io - https://brasil.io/dataset/covid19/caso"
        
      }
      
    }
    
    # \__ Retornar Lista -----------------------------------------------------
    
    return(list(
      tmp_map            = tmp_map, 
      tmp_min_mapa       = tmp_min_mapa, 
      tmp_max_mapa       = tmp_max_mapa, 
      tmp_min_color_mapa = tmp_min_color_mapa, 
      tmp_max_color_mapa = tmp_max_color_mapa, 
      tmp_name_mapa      = tmp_name_mapa, 
      tmp_titulo_mapa    = tmp_titulo_mapa,
      tmp_fonte_mapa     = tmp_fonte_mapa,
      tmp_tooltip        = tmp_tooltip
    ))
    
  })
  
  # |_ Mapa ------------------------------------------------------------------
  
  output$mapa_acumulado <- renderHighchart({
    
    req(input$variavel_mapa)
    
    if (local() == "Brasil") {
      # \__ Brasil -----------------------------------------------------------
      
      tmp_hc <- 
        highchart() %>%
        hc_add_series_map(
          map         = mapa_json(),
          df          = mapa_dados()$tmp_map,
          value       = "variavel_analise_mapa",
          joinBy      = c("State", "uf_num"),
          name        = mapa_dados()$tmp_name_map,
          dataLabels  = list(enabled = FALSE, format = '{point.uf}'),
          borderColor = "#8c8c8c",
          borderWidth = 0.2
        ) %>%
        # \\____ Info na Tooltip ####
      hc_tooltip(
        useHTML = TRUE,
        #header = "{point.uf}",
        #pointFormat = "{point.uf}: {point.value:} </br> Casos por 100 Mil Habitantes: {point.confirmed_per_100k_inhabitants}"
        pointFormat = mapa_dados()$tmp_tooltip
      )
      
    } else {
      
      # \__ Estados/Municipio ------------------------------------------------
      
      tmp_hc <- 
        highchart() %>%
        hc_add_series_map(
          map         = mapa_json(),
          df          = mapa_dados()$tmp_map,
          value       = "variavel_analise_mapa",
          joinBy      = c("City", "cod_municipio"),
          name        = mapa_dados()$tmp_name_mapa,
          dataLabels  = list(enabled = FALSE),
          borderColor = "#8c8c8c",
          borderWidth = 0.2
        ) %>%
        # \\____ Info na Tooltip ####
      hc_tooltip(
        useHTML = TRUE,
        pointFormat = mapa_dados()$tmp_tooltip
      )
    }
    
    # \__ Plotar Mapa --------------------------------------------------------
    
    tmp_hc %>%
      # \\____ Legenda ####
    hc_legend(
      align         = "center",
      verticalAlign = "bottom",
      layout        = "horizontal",
      itemStyle     = list(fontSize = "9px")
    ) %>%
      # \\____ Cores ####
    hc_colorAxis(
      min = mapa_dados()$tmp_min_mapa,
      max = mapa_dados()$tmp_max_mapa,
      minColor = mapa_dados()$tmp_min_color_mapa,
      maxColor = mapa_dados()$tmp_max_color_mapa
    ) %>%
      # \\____ Titulo ####
    hc_title(
      text = mapa_dados()$tmp_titulo_mapa,
      align ="left",
      style = list(fontSize= "16px")
    ) %>%
      # \\____ Fonte Dados ####
    hc_subtitle(
      text = mapa_dados()$tmp_fonte_mapa,
      verticalAlign = "bottom",
      align  = "right",
      margin = 25,
      style  = list(fontSize= "9px")
    ) %>%
      # \\____ Credito Criacao ####
    hc_credits(
      enabled = TRUE,
      text = "Mapa: Gustavo Varela-Alvarenga - ogustavo.com",
      position = list(align = "right", y = -15)
      
    ) %>%
      # \\____ Botao Exportar Imagem ####
    hc_exporting(enabled = TRUE)
    
  })
}

# ************************************************************************* ####
# FIM                                                                       ####
# **************************************************************************** #