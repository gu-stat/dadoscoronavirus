# ************************************************************************* ----
# Server                                                                    ----
# ************************************************************************* ----

function(input, output, session) {
  
  # |_ Dados ===================================================================
  
  dados <- reactive({
    
    req(input$localidade)
    
    if (input$localidade == "Brasil") {
      tmp_casos <- dados_brasil
      
    } else {
      tmp_casos <- 
        dados_estados %>%
        filter(uf_num == input$localidade)
    }
    
    tmp_casos
  })
  
  # |_ Inputs ==================================================================
  
  # \__ Selecao de Localidade --------------------------------------------------
  
  output$selecao_localidade <- renderUI({
    selectInput(
      inputId = "localidade",
      label = h4("Localidade:"),
      choices = c(
        "Brasil",
        with(
          dados_selecionados_cidade, 
          split(
            setNames(
              dados_selecionados_cidade$uf_num, 
              dados_selecionados_cidade$uf_nome
            ), 
            regiao
          )
        )
      ),
      multiple = FALSE,
      selectize = TRUE
    )
  })
  
  # Outputs ====================================================================
  
  # \__ Datas ------------------------------------------------------------------
  
  # \\____ Ultima Observacao ####
  
  output$ultima_observacao <- renderUI({
    h5(paste0("Dados observados até ", data_final_br))
  })
  
  # \\____ Ultima Atualizacao ####
  
  output$ultima_atualizacao <- renderUI({
    h5(paste0("Dados atualizados em ", data_atualizacao_br))
  })
  
  # |_ Modulos =================================================================
  
  # \__ Painel - Resumo --------------------------------------------------------
  
  # \\____ Boxes Casos ####
  callModule(
    module        = boxesModule, 
    id            = "caixas-info", 
    dados_analise = dados
  )
  # \\____ Mapa ####
  callModule(
    module = mapaModule, 
    id     = "mapas", 
    local  = reactive(input$localidade)
  )
  # \\____ Grafico - Mais Afetados ####
  callModule(
    module        = graficoMAModule, 
    id            = "grafico-mais-afetados", 
    dados_analise = dados, 
    local         = reactive(input$localidade)
  )
  # \\____ Grafico - Numero Total (Acumulado) ####
  callModule(
    module        = graficoTAModule, 
    id            = "grafico-total-acumulado", 
    dados_analise = dados, 
    local         = reactive(input$localidade)
  )
  # \\____ Grafico - Casos Novos por Dia ####
  callModule(
    module        = graficoNCModule, 
    id            = "grafico-novos-casos", 
    dados_analise = dados, 
    local         = reactive(input$localidade)
  )

  # \__ Painel - Previsao ------------------------------------------------------
  
  callModule(
    module        = previsaoModule,
    id            = "previsao"
  )
  
  # \__ Painel - Dados Brutos --------------------------------------------------
  
  callModule(
    module        = dadosModule,
    id            = "dados-brutos"
  )
  
  # \__ Painel - Fontes --------------------------------------------------------
  
  callModule(
    module        = fontesModule,
    id            = "fontes"
  )
  
}

# ************************************************************************* ####
# FIM                                                                       ####
# **************************************************************************** #
