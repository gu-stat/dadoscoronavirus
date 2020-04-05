# ************************************************************************* ----
# Server                                                                    ----
# ************************************************************************* ----

function(input, output, session) {
  
  # |_ Dados ===================================================================
  
  # \___ Dados Municipio -------------------------------------------------------
  
  dados_selecionados_cidade <- reactive({
    dados_originais_br_io() %>%
      tibble::as_tibble() %>%
      rename(
        "uf"                 = "state",
        "municipio"          = "city",
        "cod_municipio"      = "city_ibge_code"
      ) %>%
      mutate(
        dia = as.Date(date),
        uf_num = substr(cod_municipio, 1, 2),
        casos_confirmados  = as.numeric(confirmed),
        mortes_confirmadas = as.numeric(deaths),
        confirmed_per_100k_inhabitants = round(
          as.numeric(confirmed_per_100k_inhabitants), 2
        )
      ) %>%
      mutate(
        uf_nome = case_when(
          uf_num == 11 ~ 'Rondônia',
          uf_num == 12 ~ 'Acre',
          uf_num == 13 ~ 'Amazonas',
          uf_num == 14 ~ 'Roraima',
          uf_num == 15 ~ 'Pará',
          uf_num == 16 ~ 'Amapá',
          uf_num == 17 ~ 'Tocantins',
          uf_num == 21 ~ 'Maranhão',
          uf_num == 22 ~ 'Piauí',
          uf_num == 23 ~ 'Ceará',
          uf_num == 24 ~ 'Rio Grande do Norte',
          uf_num == 25 ~ 'Paraíba',
          uf_num == 26 ~ 'Pernambuco',
          uf_num == 27 ~ 'Alagoas',
          uf_num == 28 ~ 'Sergipe',
          uf_num == 29 ~ 'Bahia',
          uf_num == 31 ~ 'Minas Gerais',
          uf_num == 32 ~ 'Espírito Santo',
          uf_num == 33 ~ 'Rio de Janeiro',
          uf_num == 35 ~ 'São Paulo',
          uf_num == 41 ~ 'Paraná',
          uf_num == 42 ~ 'Santa Catarina',
          uf_num == 43 ~ 'Rio Grande do Sul',
          uf_num == 50 ~ 'Mato Grosso do Sul',
          uf_num == 51 ~ 'Mato Grosso',
          uf_num == 52 ~ 'Goiás',
          uf_num == 53 ~ 'Distrito Federal'
        ),
        regiao = case_when(
          uf_num == 11 ~ 'Norte',
          uf_num == 12 ~ 'Norte',
          uf_num == 13 ~ 'Norte',
          uf_num == 14 ~ 'Norte',
          uf_num == 15 ~ 'Norte',
          uf_num == 16 ~ 'Norte',
          uf_num == 17 ~ 'Norte',
          uf_num == 21 ~ 'Nordeste',
          uf_num == 22 ~ 'Nordeste',
          uf_num == 23 ~ 'Nordeste',
          uf_num == 24 ~ 'Nordeste',
          uf_num == 25 ~ 'Nordeste',
          uf_num == 26 ~ 'Nordeste',
          uf_num == 27 ~ 'Nordeste',
          uf_num == 28 ~ 'Nordeste',
          uf_num == 29 ~ 'Nordeste',
          uf_num == 31 ~ 'Sudeste',
          uf_num == 32 ~ 'Sudeste',
          uf_num == 33 ~ 'Sudeste',
          uf_num == 35 ~ 'Sudeste',
          uf_num == 41 ~ 'Sul',
          uf_num == 42 ~ 'Sul',
          uf_num == 43 ~ 'Sul',
          uf_num == 50 ~ 'Centro-Oeste',
          uf_num == 51 ~ 'Centro-Oeste',
          uf_num == 52 ~ 'Centro-Oeste',
          uf_num == 53 ~ 'Centro-Oeste'
        )
      ) %>%
      select(
        dia,
        is_last,
        regiao,
        uf_nome,
        uf,
        uf_num,
        municipio,
        cod_municipio,
        place_type,
        casos_confirmados,
        mortes_confirmadas,
        confirmed_per_100k_inhabitants,
        estimated_population_2019
      ) %>%
      arrange(uf_num, cod_municipio, dia) %>%
    # Ajustes Mortes CE, dia 04/04 
      # Fonte: https://dev.org.br/api/casos-ceara-por-dia
      mutate(
        mortes_confirmadas = case_when(
          is_last == "True" & cod_municipio == 2304400 & mortes_confirmadas == 0 ~ 17,
          is_last == "True" & cod_municipio == 2304285 & mortes_confirmadas == 0 ~ 1,
          is_last == "True" & cod_municipio == 2312205 & mortes_confirmadas == 0 ~ 1,
          is_last == "True" & cod_municipio == 2306900 & mortes_confirmadas == 0 ~ 1,
          is_last == "True" & cod_municipio == 2313401 & mortes_confirmadas == 0 ~ 1,
          TRUE ~ mortes_confirmadas
          
        )
      )
  })

  # \___ Dados Estados ---------------------------------------------------------

  dados_estados <- reactive({
    dados_selecionados_cidade() %>%
      filter(place_type == "state") %>%
      mutate(
        confirmados_dia = fcn_dia_uf(., variavel = "casos_confirmados"),
        mortes_dia      = fcn_dia_uf(., variavel = "mortes_confirmadas"),
        confirmed_per_100k_inhabitants = prettyNum(
          round(confirmed_per_100k_inhabitants, 2),
          big.mark = ".",
          decimal.mark = ","
        )
      )
  })

  # \___ Dados Brasil ----------------------------------------------------------

  dados_brasil <- reactive({

    populacao_2019 <-
      dados_estados() %>%
      group_by(dia, uf) %>%
      summarise(
        estimated_population_2019 = mean(estimated_population_2019)
      ) %>%
      ungroup() %>%
      distinct(estimated_population_2019) %>%
      sum()

    dados_estados() %>%
      group_by(dia) %>%
      summarise(
        confirmados_dia      = sum(confirmados_dia, na.rm = TRUE),
        mortes_dia           = sum(mortes_dia, na.rm = TRUE)
      ) %>%
      mutate(
        casos_confirmados  = cumsum(confirmados_dia),
        mortes_confirmadas = cumsum(mortes_dia),
        confirmed_per_100k_inhabitants = casos_confirmados/populacao_2019*100000,
        confirmed_per_100k_inhabitants = prettyNum(
          round(confirmed_per_100k_inhabitants, 2),
          big.mark = ".",
          decimal.mark = ","
        )
      )
  })


  dados <- reactive({

    req(input$localidade)

    if (input$localidade == "Brasil") {
      tmp_casos <- dados_brasil()

    } else {
      tmp_casos <-
        dados_estados() %>%
        filter(uf_num == input$localidade)
    }

    tmp_casos
  })

  # |_ Inputs ==================================================================

  # \__ Selecao de Localidade --------------------------------------------------

  output$selecao_localidade <- renderUI({
    
    tmp_locais <-
      dados_selecionados_cidade() %>%
      select(regiao, uf_nome, uf_num) %>%
      distinct() %>%
      arrange(regiao, uf_nome, uf_num)
    
    selectInput(
      inputId = "localidade",
      label = h4("Localidade:"),
      choices = c(
        "Brasil",
        with(
          tmp_locais,
          split(
            setNames(
              tmp_locais$uf_num,
              tmp_locais$uf_nome
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
  
  # \___ Datas das Observacoes ---------------------------------------------------
  
  data_range <- reactive({
    dados_selecionados_cidade() %>%
      select(dia) %>%
      distinct() %>%
      arrange(desc(dia))
  })


  #data_comeco <- ymd(data_range$dia[dim(data_range)[1]])

  data_final  <- reactive(ymd(data_range()$dia[1]))

  #data_comeco_br <- format(as.Date(data_comeco), "%d/%m/%Y")

  data_final_br  <- reactive(format(as.Date(data_final()), "%d/%m/%Y"))

  # \\____ Ultima Observacao ####

  output$ultima_observacao <- renderUI({
    h5(paste0("Dados observados até ", data_final_br()))
  })

  # \\____ Ultima Atualizacao ####

  output$ultima_atualizacao <- renderUI({
    
    data_atualizacao_br  <- 
      read_html("https://brasil.io/dataset/covid19/caso") %>%
      html_nodes(., "p") %>%
      str_subset(., "Importação dos dados feita em") %>%
      str_remove(., "Dados capturados em 01 de Abril de 2020.") %>%
      str_remove(., "Importação dos dados feita em ") %>%
      str_remove(., "<p>") %>%
      str_remove(., "</p>") %>%
      str_replace_all(., "[\r\n]" , "") %>%
      str_squish()
    
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
    local  = reactive(input$localidade),
    dados_estados = dados_estados,
    dados_selecionados_cidade = dados_selecionados_cidade,
    data_final = data_final
  )
  # \\____ Grafico - Mais Afetados ####
  callModule(
    module        = graficoMAModule,
    id            = "grafico-mais-afetados",
    dados_analise = dados,
    local         = reactive(input$localidade),
    dados_estados = dados_estados,
    dados_selecionados_cidade = dados_selecionados_cidade
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
    id            = "previsao",
    dados_brasil  = dados_brasil,
    data_final    = data_final
  )
  
  # \__ Painel - Dados Brutos --------------------------------------------------

  callModule(
    module        = dadosModule,
    id            = "dados-brutos",
    dados_brasil  = dados_brasil,
    dados_estados = dados_estados,
    dados_selecionados_cidade = dados_selecionados_cidade,
    dados_originais_br_io = dados_originais_br_io
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
