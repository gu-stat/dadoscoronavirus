# ************************************************************************* ----
# UI - Pagina Resumo                                                        ----
# ************************************************************************* ----

pagina_resumo <- fluidPage(
  id = "pagina-resumo",
  sidebarLayout(
    sidebarPanel(
      width = 2,
      # |_ Localidade Widget ===================================================
      uiOutput("selecao_localidade"),
      hr(),
      boxesModuleUI(id = "caixas-info"),
      hr(),
      # |_ Disclaimer Dados ====================================================
      uiOutput("ultima_observacao"),
      uiOutput("ultima_atualizacao")
    ),
    mainPanel(
      # |_ Painel Principal ====================================================
      id = "main-resumo",
      width = 10,
      # \__ Aviso --------------------------------------------------------------
      p(paste0("Se estiver em um desktop/laptop, passe o mouse sobre os ",
               "gráficos e mapa para obter mais informações.")),
      p(paste0("Se estiver em um dispositivo móvel, clique nos ",
               "gráficos e mapa para obter mais informações.")),
      HTML(paste0("Clique no icone ",icon("bars"), " à direita do título dos ",
               "gráficos e mapa para exportar a imagem.")),
      fluidRow(
        # \__ Graficos Esquerda ------------------------------------------------
        column(
          width = 6,
          # \\____ Mapa ####
          mapaModuleUI(id = "mapas"),
          # \\____ Mais Afetados ####
          graficoMAModuleUI(id = "grafico-mais-afetados")
        ),
        # \__ Graficos Direita -------------------------------------------------
        column(
          width = 6,
          # \\____ Numero Total ####
          graficoTAModuleUI(id = "grafico-total-acumulado"),
          # \\____ Casos Novos Por Dia ####
          graficoNCModuleUI(id = "grafico-novos-casos")
        )
      )
    )
  )
)

# ************************************************************************* ####
# FIM                                                                       ####
# **************************************************************************** #