# ************************************************************************* ----
# UI - Pagina Resumo                                                        ----
# ************************************************************************* ----

pagina_resumo <- fluidPage(
  id = "pagina-resumo",
  hr(),
  # \__ Aviso --------------------------------------------------------------
  h6(paste0("Se estiver em um desktop/laptop, passe o mouse sobre os ",
            "gráficos e mapa para obter mais informações. ",
            "Se estiver em um dispositivo móvel, clique nos ",
            "gráficos e mapa para obter mais informações.")),
  h6(HTML(paste0("Clique no icone ",icon("bars"), " à direita do título dos ",
                 "gráficos e mapa para exportar a imagem."))),
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
      uiOutput("ultima_atualizacao"),
      p(paste0("Fonte: Secretarias de Saúde das Unidades Federativas, dados ",
               "tratados por Álvaro Justen e colaboradores /"),
        a("Brasil.IO", href = "https://brasil.io/"))
    ),
    mainPanel(
      # |_ Painel Principal ====================================================
      id = "main-resumo",
      width = 10,
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