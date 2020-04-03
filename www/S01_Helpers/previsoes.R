# ************************************************************************* ----
# Funcao para Realizar as Previsoes                                         ----
# ************************************************************************* ----

fcn_previsoes <- function(DF,
                          variavel_analise, 
                          modelo, 
                          horizonte_previsao,
                          exporta_dados = FALSE,
                          importa_dados = FALSE,
                          data_previsao_anterior,
                          ...){

  # \__ Novas Previsoes ------------------------------------------------------
  
  tmp <- 
    DF %>%
    select(!!variavel_analise) %>%
    as.zoo(order.by = DF$dia)
  
  # ANN Simple exponential smoothing with additive errors
  # MNN Simple exponential smoothing with MULTIPLICATIVE errors
  # MAN Additive Holt-Winters’ method with multiplicative errors 
  # AAN Additive Holt-Winters’ method with additive errors
  
  fit <- ets(tmp, model = modelo)
  
  #fit <- holt(tmp, exponential=TRUE, biasadj = TRUE)
  
  #accuracy(fit)
  
  #checkresiduals(fit)
  
  forecast.fit <- forecast.ets(
    object   = fit, 
    h        = horizonte_previsao, 
    simulate = TRUE, 
    boostrap = TRUE
  )
  
  tmp_data_observado <- 
    DF %>%
    select(dia, !!variavel_analise) %>%
    mutate(
      data_da_previsao = data_final,
      previsao = NA,
      lo.80    = NA,
      hi.80    = NA,
      lo.95    = NA,
      hi.95    = NA
    )
  
  tmp_data_previsao <- 
    tibble(
      dia = c(data_final + days(1:horizonte_previsao)),
      data_da_previsao = data_final,
      !!variavel_analise := NA,
      previsao = forecast.fit$mean %>% c() %>% round(0),
      lo.80 = forecast.fit$lower[,1] %>% c() %>% round(0),
      hi.80 = forecast.fit$upper[,1] %>% c() %>% round(0),
      lo.95 = forecast.fit$lower[,2] %>% c() %>% round(0),
      hi.95 = forecast.fit$upper[,2] %>% c() %>% round(0)
    ) 
  
  tmp_data_plot <- rbind(tmp_data_observado, tmp_data_previsao)
  
  # \__ Modelo Utilizado -----------------------------------------------------
  
  tmp_modelo <- fit$method
  
  if (modelo == "ZZZ"){
    tmp_modelo_arquivo <- "ZZZ_"
  } else if(modelo == "MMN"){
    tmp_modelo_arquivo <- "MMN_"
  } else {
    tmp_modelo_arquivo <- gsub("[\\(+\\)]","_", gsub(",","_", tmp_modelo))
  }
  
  
  # \__ Exportar Novas Previsoes ---------------------------------------------
  
  if (exporta_dados == TRUE) {
    
    
    # Transforma data para pode colocar como nome de variavel
    tmp_data_anterior <- format(data_previsao_anterior, format = "%Y_%m_%d")
    
    # Renomeia variavel e inclui data anterior no nome
    tmp_data_export <- 
      tmp_data_plot %>%
      mutate(
        data_da_previsao = data_previsao_anterior
      )
    
    # Exporta
    write.csv(
      x = tmp_data_export,
      file = paste0(
        "./www/S00_Dados_Brutos/Previsoes/previsao_", 
        tmp_modelo_arquivo,
        tmp_data_anterior, 
        ".csv"
      )
    )
    
  }
  
  # \__ Importar Previsoes Anteriores ----------------------------------------
  
  if (importa_dados == TRUE) {
    # Transforma data para pode colocar como nome de variavel
    tmp_data_anterior <- format(data_previsao_anterior, format = "%Y_%m_%d")
    
    tmp_data_plot_anterior <- read.csv(
      file = paste0(
        "./www/S00_Dados_Brutos/Previsoes/previsao_",
        tmp_modelo_arquivo,
        tmp_data_anterior,
        ".csv"
      )
    )
    
    return(list(
      data_plot = tmp_data_plot,
      data_plot_anterior = tmp_data_plot_anterior,
      modelo_usado = tmp_modelo
    ))
    
  } else {
    
    return(list(
      data_plot = tmp_data_plot,
      data_plot_anterior = NULL,
      modelo_usado = tmp_modelo
    ))
    
  }
  
}