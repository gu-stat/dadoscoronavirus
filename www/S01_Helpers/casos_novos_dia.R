# ************************************************************************* ----
# Funcoes para calcular o numero de casos novos por dia                     ----
# ************************************************************************* ----

# As bases dao os dados acumulados. Temos que calcular os casos por dia.

# |_ Casos Novos - Brasil ====================================================== 

fcn_dia <- function(DADOS, variavel){
  tmp <- sapply(
    X = 1:dim(DADOS)[1],
    function(X) {
      if (X == 1) {
        DADOS[1, variavel] 
      } else {
        DADOS[X, variavel] - DADOS[X-1, variavel] 
      }
    }
  )
  
  do.call(rbind, tmp)
}

# |_ Casos Novos - Por UF ======================================================

fcn_dia_uf <- function(DF, variavel){
  
  tmp_ufs <- DF %>% select(uf_num) %>% distinct() %>% c()
  
  tmp_outer <- lapply(
    X = tmp_ufs$uf_num,
    function(UF = X){
      tmp_DADOS <- DF %>% filter(uf_num == UF)
      
      fcn_dia(tmp_DADOS, variavel)
    }
  )
  
  do.call(rbind, tmp_outer)
  
}

# |_ Correcao Data - Por Municipio =============================================

fcn_corrige_mun <- function(DF){
  
  data_range <- 
    DF %>%
    select(dia) %>%
    distinct() %>%
    arrange(desc(dia))
  
  data_final  <- ymd(data_range$dia[1])
  
  tmp_uf_mun <- 
    DF %>% 
    filter(municipio != "")
  
  # \__ Sem NA -----------------------------------------------------------------
  
  tmp_uf_mun_sem_NA <- 
    tmp_uf_mun %>%
    filter(!is.na(cod_municipio))
  
  tmp_uf_mun_sem_NA_unique <- 
    tmp_uf_mun_sem_NA %>%
    select(cod_municipio) %>%
    distinct()
  
  mun_corrigido_sem_NA <- lapply(
    X = 1:length(tmp_uf_mun_sem_NA_unique$cod_municipio),
    function(X){
      
      MUN <- tmp_uf_mun_sem_NA_unique$cod_municipio[X] 
      
      tmp_DADOS <- DF %>% filter(cod_municipio == MUN)
      
      tmp_min_data <- min(tmp_DADOS$dia)
      
      tmp_num_dias <- data_final - tmp_min_data
      
      tmp_data_esperadas <- tmp_min_data + days(0:tmp_num_dias)
      
      tmp_data_observadas <- tmp_DADOS$dia 
      
      tmp_mun_corrigido <- 
        tibble(
          dia = tmp_data_esperadas
        ) %>%
        left_join(
          tmp_DADOS,
          by = "dia"
        )
      
      for (Dias in 2:dim(tmp_mun_corrigido)[1]) {
        if(is.na(tmp_mun_corrigido[Dias, "is_last"]) == TRUE){
          tmp_mun_corrigido[Dias, -1] <- tmp_mun_corrigido[Dias-1, -1]
        }
      }
      
      tmp_mun_corrigido <-
        tmp_mun_corrigido %>%
        mutate(
          mortes_confirmadas = case_when(
            is.na(mortes_confirmadas) ~ 0,
            TRUE ~ mortes_confirmadas
          )
        )
      
      for (Dias in 2:dim(tmp_mun_corrigido)[1]) {
        if(tmp_mun_corrigido[Dias, "casos_confirmados"] == 0){
          tmp_mun_corrigido[Dias, "casos_confirmados"] <- 
            tmp_mun_corrigido[Dias-1, "casos_confirmados"]
        }
        if(tmp_mun_corrigido[Dias, "mortes_confirmadas"] == 0){
          tmp_mun_corrigido[Dias, "mortes_confirmadas"] <- 
            tmp_mun_corrigido[Dias-1, "mortes_confirmadas"]
        }
      }
      
      tmp_mun_corrigido <- 
        tmp_mun_corrigido %>%
        mutate(
          confirmados_dia = fcn_dia(., variavel = "casos_confirmados"),
          mortes_dia      = fcn_dia(., variavel = "mortes_confirmadas")
        )
      
      return(tmp_mun_corrigido)
      
    }
  )
    
  mun_corrigido_sem_NA <- do.call(rbind, mun_corrigido_sem_NA)
  
  # \__ Com NA -----------------------------------------------------------------
  
  tmp_uf_mun_com_NA <- 
    tmp_uf_mun %>%
    filter(is.na(cod_municipio)) %>%
    arrange(dia, uf)
  
  tmp_uf_com_NA <- tmp_uf_mun_com_NA %>% select(uf) %>% distinct()
  
  mun_corrigido_com_NA <- lapply(
    X = 1:length(tmp_uf_com_NA$uf),
    function(X){
      UF <- tmp_uf_com_NA$uf[X] 
      
      tmp_DADOS <- tmp_uf_mun_com_NA %>% filter(uf == UF)
      
      tmp_min_data <- min(tmp_DADOS$dia)
      
      #tmp_max_data <- max(tmp_DADOS$dia)
      
      tmp_num_dias <- data_final - tmp_min_data
      
      tmp_data_esperadas <- tmp_min_data + days(0:tmp_num_dias)
      
      tmp_data_observadas <- tmp_DADOS$dia 
      
      tmp_uf_mun_corrigido <- 
        tibble(
          dia = tmp_data_esperadas
        ) %>%
        left_join(
          tmp_DADOS,
          by = "dia"
        )
      
      for (Dias in 2:dim(tmp_uf_mun_corrigido)[1]) {
        if(is.na(tmp_uf_mun_corrigido[Dias, "is_last"]) == TRUE){
          tmp_uf_mun_corrigido[Dias, -1] <- tmp_uf_mun_corrigido[Dias-1, -1]
        }
      }
      
      tmp_uf_mun_corrigido <-
        tmp_uf_mun_corrigido %>%
        mutate(
          mortes_confirmadas = case_when(
            is.na(mortes_confirmadas) ~ 0,
            TRUE ~ mortes_confirmadas
          )
        )
      
      for (Dias in 2:dim(tmp_uf_mun_corrigido)[1]) {
        if(tmp_uf_mun_corrigido[Dias, "casos_confirmados"] == 0){
          tmp_uf_mun_corrigido[Dias, "casos_confirmados"] <- 
            tmp_uf_mun_corrigido[Dias-1, "casos_confirmados"]
        }
        if(tmp_uf_mun_corrigido[Dias, "mortes_confirmadas"] == 0){
          tmp_uf_mun_corrigido[Dias, "mortes_confirmadas"] <- 
            tmp_uf_mun_corrigido[Dias-1, "mortes_confirmadas"]
        }
      }
      
      tmp_uf_mun_corrigido <- 
        tmp_uf_mun_corrigido %>%
        mutate(
          confirmados_dia = fcn_dia(., variavel = "casos_confirmados"),
          mortes_dia      = fcn_dia(., variavel = "mortes_confirmadas")
        )
      
      return(tmp_uf_mun_corrigido)
      
    }
  )
  
  mun_corrigido_com_NA <- do.call(rbind, mun_corrigido_com_NA)
  
  # \__ Data Municipio Final ---------------------------------------------------
  
  mun_corrigido_final <- 
    rbind(mun_corrigido_sem_NA, mun_corrigido_com_NA) %>%
    arrange(uf_num, cod_municipio, dia)
  
    
  mun_corrigido_final
}

# ************************************************************************* ####
# FIM                                                                       ####
# **************************************************************************** #