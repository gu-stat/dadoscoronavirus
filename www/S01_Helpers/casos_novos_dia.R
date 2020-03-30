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

# ************************************************************************* ####
# FIM                                                                       ####
# **************************************************************************** #