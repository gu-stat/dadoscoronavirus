# ************************************************************************* ----
# Correcao de bug na funcao 'get_brmap' do pacote 'brazilmaps'              ----
# ************************************************************************* ----

# Veja detalhes aqui: 
# https://github.com/rpradosiqueira/brazilmaps/issues/2#issue-461768010

get_brmap_fixed <- function (geo = c("Brazil", "Region", "State", "MesoRegion", 
                                     "MicroRegion", "City"), 
                             geo.filter = NULL, 
                             class = c("sf", "SpatialPolygonsDataFrame", 
                                       "data.frame")
                             ) {
  geo <- match.arg(geo)
  
  class <- match.arg(class)
  
  geo.filter.df <- data.frame(
    description = as.character(
      c("Brazil", "Region", "State", "MesoRegion", "MicroRegion", "City")
    ),
    level = c(0, 1, 2, 4, 5, 7)
  )
  
  geo1 <- merge(geo.filter.df, data.frame(description = names(geo.filter)))
  
  geo2 <- geo.filter.df[which(geo.filter.df$description == geo), ]
  
  if (any(geo1$level > geo2$level)) 
    stop("The 'geo.filter' argument is misspecified")
  
  brmap <- base::readRDS(
    system.file("maps", paste0(geo, ".rds"), package = "brazilmaps")
  )
  
  if ((!is.null(geo.filter)) & (geo %in% c("Region", "Brazil"))) {
    warning("'geo.filter' argument will be assign to NULL when geo is 'Region' or 'Brazil'")
    geo.filter <- NULL
  }
  else if (!is.null(geo.filter)) {
    brmap_list <- list()
    for (i in 1:nrow(geo1)) {
      brmap_list[[i]] <- 
        brmap %>% 
        dplyr::filter(brmap[[(names(geo.filter)[i])]] %in% geo.filter[[i]]) %>% 
        sf::st_as_sf()
    }
    brmap <- do.call("rbind", brmap_list)
  }
  if (class == "SpatialPolygonsDataFrame") brmap <- sf::as_Spatial(brmap)   # <-- FIX
  if (class == "data.frame") brmap <- sf::as_Spatial(brmap) %>% ggplot2::fortify(region = geo)
  brmap
}

# ************************************************************************* ####
# FIM                                                                       ####
# **************************************************************************** #