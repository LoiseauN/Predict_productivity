#' Pooling productivity and biomass by transect
#' 
#' @param prod_data data to pool
#' 
#' 
#' @return data with productivity biomass for each transect
#' @export
#' 

prod_pool <- function(prod_data){
  
  #Selecting transect variables
  NC_transect <- prod_data %>%
    dplyr::select(ID_transect,Latitude,Longitude,biotope,sst:mpa_pres,Biom,Prod)%>%
    group_by(ID_transect)%>%
    mutate(Prod=sum(Prod),
           Biom=sum(Biom),
           ProdB = Prod/Biom,
           logProd = log(Prod+1),
           logBiom = log(Biom+1),
           logProdB = log(ProdB+1))%>%
    distinct()
}
