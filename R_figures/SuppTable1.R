#' Pooling productivity and biomass by transect
#' 
#' @param prod_data data to pool
#' 
#' 
#' @return data with productivity biomass for each transect
#' @export
#' 

SuppTable1 = function(data_prod){
  
  data_family_genus = data_prod %>%
    dplyr::select(Family,Genus)%>%
    distinct(Genus, .keep_all = T)
  
  write.csv(data_family_genus, file = "Figures/SuppTable1.csv", sep = ",", quote = FALSE, row.names = F)
  
  
}