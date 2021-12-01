#' Table of taxonomy used in this study
#' 
#' @param data_prod Community level information for the 3714 transects
#' 
#' 
#' @return Table with taxonomy information used in this study
#' 

used_taxonomy = function(data_prod){
  
  data_family_genus = data_prod %>%
    dplyr::select(Family,Genus)%>%
    distinct(Genus, .keep_all = T)
  
  write.csv(data_family_genus, file = "Figures/SuppTable1.csv", sep = ",", quote = FALSE, row.names = F)
  
  
}