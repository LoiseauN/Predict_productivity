#' Pooling productivity and biomass by transect
#' 
#' @param prod_data data to pool
#' 
#' 
#' @return data with productivity biomass for each transect
#' @export
#' 

data_management <- function(data_prod){
  
  biom75 = quantile(data_prod$logBiom,0.95)
  biom25 = quantile(data_prod$logBiom,0.25)
  prod75 = quantile(data_prod$logProdB,0.75)
  prod25 = quantile(data_prod$logProdB,0.25)
  
  #Diving data into 3 classes for each biomass/productivity relationship
  management = data_prod %>% 
    mutate(Travel_h = travel_time/60)%>%
    mutate(Class = ifelse(logBiom < biom25 & logProdB < prod25,"deadzone",
                          ifelse(logBiom < biom75 & logProdB > prod75,"partial",
                                 ifelse(logBiom > biom75,"pristine","transition"))),
           Class = as.factor(Class))%>%
    column_to_rownames("ID_transect")
  
  return(management)
  
}