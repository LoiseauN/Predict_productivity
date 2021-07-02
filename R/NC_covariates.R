#' Pooling productivity and biomass by transect
#' 
#' @param prod_data data to pool
#' 
#' 
#' @return data with productivity biomass for each transect
#' @export
#' 

data_covariates <- function(data_prod){
  
  covariates = data_prod %>%
    dplyr::select(-c(log10Biom,log10ProdB,SurveyID,SiteLatitude,SiteLongitude,Country))%>%
    mutate(Class = as.factor(Class))
  
  return(covariates)
  
  
}