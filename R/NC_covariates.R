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
    dplyr::select(-c(Biom,Prod:logProdB,Latitude,sst,travel_time,Longitude,mpa_pres,coral_surf,coralpres))%>%
    na.omit()%>%
    rownames_to_column('ID_transect') %>%
    dplyr::mutate(depth = arm::rescale(log(abs(depth)+1)),
                  coralcover = arm::rescale(log(coralcover+1)),
                  coral_l2 = as.factor(coral_l2),
                  coral_l3= as.factor(coral_l3),
                  Travel_h = arm::rescale(log(Travel_h+1)),
                  mpa_type = as.factor(mpa_type))%>%
    column_to_rownames('ID_transect')
  
  return(covariates)
  
  
}