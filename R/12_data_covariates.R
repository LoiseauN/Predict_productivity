#' Prepping data with covariates, keeping only transects for coral reefs
#' 
#' @param data_prod output from the calc_prod_transect function
#' @param env_data environmental data for RLS
#' @param mpa_data mpa data for RLS
#' @param socio_data socio-economic data for RLS
#' 
#' @return data at transect levels with covariates etc
#' 


data_covariates <- function(data_prod,env_data,mpa_data,socio_data){
  
  covariates = data_prod %>%
    #Merging with covariates data
    left_join(socio_data,by="SurveyID") %>%
    left_join(env_data,by="SurveyID") %>%
    left_join(mpa_data, by="SurveyID") %>%
    #Keeping tropical only
    filter(min_sst_5year > 17) %>% 
    #Selecting variables of interest
    dplyr::select(SurveyID,site_code,Depth,gravtot2,mean_npp_5year,No.take.multizoned,mean_sst_5year,
                  mean_pH_1year_5year,mean_DHW_5year,NoViolence,ControlofCorruption,
                  Voice,HDI,MarineEcosystemDependency,Depth,NGO,Effectiveness,log10ProdB,log10Prod,log10Biom,SiteLatitude,SiteLongitude,Country)
  
  #Correcting Effectiveness covariate : NA means no MPA
  levels(covariates$Effectiveness)  = c("Low","Medium","High","No_Mpa")
  covariates$Effectiveness[is.na(covariates$Effectiveness)] = "No_Mpa"
  
  return(covariates)
  
  
}
