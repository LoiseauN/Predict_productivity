#' Creating management classes for our data 
#' 
#' @param data_prod  output from the data_covariates function
#' @param s_sup_biom upper biomass threshold
#' @param s_inf_biom lower biomass threshold
#' @param s_sup_prod upper productivity threshold
#' @param s_inf_prod lower productivity threshold
#' 
#' @return data with management class added
#' 

data_management <- function(data_prod,s_sup_biom,s_inf_biom,s_sup_prod,s_inf_prod){
  
  biom75 = quantile(data_prod$log10Biom,s_sup_biom)
  biom25 = quantile(data_prod$log10Biom,s_inf_biom)
  prod75 = quantile(data_prod$log10ProdB,s_sup_prod)
  prod25 = quantile(data_prod$log10ProdB,s_inf_prod)
  
  #Diving data into 3 classes for each biomass/productivity relationship
  management = data_prod %>% 
    mutate(Class = ifelse(log10Biom < biom25 & log10ProdB < prod25,"deadzone",
                          ifelse(log10ProdB > prod75,"partial",
                                 ifelse(log10Biom > biom75 & log10ProdB < prod75,"pristine","transition"))))%>%
    mutate(Class = as.factor(Class))
  
  return(management)
  
}