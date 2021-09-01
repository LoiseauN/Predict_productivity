#' Calculating productivity
#' 
#' @param prod_data data to predict productivity on
#' @param area transect area
#' @return dataframe with performance of each model
#' @export
#' 

calc_prod <- function(data_prod){
  
  #Size corrections
  data_forprod = data_prod %>%
    #If observed size is higher than the maximum reported size, then replace observed size with maximum size
    mutate(Sizeclass = ifelse(Sizeclass>MaxLength,MaxLength,Sizeclass))
    #If observed size is higher than Linf, then we replace Linf with maximum size
    # mutate(Linf = ifelse(Sizeclass>Linf_pred,MaxLength,Linf_pred))
  
  data_with_prod = data_forprod %>%
    #Calculating production
    #Weight of fish at time of census
    mutate(W = lwa*Sizeclass^lwb,
           #Age of fish at time of census
           t = (1/K_pred)*log((MaxLength)/((1-(Sizeclass/MaxLength))*MaxLength)),
           #Projected size one year later
           Ltx = MaxLength * (1-exp(-K_pred*(t+365))),
           #Projected weight one year later
           Wtx = lwa*Ltx**lwb,
           #Production 
           Wgain = Wtx - W,
           #Biomass
           Biom = (W*Num)/Area,
           #Individual Biomass 
           IndBiom = (W/Area),
           #Production
           Prod = (Wgain * Num)/Area,
           #Individual production 
           IndProd = (Wgain/Area)) %>%
    filter(!is.na(Prod))
  
  return(data_with_prod)

}