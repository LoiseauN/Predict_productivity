#' Calculating productivity
#' 
#' @param data_prod Output from the predict_Linf function, with K and Linf predcited
#' 
#' 
#' @return dataframe with biomass/biomass production/productivity calculated for all communities in RLS data base
#' 
calc_prod <- function(data_prod){

  #Size corrections
  data_forprod = data_prod %>%
    #If observed size is higher than the maximum reported size, then replace observed size with maximum size
    dplyr::mutate(Sizeclass = ifelse(Sizeclass>MaxLength,MaxLength,Sizeclass)) %>%
    #If observed size is higher than Linf, then we replace Linf with maximum size
    dplyr::mutate(Linf = ifelse(Sizeclass>Linf_pred,MaxLength,Linf_pred))
  
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