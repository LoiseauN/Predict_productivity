#' Calculating productivity
#' 
#' @param prod_data data to predict productivity on
#' @param area transect area
#' @return dataframe with performance of each model
#' @export
#' 

calc_prod <- function(data_prod,area){
  
  #Size corrections
  data_forprod = data_prod %>%
    #If observed size is higher than the maximum reported size, then replace observed size with maximum size
    mutate(Size = ifelse(Size>SizeMax,SizeMax,Size))%>%
    #If observed size is higher than Linf, then we replace Linf with maximum size
    mutate(Linf = ifelse(Size>Linf_pred,SizeMax,Linf_pred))
  
  data_with_prod = data_forprod %>%
    #Calculating production
    #Weight of fish at time of census
    mutate(W = lwa*Size^lwb,
           #Age of fish at time of census
           t = (1/K_pred)*log((SizeMax)/((1-(Size/SizeMax))*SizeMax)),
           #Projected size one year later
           Ltx = Linf * (1-exp(-K_pred*(t+365))),
           #Projected weight one year later
           Wtx = lwa*Ltx**lwb,
           #Production 
           Wgain = Wtx - W,
           #Biomass
           Biom = (W*nb_corr)/area,
           #Individual Biomass 
           IndBiom = (W/area),
           #Production
           Prod = (Wgain * nb_corr)/area,
           #Individual production 
           IndProd = (Wgain/area)) %>%
    filter(!is.na(Prod))
  
  return(data_with_prod)

}