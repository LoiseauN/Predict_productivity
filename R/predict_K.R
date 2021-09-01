#' Predicting K using MTE 
#' 
#' @param prod_data data to predict productivity on
#' @param gen_model the output from the save_gen_model_K function
#' @param fam_model the output from the save_fam_model_K function
#' @param fish_model the output from the fish_model_K function
#' 
#' @return dataframe with performance of each model
#' @export
#' 


predict_K <- function(data_prod,gen_model,fam_model,fish_model){
  
  `%notin%` <- Negate(`%in%`)
  data_prod$K_pred <- NA
  
  for (i in 1:nrow(data_prod)){
    
    print(paste("i =", i,"  percentage avancement =", round((i/nrow(data_prod))*100, digits = 2) ,"%"))
    
    if(!is.na(data_prod$K_growth[i])) {
      
      data_prod$K_pred[i] = data_prod$K_growth[i] * exp((-0.3839812/8.62e-05)*
                                                          ((1/(data_prod$sstmean_growth[i]))
                                                           -(1/(data_prod$Temperature[i]+237.5))))}
    
    else if (is.na(data_prod$K_growth[i]) & data_prod$Genus[i] %in% gen_model$Genus)  { 
      
      sub_gen <-subset(gen_model,gen_model$Genus == data_prod$Genus[i])
      
      data_prod$K_pred[i] = exp(sub_gen$Intercept) * data_prod$Mmax[i]**(sub_gen$SlopeLogMmax)*exp(sub_gen$SlopeInvTkb/(8.62e-05*(data_prod$Temperature[i]+237.5))) 
      
    }  else if(is.na(data_prod$K_growth[i]) & data_prod$Genus[i] %notin% gen_model$Genus & data_prod$Family[i] %in% fam_model$Family)  { 
      
      sub_fam <-subset(fam_model,fam_model$Family == data_prod$Family[i])
      
      data_prod$K_pred[i] = exp(sub_fam$Intercept) * data_prod$Mmax[i]**(sub_fam$SlopeLogMmax)*exp(sub_fam$SlopeInvTkb/(8.62e-05*(data_prod$Temperature[i]+237.5))) 
      
    }  else { data_prod$K_pred[i] = exp(fish_model$Intercept[1]) * data_prod$Mmax[i]**(fish_model$SlopeLogMmax[1])*exp((fish_model$SlopeInvTkb[1]/(8.62e-05*(data_prod$Temperature[i]+237.5))))
    }
    
  }
  
return(data_prod)  

}
