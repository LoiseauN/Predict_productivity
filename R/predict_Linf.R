#' Predicting Linf
#'
#' @param prod_data data to predict productivity on
#' @param gen_model the output from the gen_model_Linf function
#' @param fam_model the output from the fam_model_Linf function
#' 
#' @return dataframe with performance of each model
#' @export
#' 

predict_Linf <- function(data_prod, fam_model_Linf, gen_model_Linf,fish_model_Linf){
  
  `%notin%` <- Negate(`%in%`)
  data_prod$Linf_pred <- NA

for (i in 1:nrow(data_prod)){
  
  print(paste("i =", i,"  percentage avancement =", round((i/nrow(data_prod))*100, digits = 2) ,"%"))
  
  if (data_prod$Genus[i] %in% gen_model_Linf$Genus)  {
    
    subdata_Linf_gen <-subset(gen_model_Linf,gen_model_Linf$Genus == data_prod$Genus[i])
    
    data_prod$Linf_pred[i] = subdata_Linf_gen$pente_pred*data_prod$MaxLength[i]
    
  }  else if(data_prod$Genus[i] %notin% gen_model_Linf$Genus & data_prod$Family[i] %in% fam_model_Linf$Family)  {
    
    subdata_Linf_fam <- subset(fam_model_Linf,fam_model_Linf$Family == data_prod$Family[i])
    
    data_prod$Linf_pred[i] = subdata_Linf_fam$pente_pred*data_prod$MaxLength[i]
    
  }  else { data_prod$Linf_pred[i] = fish_model_Linf *data_prod$MaxLength[i]
  
  }
}
  
  
  return(data_prod)
  
  save(data_prod,file="outputs/data_final.RData")
  
}
