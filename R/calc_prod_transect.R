#' Calculating productivity
#' 
#' @param prod_data data to predict productivity on
#' @param area transect area
#' @return dataframe with performance of each model
#' @export
#' 

calc_prod_transect <- function(data_with_prod,transect_info){
  
  #At the scale of the community (transect)
  data_prod_brut <-  data_with_prod[,c("SurveyID","Num", "Wgain","Biom","Prod")]
  data_prod_brut <- aggregate(. ~ SurveyID, data = data_prod_brut, sum, na.rm=T)
  data_prod_brut$Productivity  <- data_prod_brut$Prod/data_prod_brut$Biom
  
  data_prod_brut <-merge(data_prod_brut,transect_info,by="SurveyID",all.x=T)
  
  data_prod_brut$log10ProdB <-log10(data_prod_brut$Productivity)
  data_prod_brut$log10Biom <-log10(data_prod_brut$Biom)
  data_prod_brut$log10Prod<-log10(data_prod_brut$Prod)
  data_prod_brut$SiteLatitude <- as.numeric(as.character(data_prod_brut$SiteLatitude))
  data_prod_brut$SiteLongitude <- as.numeric(as.character(data_prod_brut$SiteLongitude))
  
  return(data_prod_brut)
  

  
 
  
}