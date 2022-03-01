#' Aggregating productivity at transect level, and logging it 
#' 
#' @param data_with_prod Output from the calc_prod function
#' @param transect_info transect area
#' 
#' @return dataframe with productivity calculated at community level
#' 

calc_prod_transect <- function(data_with_prod,transect_info){

  transect_info <- read.table("data/RLS_transect_info.txt")
  
  transect_info = transect_info %>% 
    dplyr::select(SurveyID, SiteLatitude, SiteLongitude, SiteCode, Depth, Country)
  
  transect_site = transect_info %>% dplyr::select(SurveyID, SiteCode) %>%
    dplyr::filter(SurveyID %in% data_with_prod$SurveyID) 
  
  data_with_prod = data_with_prod %>% left_join(transect_site, by = "SurveyID")
  
  #At the scale of the community (transect)
  data_prod_brut <-  data_with_prod[,c("SurveyID","Biom","Prod")]
  data_prod_brut_sites <-  data_with_prod[,c("SurveyID","SiteCode")]
  data_prod_brut <- aggregate(. ~ SurveyID, data = data_prod_brut, sum, na.rm=T)
  data_prod_brut$Productivity  <- data_prod_brut$Prod/data_prod_brut$Biom
  data_prod_brut <- left_join(data_prod_brut,data_prod_brut_sites,by="SurveyID")
  data_prod_brut <- aggregate(. ~ SiteCode, data = data_prod_brut[-1], mean, na.rm=T)
  
  data_prod_brut = left_join(data_prod_brut, transect_info, by ="SiteCode")
  
  data_prod_brut$log10ProdB <-log10(data_prod_brut$Productivity+1)
  data_prod_brut$log10Biom <-log10(data_prod_brut$Biom+1)
  data_prod_brut$log10Prod<-log10(data_prod_brut$Prod+1)
  data_prod_brut$SiteLatitude <- as.numeric(as.character(data_prod_brut$SiteLatitude))
  data_prod_brut$SiteLongitude <- as.numeric(as.character(data_prod_brut$SiteLongitude)) 
  
  data_prod_brut = data_prod_brut %>% dplyr::rename(site_code = "SiteCode")


  return(data_prod_brut)

}