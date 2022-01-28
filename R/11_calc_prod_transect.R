#' Aggregating productivity at transect level, and logging it 
#' 
#' @param data_with_prod Output from the calc_prod function
#' @param transect_info transect area
#' 
#' @return dataframe with productivity calculated at community level
#' 

calc_prod_transect <- function(data_with_prod,transect_info){
  
  data_with_prod = RLS_prod_all
  transect_info = info
  
  transect_info = transect_info %>% dplyr::rename(SiteLatitude = "latitude",
                                         SiteLongitude = "longitude") %>%
    dplyr::select(survey_id, SiteLatitude, SiteLongitude, site_code, depth, country) %>%
    dplyr::rename(Depth = "depth",
                  Country = "country")
  
  transect_site = transect_info %>% dplyr::select(survey_id, site_code) %>% dplyr::rename(SurveyID = 'survey_id') %>% 
    dplyr::filter(SurveyID %in% data_with_prod$SurveyID) 
  
  data_with_prod = data_with_prod %>% left_join(transect_site, by = "SurveyID")
  
  #At the scale of the community (transect)
  data_prod_brut <-  data_with_prod[,c("SurveyID","Biom","Prod")]
  data_prod_brut_sites <-  data_with_prod[,c("SurveyID","site_code")]
  data_prod_brut <- aggregate(. ~ SurveyID, data = data_prod_brut, sum, na.rm=T)
  data_prod_brut$Productivity  <- data_prod_brut$Prod/data_prod_brut$Biom
  data_prod_brut <- left_join(data_prod_brut,data_prod_brut_sites,by="SurveyID")
  data_prod_brut <- aggregate(. ~ site_code, data = data_prod_brut[-1], mean, na.rm=T)
  
  data_prod_brut = left_join(data_prod_brut, transect_info, by ="site_code")
  
  data_prod_brut$log10ProdB <-log10(data_prod_brut$Productivity+1)
  data_prod_brut$log10Biom <-log10(data_prod_brut$Biom+1)
  data_prod_brut$log10Prod<-log10(data_prod_brut$Prod+1)
  data_prod_brut$SiteLatitude <- as.numeric(as.character(data_prod_brut$SiteLatitude))
  data_prod_brut$SiteLongitude <- as.numeric(as.character(data_prod_brut$SiteLongitude)) 
  
  data_prod_brut = data_prod_brut %>% dplyr::rename(SurveyID = "survey_id")

  return(data_prod_brut)

}