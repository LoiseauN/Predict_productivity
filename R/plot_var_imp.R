#' Pooling productivity and biomass by transect
#' 
#' @param model_test_output the output from the test_model function
#' 
#' 
#' @return data with productivity biomass for each transect
#' @export
#' 

model_test_output = model_test
data_covariates <- function(model_test_output){
  
  #Getting variable importance from output list
  rel_inf = model_test_output[[1]] %>%
    #Keeping only variables and their importance
    dplyr::select(rowname,importance.mod.)%>%
    na.omit()%>%
    #Getting mean variable importance over all iterations
    dplyr::group_by(rowname)%>%
    dplyr::summarize(importance.mod.=mean(importance.mod.))
  
    ggdotchart(rel_inf,x = "importance.mod.",y="rowname",
               sorting = "descending",
               add = "segments",
               rotate = TRUE,   
               group = "rowname",
               dot.size = 6, 
               label = round(rel_inf$importance.mod.),
               font.label = list(color = "white", size = 9, 
                                 vjust = 0.5),
               ggtheme = theme_pubr())
            
}