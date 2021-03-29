#' Pooling productivity and biomass by transect
#' 
#' @param model_test_output the output from the test_model function
#' 
#' 
#' @return data with productivity biomass for each transect
#' @export
#' 

plot_var_imp <- function(model_test_output){
  
  #Getting variable importance from output list
  rel_inf = model_test_output[[1]] %>%
    #Keeping only variables and their importance
    dplyr::select(rowname,importance.mod.)%>%
    na.omit()%>%
    #Getting mean variable importance over all iterations
    dplyr::group_by(rowname)%>%
    dplyr::summarize(importance.mod.=mean(importance.mod.))%>%
    arrange(importance.mod.)

rel_inf$rowname = c("Coral habitat 1","MPA Type","Biotope","Coral habitat 2","Coral cover","Depth","Travel time")
         
   plot = ggdotchart(rel_inf,x = "rowname",y="importance.mod.",
               color = "rowname",
               palette = "uchicago",
               add = "segments",
               add.params = list(color = "rowname",size = 1.5),
               rotate = TRUE,   
               group = "rowname",
               dot.size = 11, 
               label = round(rel_inf$importance.mod.,4),
               font.label = list(color = "white", size = 6, 
                                 vjust = 0.5),
               xlab = "",
               ylab = "Variable importance (permutation)",
               ggtheme = theme_pubclean())
    
    ggpar(plot, legend = "none")
    
    ggsave("Figures/variableimportance.pdf",height=12,width=15)
    ggsave("Figures/variableimportance.png",height=12,width=15)
            
}
