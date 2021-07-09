#' Pooling productivity and biomass by transect
#' 
#' @param model_test_output the output from the test_model function
#' 
#' 
#' @return data with productivity biomass for each transect
#' @export
#' 


model_test_output = model_test

plot_var_imp <- function(model_test_output){
  
  #Getting variable importance from output list
  rel_inf = model_test_output[[1]] %>%
    #Keeping only variables and their importance
    dplyr::select(rowname,importance.mod.)%>%
    na.omit()%>%
    #Getting mean variable importance over all iterations
    dplyr::group_by(rowname)%>%
    dplyr::summarize(importance.mod.=mean(importance.mod.))%>%
    mutate(percentage = (importance.mod.*100)/sum(importance.mod.))%>%
    arrange(percentage)

  plot = ggdotchart(rel_inf,x = "rowname",y="percentage",
                    color = "rowname",
                    palette = "simpsons",
                    add = "segments",
                    add.params = list(color = "rowname",size = 1.5),
                    rotate = TRUE,   
                    group = "rowname",
                    dot.size = 11, 
                    label = round(rel_inf$percentage,4),
                    font.label = list(color = "white", size = 6, 
                                      vjust = 0.5),
                    xlab = "",
                    ylab = "Variable importance (%)",
                    ggtheme = theme_pubclean())
  
  ggpar(plot, legend = "none")
    
  ggsave("Figures/Figure3.pdf",height=210,width=297,units="mm")
            
}
