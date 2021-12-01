#' Variable importance plot of our random forest model - the mean over 100 iterations
#' 
#' @param model_test_output the output from the test_model function
#' 
#' @return plot representing mean variable importance of our random forest model over 100 iterations
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
    mutate(percentage = (importance.mod.*100)/sum(importance.mod.))%>%
    arrange(percentage)
  
  rel_inf$var_names = c("NGO Presence","Control of Corruption","Depth","Human Development Index","Marine Ecosystem Dependency","No Violence","Effectiveness","Human Voice","Mean DHW (over 5 years)","Mean pH (over 5 years)","Mean SST (over 5 years)","Mean NPP (overs 5 years)","Human gravity")

  plot = ggdotchart(rel_inf,x = "var_names",y="percentage",
                    color = "var_names",
                    palette = "simpsons",
                    add = "segments",
                    add.params = list(color = "var_names",size = 1.5),
                    rotate = TRUE,   
                    group = "var_names",
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
