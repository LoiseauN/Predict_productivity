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
    arrange(percentage) %>%
    mutate(variabletype = ifelse(rowname %in% c("Depth","mean_DHW_5year","mean_pH_1year_5year","mean_npp_5year","mean_sst_5year"),"Environmental","Socio-economic"),
           rowname = as.factor(rowname))
  
  rel_inf$var_names = recode_factor(rel_inf$rowname, depth = "Depth",Effectiveness = "MPA Effectiveness",ControlofCorruption = "Control of Corruption",HDI = "Human Development Index",NoViolence = "No Violence",
                                    Voice = "Voice",MarineEcosystemDependency = "Marine Ecosystem Dependency",NGO = "NGO Presence",mean_DHW_5year = "Mean DHW over 5 years",
                                    gravtot2 = "Human gravity",mean_npp_5year="Mean NPP over 5 years",mean_pH_1year_5year="mean pH over 5 years",mean_sst_5year="Mean SST over 5 years")
  
  rel_inf$var_names = fct_reorder(rel_inf$var_names, rel_inf$percentage)
  

  plot = ggdotchart(arrange(rel_inf,percentage),x = "var_names",y="percentage",
                    color = "variabletype",
                    palette = c('#56B4E9','#E69F00'),
                    add = "segments",
                    add.params = list(color = "variabletype",size = 1.5),
                    rotate = TRUE,   
                    group = "var_names",
                    dot.size = 13, 
                    label = round(rel_inf$percentage,2),
                    font.label = list(color = "white", size = 8, 
                                      vjust = 0.5),
                    xlab = "",
                    ylab = "Variable importance (%)",
                    ggtheme = theme_pubclean(),
                    legend.title = "Variable type")
  
  ggpar(plot, legend = "bottom")
    
  ggsave("Figures/Figure3_+5.pdf",height=210,width=297,units="mm")
  ggsave("Figures/Figure3_+5.png",height=210,width=297,units="mm")      
}
