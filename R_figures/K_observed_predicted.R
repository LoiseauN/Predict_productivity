#' Plotting  observed K values versus our predicted K values
#' 
#' @param growth_data output from the data_prep function
#' 
#' @return Plot of observed K versus predicted K 


K_observed_predicted = function(growth_data){
  
  # Quick figure to plot K observed versus K predicted
  # For testing with intial database
  data_merged = growth_data %>%  mutate(K_growth = NA) %>% rename(Temperature = "sstmean") %>% mutate(Temperature = Temperature - 237.5)

  data_K = predict_K(data_merged,gen_model_K,fam_model_K,fish_model_K)

  data_plot = data_K %>%
    pivot_longer(c(K,K_pred))%>%
    mutate(value = log(value+1))

  ggplot(data_plot,aes(value,fill=name))+
    geom_density(alpha = 0.6)+
    scale_fill_manual(labels = c("Observed K","Predicted K"), values = c("#1E72A6","#FDDA26"))+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"))+
    labs(x = "Growth rate",
         y=  "Density")+
    guides(fill = guide_legend(title="Growth rate"))

  ggsave("figures/PredictedObservedK.pdf",width = 297, height = 210,unit="mm")
  ggsave("figures/PredictedObservedK.png",width = 297, height = 210,unit="mm")
  
}


