#' PVarious figures with K by size, K by family, K for example families and genus
#' 
#' @param prod_data Community level productivity for the 3714 transects
#' 
#' 
#' @return Figure with the number of predictions by level in figures folder
#' 

predictions_by_level = function(data_prod){
  
  #Plot number of predictions at family, genus, and species level for K
  ggplot(data_prod)+
    geom_histogram(bins = 30,aes(K_pred,fill=pred_type))+
    scale_fill_viridis_d()+
    theme_classic()+
    labs(x = "Predicted growth rates K",
         y= "Number of predictions",
         fill = "Prediction level")
  
  ggsave("Figures/predictionslevels.pdf",height=210,width=297,units="mm")
  ggsave("Figures/predictionslevels.png",height=210,width=297,units="mm")
  
  data_prod$pred_type = as.factor(data_prod$pred_type)
  print(summary(data_prod))

}
