#' Pooling productivity and biomass by transect
#' 
#' @param prod_data data with management classes to plot
#' 
#' 
#' @return data with productivity biomass for each transect
#' @export
#' 

growth_size <- function(data){
  
  ggplot(data, aes(K_pred,SizeMax))+
    geom_point(alpha = 0.2)+
    geom_smooth(color = "black")+
    labs(x="Predicted growth rates",
         y="Maximum observed size")+
    theme_bw()
  
  ggsave("Figures/growth_by_size.pdf",height=12,width=16)
  ggsave("Figures/growth_by_size.png",height=12,width=16)
  
}
