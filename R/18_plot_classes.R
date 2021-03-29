#' Pooling productivity and biomass by transect
#' 
#' @param prod_data data with management classes to plot
#' 
#' 
#' @return data with productivity biomass for each transect
#' @export
#'



plot_classes <- function(data_prod){
  
  biom75 = quantile(data_prod$logBiom,0.95)
  biom25 = quantile(data_prod$logBiom,0.25)
  prod75 = quantile(data_prod$logProdB,0.75)
  prod25 = quantile(data_prod$logProdB,0.25)
  
  group.colors <- c(deadzone = "#eccbae", partial = "#046c9a", pristine = "#d69d4e", transition = "#ABDDDE")
  
  #Plotting the classes
  (ggplot(data_prod,aes(logBiom,logProdB,colour=Class))+
     geom_point(size=4,alpha=0.4)+
     geom_hline(yintercept=prod25,linetype="dashed")+
     geom_vline(xintercept=biom25,linetype="dashed")+
     geom_hline(yintercept=prod75,linetype="dashed")+
     geom_vline(xintercept=biom75,linetype="dashed")+
     scale_colour_manual(values=group.colors,labels=c("High Biomass","Transition","Low Productivity/Biomass","High Productivity"))+
     labs(x="Biomass (g/m²)",
          y = "Productivity (%)")+
     theme_classic())
  
  ggsave("Figures/plot_classes.pdf",height=10,width=12)
  ggsave("Figures/plot_classes.png",height=10,width=12)
  
  
}