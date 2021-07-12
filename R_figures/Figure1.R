#' Pooling productivity and biomass by transect
#' 
#' @param prod_data data with management classes to plot
#' 
#' 
#' @return data with productivity biomass for each transect
#' @export
#'


plot_classes <- function(data_prod){
  
  biom75 = quantile(data_prod$log10Biom,0.95)
  biom25 = quantile(data_prod$log10Biom,0.25)
  prod75 = quantile(data_prod$log10ProdB,0.75)
  prod25 = quantile(data_prod$log10ProdB,0.25)
  
  group.colors <- c(deadzone = "#d69d4e", partial = "#046c9a", pristine = "#C1DB60", transition = "#ABDDDE")
  
  #Plotting the classes
  (ggplot(data_prod,aes(log10Biom,log10ProdB,colour=Class))+
     geom_point(size=4,alpha=0.4)+
     geom_hline(yintercept=prod25,linetype="dashed")+
     geom_vline(xintercept=biom25,linetype="dashed")+
     geom_hline(yintercept=prod75,linetype="dashed")+
     geom_vline(xintercept=biom75,linetype="dashed")+
     scale_colour_manual(values=group.colors,labels=c("Degraded reefs","Productive reefs","Sanctuaries","Transitional reefs"))+
     labs(x="Biomass (g/m²)",
          y = "Productivity (%)")+
     theme_classic())
  
  ggsave("Figures/Figure1.pdf",dpi=300,width = 297, height = 210,units = "mm")
  ggsave("Figures/Figure1.png",dpi=300,width = 297, height = 210,units = "mm")
  
  
}