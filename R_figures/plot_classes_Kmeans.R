#' Plotting various representations of our biplot of biomass/productivity
#' 
#' @param data_prod output from the data_management function
#' 
#' @return Classic biplot used in figure 1, biplot according to protection status, and biplot according to gravity/marine ecosystem dependency used in Figure 4


plot_classes <- function(data_prod){
  
  data_prod = RLS_Management
  
  group.colors <- c(deadzone = "#d69d4e", partial = "#046c9a", pristine = "#C1DB60")
  
  #Plotting the classes
  (ggplot(data_prod,aes(log10Biom,log10ProdB,colour=Class))+
      geom_mark_hull(aes(fill = Class), con.cap= 0, expand = 0, radius = 0, con.size = 0)+
     geom_point(size=3,alpha=0.4)+
     scale_colour_manual(values=group.colors,labels=c("Low turnover/biomass reefs","High turnover","High Biomass reefs"))+
     scale_fill_manual(values=group.colors,labels=c("Low turnover/biomass reefs","Productive reefs","High Biomass reefs"))+
     labs(x="Biomass (g/m²) - log scale",
          y = "Productivity (%/year) - log scale")+
     theme_classic())

  
  ggsave("Figures/Figure1.pdf",dpi=300,width = 297, height = 210,units = "mm")
  ggsave("Figures/Figure1.png",dpi=300,width = 297, height = 210,units = "mm")
  
  library(ggnewscale)

  (ggplot(data_prod,aes(log10Biom,log10ProdB))+
      geom_mark_hull(aes(fill = Class), con.cap= 0, expand = 0, radius = 0, alpha = 0.3)+
      scale_fill_manual(values=group.colors,labels=c("Low Productivity/Biomass reefs","Productive reefs","High Biomass reefs"))+
      ggnewscale::new_scale_fill() +
      geom_point(shape=21,color="black",aes(size=gravtot2,fill = MarineEcosystemDependency), alpha = 0.6)+
      scale_fill_viridis_c(option="D") +
      scale_alpha_continuous(range = c(0.3, 1)) +
      labs(x="Biomass (g/m²) - log scale",
           y = "Productivity (%/year)",
           fill = "Marine Ecosystem Dependency",
           size = "Gravity") +
      scale_size(range = c(0.5,15))+
      theme_classic())
  
ggsave("Figures/Figure_gravity.pdf",dpi=300,width = 297, height = 210,units = "mm")
ggsave("Figures/Figure1_gravity.png",dpi=300,width = 297, height = 210,units = "mm")

data_prod$No.take.multizoned[data_prod$No.take.multizoned == "No Take"] = data_prod$No.take.multizoned[data_prod$No.take.multizoned == "No take"]
data_prod$No.take.multizoned[data_prod$No.take.multizoned == "No take "] = data_prod$No.take.multizoned[data_prod$No.take.multizoned == "No take"]

(ggplot(data_prod,aes(log10Biom,log10ProdB))+
    geom_mark_hull(aes(fill = Class), con.cap= 0, expand = 0, radius = 0, alpha = 0.3)+
    scale_fill_manual(values=group.colors,labels=c("Low Productivity/Biomass reefs","Productive reefs","High Biomass reefs"))+
    ggnewscale::new_scale_fill() +
    geom_point(size=3,alpha=0.4,shape=21,color="black",aes(fill=No.take.multizoned))+
    scale_fill_viridis_d()+
    labs(x="Biomass (g/m²) - log scale",
         y = "Productivity (%/year) - log scale",
         fill = "Protection status")+
    theme_classic())

ggsave("Figures/Figure1_mpa.pdf",dpi=300,width = 297, height = 210,units = "mm")
ggsave("Figures/Figure1_mpa.png",dpi=300,width = 297, height = 210,units = "mm")

}
