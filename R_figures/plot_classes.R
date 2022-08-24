#' Plotting various representations of our biplot of biomass/productivity
#' 
#' @param data_prod output from the data_management function
#' 
#' @return Classic biplot used in figure 1, biplot according to protection status, and biplot according to gravity/marine ecosystem dependency used in Figure 4


plot_classes <- function(data_prod){
  
  data_prod = RLS_Management


  biom75 = quantile(data_prod$log10Biom,0.95)
  biom25 = quantile(data_prod$log10Biom,0.25)
  prod75 = quantile(data_prod$log10ProdB,0.75)
  prod25 = quantile(data_prod$log10ProdB,0.25)
  max_biom = max(data_prod$log10Biom)
  
  group.colors <- c(deadzone = "#d69d4e", partial = "#046c9a", pristine = "#C1DB60", transition = "lightblue")
  
  #Plotting the classes
  data_prod %>%
    # mutate(Class = ifelse(Class == "pristine","pristine",ifelse(Class == "partial","partial",ifelse(Class == "deadzone","deadzone","other")))) %>%
    mutate(Class = ifelse(Class == "pristine","pristine",ifelse(Class == "partial","partial",ifelse(Class =="deadzone","deadzone","other"))))%>%
  ggplot(aes(log10Biom,log10ProdB,colour=Class))+
    geom_segment(aes(x = biom75, xend=biom75, y =-0, yend=prod75),size = 1.5,colour = "black")+
    geom_segment(aes(x = biom25, xend=biom25, y =-0, yend=prod25),size = 1.5,colour = "black")+
    geom_segment(aes(x = 0, xend=biom25, y = prod25 , yend=prod25),size = 1.5,colour = "black")+
    geom_segment(aes(x = 0, xend=max_biom, y = prod75 , yend=prod75),size = 1.5,colour = "black")+
    # geom_segment(aes(x = 0, xend =3, y =-prod75, yend=prod75),size = 2,colour = "black")+
     geom_point(size=4,alpha=0.4)+
    theme_classic()+
     scale_colour_manual(values=c(other = "#022859",pristine = "#C1DB60",partial = "#046c9a",deadzone = "#d69d4e"),guide = "none") +
    theme(axis.text.x=element_text(colour="#022859"),
          axis.text.y = element_text(colour="#022859")) +
    theme(axis.text=element_text(size=20,face = "bold"))+
     labs(x=" ",
          y = " ")
  
  ggsave("figures/ICRS_framework_3.png",dpi=600,width = 297, height = 210,units = "mm")


  ggsave("Figures/Figure1.pdf",dpi=600,width = 297, height = 210,units = "mm")
  ggsave("Figures/Figure1.png",dpi=300,width = 297, height = 210,units = "mm")
  
  library(ggnewscale)

  (ggplot(data_prod,aes(log10Biom,log10ProdB))+
      geom_segment(aes(x = biom75, xend=biom75, y =-0, yend=prod75),size = 1.5,colour = "black")+
      geom_segment(aes(x = biom25, xend=biom25, y =-0, yend=prod75),size = 1.5,colour = "black")+
      geom_segment(aes(x = 0, xend=biom25, y = prod25 , yend=prod25),size = 1.5,colour = "black")+
      geom_segment(aes(x = 0, xend=max_biom, y = prod75 , yend=prod75),size = 1.5,colour = "black")+
      geom_point(shape=21,color="black",aes(size=gravtot2,fill = MarineEcosystemDependency), alpha = 0.6)+
      scale_fill_viridis_c(option="D") +
      scale_alpha_continuous(range = c(0.3, 1)) +
      labs(x=" ",
           y = " ",
           fill = "Marine Ecosystem Dependency",
           size = "Gravity") +
      scale_size(range = c(1,15))+
      theme_classic()+
      scale_colour_manual(values=c(other = "#022859",pristine = "#C1DB60",partial = "#046c9a",deadzone = "#d69d4e"),guide = "none") +
      theme(axis.text.x=element_text(colour="#022859"),
            axis.text.y = element_text(colour="#022859")) +
      theme(axis.text=element_text(size=20,face = "bold"))+
      theme(legend.text=element_text(size=20,colour = "#022859"),
            legend.title=element_text(size=20,colour = "#022859")))
  
  ggsave("figures/ICRS_gravity.png",dpi=600,width = 297, height = 210,units = "mm")
  
ggsave("Figures/Figure_gravity.pdf",dpi=300,width = 297, height = 210,units = "mm")


ggsave("Figures/Figure1_gravity.png",dpi=300,width = 350, height = 210,units = "mm")

data_prod$No.take.multizoned[data_prod$No.take.multizoned == "No Take"] = data_prod$No.take.multizoned[data_prod$No.take.multizoned == "No take"]
data_prod$No.take.multizoned[data_prod$No.take.multizoned == "No take "] = data_prod$No.take.multizoned[data_prod$No.take.multizoned == "No take"]

(ggplot(data_prod,aes(log10Biom,log10ProdB))+
    geom_vline(xintercept = biom75, linetype = "solid")+
    geom_vline(xintercept = biom25, linetype = "solid")+
    geom_hline(yintercept = prod75, linetype = "solid")+
    geom_hline(yintercept = prod25, linetype = "solid")+
    geom_point(size=3,alpha=0.4,shape=21,color="black",aes(fill=No.take.multizoned))+
    scale_fill_viridis_d()+
    labs(x="Biomass (g/m²) - log scale",
         y = "Biomass turnover - P/B (%/day)",
         fill = "Protection status")+
    theme_classic())

ggsave("Figures/Figure1_mpa.pdf",dpi=300,width = 297, height = 210,units = "mm")
ggsave("Figures/Figure1_mpa.png",dpi=300,width = 297, height = 210,units = "mm")

}
