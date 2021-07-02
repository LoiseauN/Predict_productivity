#' Pooling productivity and biomass by transect
#' 
#' @param prod_data data to pool
#' 
#' 
#' @return data with productivity biomass for each transect
#' @export
#' 

K_by_size = function(data_prod){
  
  data_plot = data_prod %>%
    group_by(Species)%>%
    mutate(K = mean(K_pred),
           Family = as.factor(Family),
           Species = as.factor(Species))%>%
    distinct(K,.keep_all=T)
  
  sapply(data_plot,typeof)
  
  mycolors <- colorRampPalette(wes_palette("Darjeeling1", 27, type = "continuous"))(55)
  
  (p = ggscatter(data_plot,x="Size.max",y="K_pred",
            color="Family",
            palette = mycolors,
            size=3,alpha=0.6)+
    border()+geom_smooth(se=F,color="black")+    
    labs(
    y="Estimated growth rate K",
    x="Maximum observed size (cm)")+
    theme(legend.position = "top", legend.key.width=unit(3,"cm")))
    
  ggsave("Figures/K_by_size.pdf",height=210,width=297, units = "mm")
  ggsave("Figures/K_by_size.png",height=210,width=297, units = "mm")
  
  #Details for each Family and Genus 
  
  data_lutjanidae = data_prod %>%
    filter(Family == "Lutjanidae")%>%
    group_by(Species)%>%
    mutate(K_pred2 = mean(K_pred))%>%
    ungroup()
  
  data_labridae = data_prod %>%
    filter(Family == "Labridae")%>%
    group_by(Species)%>%
    mutate(K_pred2 = mean(K_pred))%>%
    ungroup()
  
  (p_lutjanidae = ggscatter(data_lutjanidae,x="Size.max",y="K_pred2",
                 palette = mycolors,
                 size=3,alpha=0.6)+
      border()+ stat_smooth(method = lm, formula = y ~ log(x)) +
    add_fishape(family="Lutjanidae",option="Lutjanus_gibbus",
                xmin=140,xmax=160,ymin=0.75,ymax=1,fill="darkblue",alpha=0.7)+
    labs(title="Lutjanidae",
         y="Estimated growth rate K",
         x="Maximum observed size (cm)"))
  
  (p_labridae = ggscatter(data_labridae,x="Size.max",y="K_pred2",
                 palette = mycolors,
                 size=3,alpha=0.6)+
      add_fishape(family="Labridae",option="Epibulus_insidiator",
                  xmin=200,xmax=230,ymin=5,ymax=8,fill="darkblue",alpha=0.7)+
      border()+ stat_smooth(method = lm, formula = y ~ log(x)) +
    labs(title="Labridae",
         y="Estimated growth rate K",
         x="Maximum observed size (cm)"))
  
  p_family = ggarrange(p_lutjanidae,p_labridae)
  
  data_lutjanus = data_prod %>%
    filter(Genus == "Lutjanus")%>%
    group_by(Species)%>%
    mutate(K_pred2 = mean(K_pred))%>%
    ungroup()
  
  data_bodianus = data_prod %>%
    filter(Genus == "Cheilinus")%>%
    group_by(Species)%>%
    mutate(K_pred2 = mean(K_pred))%>%
    ungroup()
  
  (p_lutjanus = ggscatter(data_lutjanus,x="Size.max",y="K_pred2",
                 palette = pal,
                 size=3,alpha=0.6)+
      border()+ stat_smooth(method = lm, formula = y ~ log(x)) +
    add_fishape(family="Lutjanidae",option="Lutjanus_gibbus",
                xmin=140,xmax=160,ymin=0.8,ymax=1,fill="darkblue",alpha=0.7)+
    labs(title="Lutjanus",
         y="Estimated growth rate K",
         x="Maximum observed size (cm)"))
  
  test  = data_prod %>%
    filter(Family == "Labridae")
  
  (p_bodianus = ggscatter(data_bodianus,x="Size.max",y="K_pred2",
                 palette = pal,
                 size=3,alpha=0.6)+
      border()+ stat_smooth(method = lm, formula = y ~ log(x))+
    labs(title="Cheilinus",
         y="Estimated growth rate K",
         x="Maximum observed size (cm)")+
    add_fishape(family="Labridae",option="Epibulus_insidiator",
                xmin=190,xmax=220,ymin=3,ymax=4,fill="darkblue",alpha=0.7))
  
  p_genus = ggarrange(p_lutjanus,p_bodianus)
  
  ggarrange(p_family,p_genus,ncol=1)
  
  ggsave("Figures/K_by_size_details.png",height = 210,width=297,units="mm")
  ggsave("Figures/K_by_size_details.pdf",height = 210,width=297,units="mm")
  
  
}