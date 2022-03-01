#' PVarious figures with K by size, K by family, K for example families and genus
#' 
#' @param prod_data Community level productivity for the 3714 transects
#' 
#' 
#' @return Various figures in the figure folder
#' 

K_plots = function(data_prod){

  
  data_plot = data_prod %>%
    group_by(Species)%>%
    mutate(K = log(mean(K_pred)+1),
           Family = as.factor(Family),
           Species = as.factor(Species))%>%
    distinct(K,.keep_all=T)
  
  
  mycolors <- colorRampPalette(wes_palette("Darjeeling1", 27, type = "continuous"))(138)
  
  (p = ggscatter(data_plot,x="MaxLength",y="K",
            color="Family",
            palette = mycolors,
            size=3,alpha=0.6)+
    border()+ 
    labs(
    y="Estimated growth rate K (log scale)",
    x="Maximum observed size (cm)")+
    theme(legend.position = "none", legend.key.width=unit(3,"cm")) + stat_smooth(method = lm, formula = y ~ log(x),se=F,))
    
  ggsave("Figures/K_by_size.pdf",height=210,width=297, units = "mm")
  ggsave("Figures/K_by_size.png",height=210,width=297, units = "mm")

  #Details for each Family and Genus 
  unique(data_prod$Family)
  
  data_Serranidae = data_prod %>%
    filter(Family == "Serranidae")%>%
    group_by(Species)%>%
    mutate(K_pred2 = mean(K_pred))%>%
    ungroup()
  
  data_labridae = data_prod %>%
    filter(Family == "Labridae")%>%
    group_by(Species)%>%
    mutate(K_pred2 = mean(K_pred))%>%
    ungroup()
  
  (p_Serranidae = ggscatter(data_Serranidae,x="MaxLength",y="K_pred2",
                 palette = mycolors,
                 size=3,alpha=0.6)+
      border()+ stat_smooth(method = lm, formula = y ~ log(x)) +
    # add_fishape(family="Lutjanidae",option="Lutjanus_gibbus",
    #             xmin=140,xmax=160,ymin=0.75,ymax=1,fill="darkblue",alpha=0.7)+
    labs(title="Serranidae",
         y="Estimated growth rate K",
         x="Maximum observed size (cm)"))
  
  (p_labridae = ggscatter(data_labridae,x="MaxLength",y="K_pred2",
                 palette = mycolors,
                 size=3,alpha=0.6)+
      # add_fishape(family="Labridae",option="Epibulus_insidiator",
      #             xmin=200,xmax=230,ymin=5,ymax=8,fill="darkblue",alpha=0.7)+
      border()+ stat_smooth(method = lm, formula = y ~ log(x)) +
    labs(title="Labridae",
         y="Estimated growth rate K",
         x="Maximum observed size (cm)"))
  
  (p_family = ggarrange(p_Serranidae,p_labridae))
  
  serranidae = data_prod %>% filter(Family == "Labridae")
  unique(serranidae$Genus)
  
  data_Epinephelus = data_prod %>%
    filter(Genus == "Epinephelus")%>%
    group_by(Species)%>%
    mutate(K_pred2 = mean(K_pred))%>%
    ungroup()
  
  data_Scarus = data_prod %>%
    filter(Genus == "Scarus")%>%
    group_by(Species)%>%
    mutate(K_pred2 = mean(K_pred))%>%
    ungroup()
  
  (p_Epinephelus = ggscatter(data_Epinephelus,x="MaxLength",y="K_pred2",
                 palette = mycolors,
                 size=3,alpha=0.6)+
      border()+ stat_smooth(method = lm, formula = y ~ log(x)) +
    # add_fishape(family="Lutjanidae",option="Lutjanus_gibbus",
    #             xmin=140,xmax=160,ymin=0.8,ymax=1,fill="darkblue",alpha=0.7)+
    labs(title="Epinephelus",
         y="Estimated growth rate K",
         x="Maximum observed size (cm)"))
  

  (p_Scarus= ggscatter(data_Scarus,x="MaxLength",y="K_pred2",
                 palette = mycolors,
                 size=3,alpha=0.6)+
      border()+ stat_smooth(method = lm, formula = y ~ log(x))+
    labs(title="Scarus",
         y="Estimated growth rate K",
         x="Maximum observed size (cm)"))
    # add_fishape(family="Labridae",option="Epibulus_insidiator",
    #             xmin=190,xmax=220,ymin=3,ymax=4,fill="darkblue",alpha=0.7))
  
  p_genus = ggarrange(p_Epinephelus,p_Scarus)
  
  ggarrange(p_family,p_genus,ncol=1)
  
  ggsave("Figures/K_by_size_details.png",height = 210,width=297,units="mm")
  ggsave("Figures/K_by_size_details.pdf",height = 210,width=297,units="mm")
  
  
  data_prod = RLS_prod_figures
  
  K_by_family = ggplot(data_prod,aes(reorder(Family,-K_pred),log(K_pred+1),fill=Family,colour=Family))+
      geom_jitter(size = 0.1, alpha = 0.2)+
      geom_boxplot(alpha = 0.7)+
      scale_fill_viridis_d()+
      scale_color_viridis_d()+ 
      theme_bw()+
      theme(legend.position = "none")+
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
      labs( y = "Estimated growth rate K (log scale)",
            x = "")
  
  ggsave(K_by_family,"Figures/K_by_family.pdf",height=210,width=297, units = "mm")
  ggsave("Figures/K_by_family.png",height=210,width=297, units = "mm")
  
}
