#' Mapping management classes for all transects in the world
#' 
#' @param data_prod output from the data_management function
#' 
#' 
#' @return One figure of world map for each management class and one confounding all classes on one map
#' 

data_prod = RLS_Management

map_management <- function(data_prod,world){
  # WorldData <- map_data('world') %>% filter(region != "Antarctica") 
  # WorldData_df = WorldData %>% st_as_sf(coords = c("long","lat"),crs=4326)
  
  group.colors <- c(deadzone = "#d69d4e", partial = "#046c9a", pristine = "#C1DB60", transition = "lightblue")
  
  group.colors.notransition <- c(deadzone = "#d69d4e", partial = "#046c9a", pristine = "#C1DB60")
  
  plot_sites = ggplot() +
      geom_sf(data = world, fill = "#171F28", size = 0.2,color = "black") +
    # geom_map(data = WorldData, map = WorldData,
    #          aes(x = long, y = lat, group = group, map_id=region),
    #          colour = "black",fill="#171F28", size=0.2)+
    geom_point(data = data_prod, size = 3, alpha = 0.5, aes(x=SiteLongitude, y= SiteLatitude), colour = "darkred")+
      theme_classic() +
    # coord_map("rectangular", lat0=0, xlim=c(-180,180), ylim=c(-60, 90))+
      coord_sf(expand = F,crs=4326)+
   xlab("Longitude")+
  ylab("Latitude")  + 
     theme(axis.text=element_text(size=10),
                            axis.title=element_text(size=12,face="bold")) +
     ggsn::north(world,location = "bottomleft",anchor = c(x=-180,y=-50))

  ggsave(plot_sites, file = "figures/sites.png", width = 297, height = 210, units  = "mm")
  
  (plot_world = ggplot() +
      geom_sf(data = world, fill = "#171F28", size = 0.2,color = "black") +
    geom_point(data = filter(data_prod, Class != "transition"), size = 8, alpha = 0.7, aes(x=SiteLongitude, y= SiteLatitude, colour=Class))+
    scale_colour_manual(values=group.colors.notransition,labels=c("Low biomass/productivity","High productivity","High biomass"))+
      coord_sf(expand = F,crs=4326)+
      theme_classic() +   theme(legend.title = element_text(size=40),
                            legend.text= element_text(size=40))+
      guides(colour = guide_legend(override.aes = list(size=10)))+
      theme(legend.position = "top")+
      xlab("Longitude")+
      ylab("Latitude")  + 
      theme(axis.text=element_text(size=10),
            axis.title=element_text(size=12,face="bold")) +
      ggsn::north(world,location = "bottomleft",anchor = c(x=-180,y=-50)))
  
  ggsave(plot_world, file = "figures/map_world.png",width = 24.00, height = 15.75, units= "in",dpi= 600)
  ggsave(plot_world, file = "figures/map_world.pdf",width = 24.00, height = 15.75, units= "in",dpi= 600)
  
  sites_pristine = data_prod %>% filter(Class == "pristine")
  sites_partial  = data_prod %>% filter(Class == "partial")
  sites_deadzone = data_prod %>% filter(Class == "deadzone")
  sites_transition = data_prod %>% filter(Class == "transition")
  
  plot_pristine = ggplot() +
    geom_sf(data = world, fill = "#171F28", size = 0.2,color = "black") +
    geom_point(data = sites_pristine, size = 5, alpha = 0.8, aes(x=SiteLongitude, y= SiteLatitude, colour=Class))+
      scale_colour_manual(values=group.colors,labels=c("Low biomass/productivity","High productivity","High biomass","Mid-range"))+
    coord_sf(expand = F,crs=4326)+
    theme_classic() +   theme(legend.title = element_text(size=40),
                              legend.text= element_text(size=40),
                              axis.text=element_text(size=12),
                              axis.title=element_text(size=15,face="bold"))+
    guides(colour = guide_legend(override.aes = list(size=10)))+
      theme(legend.position = "top")+
    xlab("Longitude")+
    ylab("Latitude")  + 
    ggsn::north(world,location = "bottomleft",anchor = c(x=-180,y=-50))
    
  ggsave(plot_pristine, file = "figures/map_pristine.pdf",width = 24.00, height = 15.75, units= "in",dpi= 600)
  ggsave(plot_pristine, file = "figures/map_pristine.png",width = 24.00, height = 15.75, units= "in",dpi= 600)
  
  plot_partial = ggplot() +
    geom_sf(data = world, fill = "#171F28", size = 0.2,color = "black") +
    geom_point(data = sites_partial, size = 5, alpha = 0.8, aes(x=SiteLongitude, y= SiteLatitude, colour=Class))+
    scale_colour_manual(values=group.colors,labels=c("Low biomass/productivity","High productivity","High biomass","Mid-range"))+
    coord_map("rectangular", lat0=0, xlim=c(-180,180), ylim=c(-60, 90))+
    coord_sf(expand = F,crs=4326)+
    theme_classic() +   theme(legend.title = element_text(size=40),
                              legend.text= element_text(size=40),
                              axis.text=element_text(size=12),
                              axis.title=element_text(size=15,face="bold"))+
    guides(colour = guide_legend(override.aes = list(size=10)))+
    theme(legend.position = "top")+
    xlab("Longitude")+
    ylab("Latitude")  + 
    ggsn::north(world,location = "bottomleft",anchor = c(x=-180,y=-50))
  
  ggsave(plot_partial, file = "figures/map_partial.pdf",width = 24.00, height = 15.75, units= "in",dpi= 600)
  ggsave(plot_partial, file = "figures/map_partial.png",width = 24.00, height = 15.75, units= "in",dpi= 600)
  
  plot_deadzone = ggplot() +
    geom_sf(data = world, fill = "#171F28", size = 0.2,color = "black") +
    geom_point(data = sites_deadzone, size = 5, alpha = 0.8, aes(x=SiteLongitude, y= SiteLatitude, colour=Class))+
    scale_colour_manual(values=group.colors,labels=c("Low biomass/productivity","High productivity","High biomass","Mid-range"))+
      coord_sf(expand = F,crs=4326)+
      theme_classic() +   theme(legend.title = element_text(size=40),
                                legend.text= element_text(size=40),
                                axis.text=element_text(size=12),
                                axis.title=element_text(size=15,face="bold"))+
      guides(colour = guide_legend(override.aes = list(size=10)))+
      theme(legend.position = "top")+
      xlab("Longitude")+
      ylab("Latitude")  + 
      ggsn::north(world,location = "bottomleft",anchor = c(x=-180,y=-50))
  
  
  ggsave(plot_deadzone, file = "figures/map_deadzone.pdf",width = 24.00, height = 15.75, units= "in",dpi= 600)
  ggsave(plot_deadzone, file = "figures/map_deadzone.png",width = 24.00, height = 15.75, units= "in",dpi= 600)
  
  plot_transition = ggplot() +
    geom_sf(data = world, fill = "#171F28", size = 0.2,color = "black") +
    geom_point(data = sites_transition, size = 5, alpha = 0.8, aes(x=SiteLongitude, y= SiteLatitude, colour=Class))+
    scale_colour_manual(values=group.colors,labels=c("Low biomass/productivity","High productivity","High biomass","Mid-range"))+
    coord_sf(expand = F,crs=4326)+
    theme_classic() +   theme(legend.title = element_text(size=40),
                              legend.text= element_text(size=40),
                              axis.text=element_text(size=12),
                              axis.title=element_text(size=15,face="bold"))+
    guides(colour = guide_legend(override.aes = list(size=10)))+
    theme(legend.position = "top")+
    xlab("Longitude")+
    ylab("Latitude")  + 
    ggsn::north(world,location = "bottomleft",anchor = c(x=-180,y=-50))
  
  ggsave(plot_transition, file = "figures/map_transition.pdf",width = 24.00, height = 15.75, units= "in",dpi= 600)
  ggsave(plot_transition, file = "figures/map_transition.png",width = 24.00, height = 15.75, units= "in",dpi= 600)

  ggarrange(plot_pristine,plot_partial, plot_deadzone,plot_transition,common.legend=T, ncol = 1 ,font.label = list(size = 14, color = "black", face = "bold", family = NULL))
  
  ggsave(file = "figures/Maps.pdf",width = 24.00, height = 15.75, units= "in",dpi= 600)
  ggsave(file = "figures/Maps.png",width = 24.00, height = 15.75, units= "in",dpi= 600)
}
