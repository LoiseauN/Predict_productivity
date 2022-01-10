#' Mapping management classes for all transects in the world
#' 
#' @param data_prod output from the data_management function
#' 
#' 
#' @return One figure of world map for each management class and one confounding all classes on one map
#' 

map_management <- function(data_prod){
  
  data_prod = RLS_Management
  
  WorldData <- map_data('world') %>% filter(region != "Antarctica")
  
  group.colors <- c(deadzone = "#d69d4e", partial = "#046c9a", pristine = "#C1DB60")
  
  (plot_world = ggplot() +
    geom_map(data = WorldData, map = WorldData,
             aes(x = long, y = lat, group = group, map_id=region),
             colour = "black",fill="#171F28", size=0.2)+
    geom_point(data = data_prod, size = 4, alpha = 0.5, aes(x=SiteLongitude, y= SiteLatitude, colour=Class))+
    scale_colour_manual(values=group.colors,labels=c("Low biomass/productivity","High productivity","High biomass","Mid-range"))+
    coord_map("rectangular", lat0=0, xlim=c(-180,180), ylim=c(-60, 90))+
      theme(legend.key.size = unit(2, 'cm'))+
    theme_void())
  
  ggsave(plot_world, file = "figures/map_world.png",width = 24.00, height = 15.75, units= "in",dpi= 600)
  ggsave(plot_world, file = "figures/map_world.pdf",width = 24.00, height = 15.75, units= "in",dpi= 600)
  
  sites_pristine = data_prod %>% filter(Class == "pristine")
  sites_partial  = data_prod %>% filter(Class == "partial")
  sites_deadzone = data_prod %>% filter(Class == "deadzone")
  
  (plot_pristine = ggplot() +
    geom_map(data = WorldData, map = WorldData,
             aes(x = long, y = lat, group = group, map_id=region),
             colour = "black",fill="#171F28", size=0.2)+
    geom_point(data = sites_pristine, size = 3, alpha = 0.8, aes(x=SiteLongitude, y= SiteLatitude, colour=Class))+
      scale_colour_manual(values=group.colors,labels=c("Low biomass/productivity","High productivity","High biomass","Mid-range"))+
    coord_map("rectangular", lat0=0, xlim=c(-180,180), ylim=c(-60, 90))+
    theme_void()+   theme(legend.title = element_text(size=40),
                          legend.text= element_text(size=40))+
     guides(colour = guide_legend(override.aes = list(size=10))))
    
  ggsave(plot_pristine, file = "figures/map_pristine.pdf",width = 24.00, height = 15.75, units= "in",dpi= 600)
  
  plot_partial = ggplot() +
    geom_map(data = WorldData, map = WorldData,
             aes(x = long, y = lat, group = group, map_id=region),
             colour = "black", size=0.2)+
    geom_point(data = sites_partial, size = 3, alpha = 0.8, aes(x=SiteLongitude, y= SiteLatitude, colour=Class))+
    scale_colour_manual(values=group.colors,labels=c("Low biomass/productivity","High productivity","High biomass","Mid-range"))+
    coord_map("rectangular", lat0=0, xlim=c(-180,180), ylim=c(-60, 90))+
    theme_void() +
    guides(colour = guide_legend(override.aes = list(size=10)))
  
  ggsave(plot_partial, file = "figures/map_partial.pdf",width = 24.00, height = 15.75, units= "in",dpi= 600)
  
  plot_deadzone = ggplot() +
    geom_map(data = WorldData, map = WorldData,
             aes(x = long, y = lat, group = group, map_id=region),
             colour = "black", size=0.2)+
    geom_point(data = sites_deadzone, size = 3, alpha = 0.8, aes(x=SiteLongitude, y= SiteLatitude, colour=Class))+
    scale_colour_manual(values=group.colors,labels=c("Low biomass/productivity","High productivity","High biomass","Mid-range"))+
    coord_map("rectangular", lat0=0, xlim=c(-180,180), ylim=c(-60, 90))+
    theme_void() + theme(legend.title = element_text(size=40),
                         legend.text= element_text(size=40))+
    guides(colour = guide_legend(override.aes = list(size=10)))
  
  ggsave(plot_deadzone, file = "figures/map_deadzone.pdf",width = 24.00, height = 15.75, units= "in",dpi= 600)

  
  ggarrange(plot_pristine,plot_partial, plot_deadzone,common.legend=T, ncol = 1 ,font.label = list(size = 14, color = "black", face = "bold", family = NULL))
  
  ggsave(file = "figures/Maps.pdf",width = 24.00, height = 15.75, units= "in",dpi= 600)
  
}