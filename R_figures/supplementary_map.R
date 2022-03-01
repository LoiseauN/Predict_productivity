#' Mapping management classes for all transects in the world
#' 
#' @param data_prod output from the data_management function
#' 
#' 
#' @return One figure of world map for each management class and one confounding all classes on one map
#' 

map_supplementary <- function(data_prod){
  
  data_prod = RLS_Management
  
  WorldData <- map_data('world') %>% filter(region != "Antarctica")
  
  plot_world = ggplot() +
      geom_map(data = WorldData, map = WorldData,
               aes(x = long, y = lat, group = group, map_id=region),
               colour = "black",fill="#171F28", size=0.2)+
      geom_point(data = data_prod, size = 4, alpha = 0.5, aes(x=SiteLongitude, y= SiteLatitude, colour = "#E69F00"))+
      coord_map("rectangular", lat0=0, xlim=c(-180,180), ylim=c(-60, 90))+
      theme(legend.key.size = unit(2, 'cm'))+
      theme_void() +
      labs(color = "Surveyed sites")
  
  ggsave(plot_world, file = "figures/map_world.png",width = 24.00, height = 15.75, units= "in",dpi= 600)
  ggsave(plot_world, file = "figures/map_world.pdf",width = 24.00, height = 15.75, units= "in",dpi= 600)
  
}
