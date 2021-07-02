#' Pooling productivity and biomass by transect
#' 
#' @param prod_data data with management classes to plot
#' 
#' 
#' @return data with productivity biomass for each transect
#' @export
#' 

map_management <- function(data_prod){
  
  WorldData <- map_data('world') %>% filter(region != "Antarctica")
  
  group.colors <- c(deadzone = "#d69d4e", partial = "#046c9a", pristine = "#C1DB60", transition = "#ABDDDE")
  
  sites_pristine = data_prod %>% filter(Class == "pristine")
  sites_partial  = data_prod %>% filter(Class == "partial")
  sites_deadzone = data_prod %>% filter(Class == "deadzone")
  sites_transition = data_prod %>% filter(Class == "transition")
  
  plot_pristine = ggplot() +
    geom_map(data = WorldData, map = WorldData,
             aes(x = long, y = lat, group = group, map_id=region),
             colour = "black",fill="#171F28", size=0.2)+
    geom_point(data = sites_pristine, size = 3, alpha = 0.8, aes(x=SiteLongitude, y= SiteLatitude, colour=Class))+
    scale_colour_manual(values=group.colors,labels=c("Degraded reefs","Productive reefs","Sanctuaries","Transitional reefs"))+
    coord_map("rectangular", lat0=0, xlim=c(-180,180), ylim=c(-60, 90))+
    theme_void()
  
  ggsave(plot_pristine, file = "figures/map_pristine.pdf",width = 24.00, height = 15.75, units= "in",dpi= 600)
  
  plot_partial = ggplot() +
    geom_map(data = WorldData, map = WorldData,
             aes(x = long, y = lat, group = group, map_id=region),
             colour = "black", size=0.2)+
    geom_point(data = sites_partial, size = 3, alpha = 0.8, aes(x=SiteLongitude, y= SiteLatitude, colour=Class))+
    scale_colour_manual(values=group.colors,labels=c("Degraded reefs","Productive reefs","Sanctuaries","Transitional reefs"))+
    coord_map("rectangular", lat0=0, xlim=c(-180,180), ylim=c(-60, 90))+
    theme_void()
  
  ggsave(plot_partial, file = "figures/map_partial.pdf",width = 24.00, height = 15.75, units= "in",dpi= 600)
  
  plot_deadzone = ggplot() +
    geom_map(data = WorldData, map = WorldData,
             aes(x = long, y = lat, group = group, map_id=region),
             colour = "black", size=0.2)+
    geom_point(data = sites_deadzone, size = 3, alpha = 0.8, aes(x=SiteLongitude, y= SiteLatitude, colour=Class))+
    scale_colour_manual(values=group.colors,labels=c("Degraded reefs","Productive reefs","Sanctuaries","Transitional reefs"))+
    coord_map("rectangular", lat0=0, xlim=c(-180,180), ylim=c(-60, 90))+
    theme_void()
  
  ggsave(plot_deadzone, file = "figures/map_deadzone.pdf",width = 24.00, height = 15.75, units= "in",dpi= 600)
  
  plot_transition = ggplot() +
    geom_map(data = WorldData, map = WorldData,
             aes(x = long, y = lat, group = group, map_id=region),
             colour = "black", size=0.2)+
    geom_point(data = sites_transition, size = 3, alpha = 0.8, aes(x=SiteLongitude, y= SiteLatitude, colour=Class))+
    scale_colour_manual(values=group.colors,labels=c("Degraded reefs","Productive reefs","Sanctuaries","Transitional reefs"))+
    coord_map("rectangular", lat0=0, xlim=c(-180,180), ylim=c(-60, 90))+
    theme_void()
  
  ggsave(plot_transition, file = "figures/map_transition.pdf",width = 24.00, height = 15.75, units= "in",dpi= 600)
  
  beep(sound=4)
  
}