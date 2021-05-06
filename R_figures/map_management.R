#' Pooling productivity and biomass by transect
#' 
#' @param prod_data data with management classes to plot
#' 
#' 
#' @return data with productivity biomass for each transect
#' @export
#' 

map_management <- function(data_prod,map_shp){
  
  sites <- st_as_sf(data_prod, coords = c("Longitude", "Latitude"), 
                    crs = 4326, agr = "constant")
  
  group.colors <- c(deadzone = "#eccbae", partial = "#046c9a", pristine = "#d69d4e", transition = "#ABDDDE")
  
  full_map <- sites %>%
    filter(Class != "transition")
  
  (map_full = 
      ggplot(map_shp) +
      geom_sf(fill = "#CAE4EF") +
      geom_sf(data=full_map,size=4,alpha=0.7,aes(colour=Class))+
      scale_colour_manual(values=group.colors,labels=c("Low Productivity/Biomass","High Productivity","High Biomass"))+
      theme(panel.background = element_rect(fill = "white"),axis.ticks = element_blank(),
            axis.text = element_blank(),
            panel.grid = element_line(color = "white", size = 0.8),
            panel.border = element_rect(colour = "white", fill=NA, size=1))+
      theme(plot.margin=grid::unit(c(0,0,0,0), "mm")))
  
  map_dead = sites %>%
    filter(Class=="deadzone")
  
  
  map_partial = sites %>%
    filter(Class=="partial")
  
  
  map_prisine = sites %>%
    filter(Class=="pristine")
  
  
  #Classes maps
  Class_dead = 
    ggplot(map_shp) +
    geom_sf(fill = "#CAE4EF") +
    geom_sf(data=map_dead,size=4,alpha=0.7,colour="#eccbae")+
    theme(panel.background = element_rect(fill = "white"),axis.ticks = element_blank(),
          axis.text = element_blank(),
          panel.grid = element_line(color = "white", size = 0.8),
          panel.border = element_rect(colour = "#CAE4EF", fill=NA, size=1))+
    theme(plot.margin=grid::unit(c(0,0,0,0), "mm"))

  
  #Classes
  Class_partial = 
    ggplot(map_shp) +
    geom_sf(fill = "#CAE4EF") +
    geom_sf(data=map_partial,size=4,alpha=0.7,colour="#046c9a")+
    theme(panel.background = element_rect(fill = "white"),axis.ticks = element_blank(),
          axis.text = element_blank(),
          panel.grid = element_line(color = "white", size = 0.8),
          panel.border = element_rect(colour = "#CAE4EF", fill=NA, size=1))+
    theme(plot.margin=grid::unit(c(0,0,0,0), "mm"))

  
  #Classes
  Class_pristine = 
    ggplot(map_shp) +
    geom_sf(fill = "#CAE4EF") +
    geom_sf(data=map_prisine,size=4,alpha=0.7,colour="#d69d4e")+
    theme(panel.background = element_rect(fill = "white"),axis.ticks = element_blank(),
          axis.text = element_blank(),
          panel.grid = element_line(color = "white", size = 0.8),
          panel.border = element_rect(colour = "#CAE4EF", fill=NA, size=1))+
    theme(plot.margin=grid::unit(c(0,0,0,0), "mm"))
  
  
  #Loading images
  img_pristine = readPNG("graphics/pristine.png")
  img_partial = readPNG("graphics/partial.png")
  img_dead = readPNG("graphics/dead.png")
  
  im_pr = ggplot()+
    background_image(img_pristine)+
    theme(plot.margin = margin(t=1, l=1, r=1, b=1, unit = "cm"))
  
  im_pa = ggplot()+
    background_image(img_partial)+
    theme(plot.margin = margin(t=1, l=1, r=1, b=1, unit = "cm"))
  
  im_d = ggplot()+
    background_image(img_dead)+
    theme(plot.margin = margin(t=1, l=1, r=1, b=1, unit = "cm"))
  
  #full map 
  
  img = ggarrange(im_pr,
                   im_pa,
                   im_d,nrow=1,widths = 6,heights = 5)
  
  classes = ggarrange(Class_pristine,Class_partial,Class_dead,nrow=1)
  
  final_map = ggarrange(map_full,classes,img,nrow=3,ncol=1,heights = c(2,1,1))
  
  ggsave("Figures/final_map.pdf",height=12,width=16)
  ggsave("Figures/final_map.png",height=12,width=16)
  
}