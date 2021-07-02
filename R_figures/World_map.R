#-----------------Loading packages-------------------

pkgs <- c("cowplot", "googleway", "ggplot2", "ggrepel","here",
          "ggspatial", "libwgeom", "sf", "rnaturalearth", "rnaturalearthdata")
nip <- pkgs[!(pkgs %in% installed.packages())]
nip <- lapply(nip, install.packages, dependencies = TRUE)
ip   <- unlist(lapply(pkgs, require, character.only = TRUE, quietly = TRUE))

#----------------Loading results------------------------

path = (here::here("outputs"))
setwd(path)
files <- list.files(here::here("outputs"),pattern = ".RData")
data_list = lapply(files, load, .GlobalEnv)

#----------------PLOT WORLD---------------

setwd(here())

world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

(sites <- st_as_sf(management, coords = c("SiteLongitude", "SiteLatitude"), 
                   crs = 4326, agr = "constant"))

sites_pristine = sites %>% filter(Class == "pristine")
sites_partial  = sites %>% filter(Class == "partial")
sites_deadzone = sites %>% filter(Class == "deadzone")
sites_transition = sites %>% filter(Class == "transition")

group.colors <- c(deadzone = "#eccbae", partial = "#046c9a", pristine = "#d69d4e", transition = "#ABDDDE")

plot_pristine = ggplot(data = world) +
  geom_sf(fill = "black")+
  geom_sf(data = sites_pristine, size = 3, alpha = 0.8, aes(colour=Class))+
  scale_colour_manual(values=group.colors,labels=c("Degraded reefs","Productive reefs","Sanctuaries","Transitional reefs"))+
  theme_minimal()

plot_partial = ggplot(data = world) +
  geom_sf(fill = "black")+
  geom_sf(data = sites_partial, size = 3, alpha = 0.8, aes(colour=Class))+
  scale_colour_manual(values=group.colors,labels=c("Degraded reefs","Productive reefs","Sanctuaries","Transitional reefs"))+
  theme_minimal()

plot_deadzone = ggplot(data = world) +
  geom_sf(fill = "black")+
  geom_sf(data = sites_deadzone, size = 3, alpha = 0.8, aes(colour=Class))+
  scale_colour_manual(values=group.colors,labels=c("Degraded reefs","Productive reefs","Sanctuaries","Transitional reefs"))+
  theme_minimal()

plot_transition = ggplot(data = world) +
  geom_sf(fill = "black")+
  geom_sf(data = sites_transition, size = 3, alpha = 0.8, aes(colour=Class))+
  scale_colour_manual(values=group.colors,labels=c("Degraded reefs","Productive reefs","Sanctuaries","Transitional reefs"))+
  theme_minimal()

full = ggarrange(plot_pristine,plot_partial,plot_deadzone,plot_transition,legend = "none")

ggsave("figures/worldmap.pdf",height = 210, width = 297, units ="mm")
