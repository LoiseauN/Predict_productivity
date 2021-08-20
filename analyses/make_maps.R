# ======================================================================================
# MAP PRODUCTIVITY
# ======================================================================================
data_prod_brut <- RLS_prod


# __________ Load ready to use data from GitHub
load(url("https://github.com/valentinitnelav/RandomScripts/blob/master/NaturalEarth.RData?raw=true"))
# This will load 6 objects:
# xbl.X &amp;amp; lbl.Y are two data.frames that contain labels for graticule lines
# They can be created with the code at this link:
# https://gist.github.com/valentinitnelav/8992f09b4c7e206d39d00e813d2bddb1
# NE_box is a SpatialPolygonsDataFrame object and represents a bounding box for Earth
# NE_countries is a SpatialPolygonsDataFrame object representing countries
# NE_graticules is a SpatialLinesDataFrame object that represents 10 dg latitude lines and 20 dg longitude lines
# (for creating graticules check also the graticule package or gridlines fun. from sp package)
# (or check this gist: https://gist.github.com/valentinitnelav/a7871128d58097e9d227f7a04e00134f)
# NE_places - SpatialPointsDataFrame with city and town points
# NOTE: data downloaded from http://www.naturalearthdata.com/
# here is a sample script how to download, unzip and read such shapefiles:
# https://gist.github.com/valentinitnelav/a415f3fbfd90f72ea06b5411fb16df16

# __________ Project from long-lat (unprojected data) to Robinson projection
# spTransform() is used for shapefiles and project() in the case of data frames
# for more PROJ.4 strings check the followings
# http://proj4.org/projections/index.html
# https://epsg.io/

PROJ<- "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
# or use the short form "+proj=robin"
NE_countries_rob<- spTransform(NE_countries, CRSobj = PROJ)
NE_graticules_rob<- spTransform(NE_graticules, CRSobj = PROJ)
NE_box_rob<- spTransform(NE_box, CRSobj = PROJ)

# Add lakes http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/10m/physical/ne_10m_lakes.zip

download.file("http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/10m/physical/ne_10m_lakes.zip", 
              destfile=file.path("~/Documents/Postdoc MARBEC/REEF-FUTURE/Productivity_RLS/Last VERSION/Predict_productivity/data","ne_10m_lakes.zip"))
unzip(file.path("~/Documents/Postdoc MARBEC/REEF-FUTURE/Productivity_RLS/Last VERSION/Predict_productivity/data","ne_10m_lakes.zip"))
NE_lakes<- rgdal::readOGR('ne_10m_lakes.shp',
                   'ne_10m_lakes')
NE_lakes<- spTransform(NE_lakes, CRSobj = PROJ)
NE_lakes<- NE_lakes[NE_lakes@data$scalerank==0,]

# project long-lat coordinates for graticule label data frames
# (two extra columns with projected XY are created)
prj.coord<- rgdal::project(cbind(lbl.Y$lon, lbl.Y$lat), proj=PROJ)
lbl.Y.prj<- cbind(prj.coord, lbl.Y)
names(lbl.Y.prj)[1:2]<- c("X.prj","Y.prj")

prj.coord<- rgdal::project(cbind(lbl.X$lon, lbl.X$lat), proj=PROJ)
lbl.X.prj<- cbind(prj.coord, lbl.X)
names(lbl.X.prj)[1:2]<- c("X.prj","Y.prj")

# Project ProdB_gcd sites

ProdB_gcd_rob=rgdal::project(cbind(as.numeric(as.character(data_prod_brut$SiteLongitude)),as.numeric(as.character(data_prod_brut$SiteLatitude))),PROJ)
ProdB_gcd=data.frame(long=ProdB_gcd_rob[,1],lat=ProdB_gcd_rob[,2],Productivity=data_prod_brut$log10ProdB,Production=log10(data_prod_brut$Prod+1),Biomass=log10(data_prod_brut$Biom+1))
ProdB_gcd <- subset(ProdB_gcd,ProdB_gcd$Productivity>0)
ProdB_gcd <- subset(ProdB_gcd,ProdB_gcd$Production>0)

# __________ Plot Productivity
mapProductivity <- ggplot() +
  # add Natural Earth countries projected to Robinson, give black border and fill with gray
  geom_polygon(data=NE_box_rob, aes(x=long, y=lat), colour="black",  fill="skyblue", alpha=0.4) +
  geom_polygon(data=NE_countries_rob, aes(long,lat, group=group), colour="black", fill="white", size = 0.25) +
  geom_polygon(data=NE_lakes, aes(long,lat, group=group), colour="black", fill="white", size = 0.25) +
  # Note: "Regions defined for each Polygons" warning has to do with fortify transformation. Might get deprecated in future!
  # alternatively, use use map_data(NE_countries) to transform to data frame and then use project() to change to desired projection.
  # add Natural Earth box projected to Robinson
  geom_polygon(data=NE_box_rob, aes(x=long, y=lat), colour="black", fill="transparent", size = 0.25) +
  # add graticules projected to Robinson
  geom_path(data=NE_graticules_rob, aes(long, lat, group=group), linetype="dotted", color="grey50", size = 0.25) +
  # add graticule labels - latitude and longitude
  geom_text(data = lbl.Y.prj, aes(x = X.prj, y = Y.prj, label = lbl), color="grey50", size=2) +
  geom_text(data = lbl.X.prj, aes(x = X.prj, y = Y.prj, label = lbl), color="grey50", size=2) +
  # the default, ratio = 1 in coord_fixed ensures that one unit on the x-axis is the same length as one unit on the y-axis
  geom_point(data=ProdB_gcd, aes(long,lat, col= Productivity))+
  harrypotter::scale_color_hp(house = "Gryffindor",direction=-1)+
  #scale_color_gradientn(colors = c("#00AFBB", "#E7B800", "#FC4E07"))+
  coord_fixed(ratio = 1) +
  # remove the background and default gridlines
  theme_void()+
  theme(legend.title = element_text(colour="black", size=8, face="bold"), # adjust legend title
        legend.position = c(0.1, 0.2), # relative position of legend
        plot.margin = unit(c(t=0, r=0, b=0, l=0), unit="cm"),
        legend.background = element_rect(fill="white",
                                         size=0.5, linetype="solid",
                                         colour ="black")) # adjust margins



# __________ Plot Production
mapProduction<-ggplot() +
  # add Natural Earth countries projected to Robinson, give black border and fill with gray
  geom_polygon(data=NE_box_rob, aes(x=long, y=lat), colour="black", fill="skyblue", alpha=0.4) +
  geom_polygon(data=NE_countries_rob, aes(long,lat, group=group), colour="black", fill="white", size = 0.25) +
  geom_polygon(data=NE_lakes, aes(long,lat, group=group), colour="black", fill="white", size = 0.25) +
  # Note: "Regions defined for each Polygons" warning has to do with fortify transformation. Might get deprecated in future!
  # alternatively, use use map_data(NE_countries) to transform to data frame and then use project() to change to desired projection.
  # add Natural Earth box projected to Robinson
  geom_polygon(data=NE_box_rob, aes(x=long, y=lat), colour="black", fill="transparent", size = 0.25) +
  # add graticules projected to Robinson
  geom_path(data=NE_graticules_rob, aes(long, lat, group=group), linetype="dotted", color="grey50", size = 0.25) +
  # add graticule labels - latitude and longitude
  geom_text(data = lbl.Y.prj, aes(x = X.prj, y = Y.prj, label = lbl), color="grey50", size=2) +
  geom_text(data = lbl.X.prj, aes(x = X.prj, y = Y.prj, label = lbl), color="grey50", size=2) +
  # the default, ratio = 1 in coord_fixed ensures that one unit on the x-axis is the same length as one unit on the y-axis
  geom_point(data=ProdB_gcd, aes(long,lat, col= Production))+
  #scale_color_gradientn(colors = c("#00AFBB", "#E7B800", "#FC4E07"))+
  harrypotter::scale_color_hp(house = "Gryffindor",direction=-1)+
  coord_fixed(ratio = 1) +
  # remove the background and default gridlines
  theme_void()+
  theme(legend.title = element_text(colour="black", size=8, face="bold"), # adjust legend title
        legend.position = c(0.1, 0.2), # relative position of legend
        plot.margin = unit(c(t=0, r=0, b=0, l=0), unit="cm"),
        legend.background = element_rect(fill="white",
                                         size=0.5, linetype="solid",
                                         colour ="black"))


# __________ Plot Biomass
mapBiomass<-ggplot() +
  # add Natural Earth countries projected to Robinson, give black border and fill with gray
  geom_polygon(data=NE_box_rob, aes(x=long, y=lat), colour="black", fill="skyblue", alpha=0.4) +
  geom_polygon(data=NE_countries_rob, aes(long,lat, group=group), colour="black", fill="white", size = 0.25) +
  geom_polygon(data=NE_lakes, aes(long,lat, group=group), colour="black", fill="white", size = 0.25) +
  # Note: "Regions defined for each Polygons" warning has to do with fortify transformation. Might get deprecated in future!
  # alternatively, use use map_data(NE_countries) to transform to data frame and then use project() to change to desired projection.
  # add Natural Earth box projected to Robinson
  geom_polygon(data=NE_box_rob, aes(x=long, y=lat), colour="black", fill="transparent", size = 0.25) +
  # add graticules projected to Robinson
  geom_path(data=NE_graticules_rob, aes(long, lat, group=group), linetype="dotted", color="grey50", size = 0.25) +
  # add graticule labels - latitude and longitude
  geom_text(data = lbl.Y.prj, aes(x = X.prj, y = Y.prj, label = lbl), color="grey50", size=2) +
  geom_text(data = lbl.X.prj, aes(x = X.prj, y = Y.prj, label = lbl), color="grey50", size=2) +
  # the default, ratio = 1 in coord_fixed ensures that one unit on the x-axis is the same length as one unit on the y-axis
  geom_point(data=ProdB_gcd, aes(long,lat, col= Biomass))+
  harrypotter::scale_color_hp(house = "Gryffindor",direction=-1)+
  #scale_color_gradientn(colors = c("#00AFBB", "#E7B800", "#FC4E07"))+
  coord_fixed(ratio = 1) +
  # remove the background and default gridlines
  theme_void()+
  theme(legend.title = element_text(colour="black", size=8, face="bold"), # adjust legend title
        legend.position = c(0.1, 0.2), # relative position of legend
        plot.margin = unit(c(t=0, r=0, b=0, l=0), unit="cm"),
        legend.background = element_rect(fill="white",
                                         size=0.5, linetype="solid",
                                         colour ="black")) # adjust margins

map <- gridExtra::grid.arrange(mapProductivity,mapProduction,mapBiomass,nrow=3)
