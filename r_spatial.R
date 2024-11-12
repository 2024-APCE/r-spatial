# Spatial analysis in R
# Han Olff nov 2021

rm(list = ls())
# set the working directoryes
setwd("G:/My Drive/Teaching/Courses/APCE/APCE_2022_2023/APCE2022GIS")

#remotes::install_github("https://github.com/rhijmans/leaflet")
library(leaflet)

#install.packages('terra', repos='https://rspatial.r-universe.dev')
library(terra)       # for working with raster data
library(tidyterra)   # for adding terra objects to ggplot
library(sf)          # for vector data objects
library(tidyverse)   # ggplot, dplyr etc
library(scales)      # for oob (out of bounds) scale
library(ggnewscale) # for using multiple color fill scales in ggplot
library(gridExtra)  # for combining multiple ggplots in one panel plot

# explore color palettes
# also see https://www.datanovia.com/en/blog/top-r-color-palettes-to-know-for-great-data-visualization/
# Base R palettes
barplot(rep(1,10), col = grey.colors(10))
barplot(rep(1,10), col = rev(topo.colors(10))) # rev turns the scale arround
barplot(rep(1,10), col = terrain.colors(10))
library(RColorBrewer) 
RColorBrewer::display.brewer.all()
barplot(rep(1,10), col = RColorBrewer::brewer.pal(10, "Spectral"))

barplot(rep(1,10), col = RColorBrewer::brewer.pal(10, "BrBG"))
library(viridis)
barplot(rep(1,10), col = viridis::viridis(10))
barplot(rep(1,10), col = viridis::plasma(10))
barplot(rep(1,10), col = viridis::heat(10))
viridis::plasma(10)
library(wesanderson)
barplot(rep(1,10), col = rev(wesanderson::wes_palette("Zissou1", 10, type = "continuous")))

# load our vector data 
sf::st_layers("./2022_protected_areas/protected_areas.gpkg")
boundaries<-sf::st_read(dsn="./2022_protected_areas/protected_areas.gpkg",
                        layer="protected_areas_2022") # read protected area boundaries
class(boundaries)
boundaries2<-terra::vect("./2022_protected_areas/protected_areas.gpkg",
                          layer="protected_areas_2022")
class(boundaries2)
studyarea<-sf::st_read(dsn="./studyarea/studyarea.shp")
rivers<-sf::st_read(dsn="./2022_rivers/rivers_hydrosheds.gpkg")
lakes<-sf::st_read(dsn="./lakes/lakes.gpkg")  

# inspect the vector data
boundaries  # show kind off the attribute table of this vector data file
plot(boundaries2)
plot(boundaries["allow_act"])  # make the map for one column
plot(studyarea)
plot(rivers)
plot(lakes)

# load  the raster data
rainfall<-terra::rast("./rainfall/CHIRPS_MeanAnnualRainfall.tif")
hillshade<-terra::rast("./elevation_hillshade/srtm3_500m_focalstatmean_hillshadeZ6.tif")
elevation<-terra::rast("./elevation/srtm3/srtm3.tif")
woodybiom<-terra::rast("./2016_WoodyVegetation/TBA_gam_utm36s.tif")

# plot the raster data - eg rainfall
rainfall  # inspect the spatraster object
e<-terra::ext(boundaries)  # make an extent to clip the rasters
e
bb<-sf::st_as_sfc(sf::st_bbox(boundaries))  # make an extent to clip the vector data 
bb
terra::plot(terra::crop(hillshade,e),
            col=grey.colors(10),
            legend=NA,
            mar=c(3,6,3,6))   # plot the rainfall
terra::plot(terra::crop(rainfall,e),
            col = rev(wesanderson::wes_palette("Zissou1", 10, type = "continuous")),
            alpha=0.5, add=T)   # plot the rainfall
plot(sf::st_geometry(boundaries),lwd=1.5,add=T)    # add the boundaries
plot(sf::st_geometry(st_intersection(rivers,bb)),  # plot rivers, only inside bb of boundaries
     add=T,col="blue",lwd=1.5)
plot(sf::st_geometry(st_intersection(lakes,bb)),  # plot lakes, only inside bb of boundaries
     add=T,col="lightblue")
plot(sf::st_geometry(st_intersection(studyarea,bb)),  # plot studyarea
     add=T,lty=2, lwd=1.5,)

# make a similar plot for elevation ( later)
terra::plot(rainfall)
rainfall_cl<-terra::classify(rainfall, 
                             c(0,seq(400,1000,100),4000))
terra::plot(rainfall_cl)
plot(sf::st_geometry(boundaries),lwd=1.5,add=T)    # add the boundaries

# make an interactive map with the leaflet library
leaflet::leaflet() %>%
  addTiles() %>%
  addRasterImage(project(rainfall_cl,"EPSG:4326"),opacity=0.5) %>%
  addPolygons(data=st_transform(boundaries,4326),
              color="black",
              weight=1,
              fillOpacity = 0) %>%
  addPolygons(data=st_transform(rivers,4326),
              color="blue",
              weight=1.2) 

# makes some maps in ggplot
xlimit<-sf::st_bbox(boundaries)[c(1,3)]
ylimit<-sf::st_bbox(boundaries)[c(2,4)]
pal_zissou1<-rev(wesanderson::wes_palette("Zissou1", 10, type = "continuous"))
colscale
p1<- ggplot2::ggplot() +
  tidyterra::geom_spatraster(data=hillshade,
                             maxcell=500000,
                             show.legend=F) +
  scale_fill_gradient(low="black",high="white") +
  ggnewscale::new_scale_fill() +   # allow for a new color fill scale
  tidyterra::geom_spatraster(data=rainfall,
                             maxcell=500000,
                             alpha=0.5) +
  scale_fill_gradientn(colours = pal_zissou1,
                       limits=c(500,1100),
                       oob=squish) +
  geom_sf(data=boundaries,color="black", size=4,fill=NA) +
  geom_sf(data=rivers,size=1,color="blue") +
  geom_sf(data=lakes,fill="lightblue") +
  coord_sf(xlimit,ylimit, datum=st_crs(32736))   # keep in original unprojected coordinates
p1  
ggsave("p1.png", plot=p1, width=1000,height=1000, units="px")
# make the same map for elevation with the palette terrain.colors(10)  


p2<- ggplot2::ggplot() +
  tidyterra::geom_spatraster(data=hillshade,
                             maxcell=100000,
                             show.legend=F) +
  scale_fill_gradient(low="black",high="white") +
  ggnewscale::new_scale_fill() +   # allow for a new color fill scale
  tidyterra::geom_spatraster(data=elevation,
                             maxcell=100000,
                             alpha=0.5) +
  scale_fill_gradientn(colours = terrain.colors(10),
                       limits=c(1100,2500),
                       oob=squish) +
  geom_sf(data=boundaries,size=2,fill=NA) +
  geom_sf(data=rivers,size=1,color="blue") +
  geom_sf(data=lakes,fill="lightblue") +
  scale_size_identity() +
  coord_sf(xlimit,ylimit, datum=st_crs(32736))   # keep in original unprojected coordinates
p2  
# combine the maps
gridExtra::grid.arrange(p1,p2,nrow=1,ncol=2)

# make a ggplot plot of woodybiom, + rivers + protected area, only for you study area
xlimit_sa<-sf::st_bbox(studyarea)[c(1,3)]
ylimit_sa<-sf::st_bbox(studyarea)[c(2,4)]
ext<-sf::st_as_sfc(sf::st_bbox(studyarea))

p3<- ggplot2::ggplot() +
  tidyterra::geom_spatraster(data=crop(hillshade,ext),
                             show.legend=F) +
  scale_fill_gradient(low="black",high="white") +
  ggnewscale::new_scale_fill() +   # allow for a new color fill scale
  tidyterra::geom_spatraster(data=crop(woodybiom,ext),
                             alpha=0.5) +
  scale_fill_gradientn(colours = rev(topo.colors(10)),
                       limits=c(-1,3),
                       oob=squish) +
  geom_sf(data=st_intersection(boundaries,studyarea),color="black", size=4,fill=NA) +
  geom_sf(data=st_intersection(rivers,studyarea),size=1,color="blue") +
  geom_sf(data=st_intersection(lakes,studyarea),fill="lightblue") +
  coord_sf(xlimit_sa,ylimit_sa, datum=st_crs(32736)) +   # keep in original unprojected coordinates
  ggtitle("woody biomass")
p3  

# create 1000 random points in our study area
randompoints<-sf::st_sample(studyarea,1000) %>%
    st_as_sf() %>%   # convert geometry object to sf object
    rename(geom=x)
randompoints
# and add to p3
p4<-p3 + geom_sf(data=randompoints,size=1) +
     coord_sf(xlimit_sa,ylimit_sa, datum=st_crs(32736))+   # keep in original unprojected
     ggtitle("random points")
p4


p5<- ggplot2::ggplot() +
  tidyterra::geom_spatraster(data=crop(hillshade,ext),
                             show.legend=F) +
  scale_fill_gradient(low="black",high="white") +
  ggnewscale::new_scale_fill() +   # allow for a new color fill scale
  tidyterra::geom_spatraster(data=crop(elevation,ext),
                             show.legend=T,alpha=0.5) +
  scale_fill_gradientn(colours = terrain.colors(10),
                       limits=c(1200,2000),
                       oob=squish) +
  ggnewscale::new_scale_fill() +   # allow for a new color fill scale
  geom_sf(data=st_intersection(boundaries,studyarea),color="black", size=4,fill=NA) +
  geom_sf(data=st_intersection(rivers,studyarea),size=1,color="blue") +
  geom_sf(data=st_intersection(lakes,studyarea),fill="lightblue") +
  coord_sf(xlimit_sa,ylimit_sa, datum=st_crs(32736)) +   # keep in original unprojected coordinates
  ggtitle("elevation (m + msl)")
p5  

p6<- ggplot2::ggplot() +
  tidyterra::geom_spatraster(data=crop(hillshade,ext),
                             show.legend=T) +
  scale_fill_gradient(low="black",high="white") +
  geom_sf(data=st_intersection(boundaries,studyarea),color="black", size=4,fill=NA) +
  geom_sf(data=st_intersection(rivers,studyarea),size=1,color="blue") +
  geom_sf(data=st_intersection(lakes,studyarea),fill="lightblue") +
  coord_sf(xlimit_sa,ylimit_sa, datum=st_crs(32736)) +   # keep in original unprojected coordinates
  ggtitle("slope")
p6  


gridExtra::grid.arrange(p3, p4,p5,p6,nrow=2,ncol=3)

# complete this for all other raster values
p_woodybiom<-terra::extract(woodybiom,randompoints) %>%
  as_tibble() %>%
  rename(woodybiom=TBA_gam_utm36s)
p_elevation<-terra::extract(elevation,randompoints) %>%
  as_tibble() %>%
  rename(elevation=srtm3)
p_elevation
p_rainfall<-terra::extract(rainfall,randompoints) %>%
  as_tibble() %>%
  rename(rainfall=CHIRPS_MeanAnnualRainfall)
p_rainfall
p_allvar<-cbind(p_woodybiom[,2],p_rainfall[,2],p_elevation[,2]) %>% as_tibble()
# make long format
p_allvar_long<-pivot_longer(data=p_allvar,
                            cols = rainfall:elevation,
                            names_to ="variable",
                            values_to = "xvar")
p_allvar_long
ggplot(data=p_allvar_long,aes(x=xvar,y=woodybiom)) +
       geom_point() +
       geom_smooth(method="lm") +
  facet_wrap(~variable,scales = "free")


# make distance to rivers raster
elevation_sa<-crop(elevation,ext)
plot(elevation_sa)
rivers_sa<-terra::vect(st_intersection(rivers,studyarea))
plot(rivers_sa,add=T)
rivers_sa_rst<-terra::rasterize(rivers_sa,elevation_sa,field=1)
dist<-terra::distance(rivers_sa_rst)
plot(dist,col=topo.colors(20))
plot(rivers_sa,add=T,lwd=2)
