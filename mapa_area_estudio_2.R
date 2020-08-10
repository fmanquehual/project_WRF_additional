library(rgdal)
library(rgeos)
library(raster)
library(stringr)
library(ggplot2)
library(dplyr)
library(maps)
library(viridis)
library(mapproj)
library(rasterVis)
library(prettymapr)
library(ggspatial)
library(cowplot)
library(ggrepel)

rm(list=ls())
dev.off()

# lectura de coberturas ----
#setwd('/home/msomos/Documentos/WRF/coberturas/coberturas_ok/')
setwd('C:/Users/Usuario/Documents/Francisco/WRF/coberturas/coberturas_ok/')

marco <- readOGR('.', 'marco_area_estudio')
marco.grande <- readOGR('.', 'marco_area_estudio_grande')
dem <- raster('dem_clip.tif')
dem.grande <- raster('dem2_clip.tif')
dem.mapa <- raster('dem3_clip_para_mapas.tif')
sud <- readOGR('.', 'sudamerica_clip')
ocean <- readOGR('.', 'oceano_sudamerica_clip')
ocean.grande <- readOGR('.', 'oceano2_sudamerica_clip')
inter <- readOGR('.', 'limite_internacional_clip')
inter.grande <- readOGR('.', 'limite_internacional2_clip')
g <- readOGR('.', 'polygon_glaciares_chile_argentina_clip')
g.grande <- readOGR('.', 'polygon_glaciares_chile_argentina_dissolve_geo')
est <- readOGR('.', 'estaciones_metereologicas')
est@data$id <- 1:nrow(est@data)
est@data$otro <- '1'

#setwd('/home/msomos/Documentos/WRF/coberturas/export/')
setwd('C:/Users/Usuario/Documents/Francisco/WRF/coberturas/export/')

cuenca <- readOGR('.', 'polygon_cuenca_baker_grass_geo')
ocean.sud <- readOGR('.', 'polygon_oceano__sudamerica_geo')

# fin ---



# preparacion coberturas ----

db.dem <- as.data.frame(dem, xy = TRUE)
head(db.dem)
names(db.dem) <- c('x', 'y', 'altitud')

cuenca.db <- fortify(cuenca@data, region="extrude")
names(cuenca.db) <- c("x","y", "order","hole","piece","group","id" )
head(cuenca.db)

g.db <- fortify(g.grande, region="loc_unc_x")
names(g.db) <- c("x","y", "order","hole","piece","group","id" )
head(g.db)

# fin ---



# mapa ----

# plot 1 ----

colours.i <- rev(c('#EBEDEF', '#212F3C'))
colours.j <- rev(c('#F9EB07', '#F48507', '#CD0E3C', '#820ECD', '#590ECD', '#09096F'))
color_etiqueta <- 'white'

p1 <- ggplot() +
  layer_spatial(dem) +
  labs(y=NULL, x=NULL) +
  scale_fill_gradientn('Elevation', colours = colours.i, na.value = 'white') +
  coord_sf(crs=4326, expand = FALSE) +
  geom_polygon(data = cuenca.db, aes(x=x, y = y, group = group, color=otro), fill='transparent', col='red') +
  geom_polygon(data = cuenca.db, aes(x=x, y = y, group = group, color=otro), fill='transparent', col='red') +
  geom_point(data=est@data, aes(x=Lon, y=Lat, size=1), col='#03F2FE') +
  theme_bw() +
  scale_size_continuous(name="Station", range=c(0,3), labels = NULL) +
  geom_label_repel(data = est@data, aes(x=Lon, y=Lat, label=est@data$id),
                   box.padding = 0.9, segment.color = 'grey50') +
  annotation_scale(location = "br", text_col = 'white', style = 'ticks', line_col = 'white') +
  annotation_north_arrow(location = "tr", which_north = "true", height = unit(0.7, "cm"), width = unit(0.7, "cm")) +
  guides(color = guide_colourbar(order=1),
         size = guide_legend(override.aes=aes(fill=NA),order=2),
         fill = guide_colourbar(order=3))
p1
