library(raster)
library(rgdal)
library(rgeos)

rm(list=ls())
dev.off()

wgs84 <- '+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'
utm18 <- "+proj=utm +zone=18 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0" #utm zona 18 sur con datum WGS84
utm19 <- "+proj=utm +zone=19 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0" #utm zona 19 sur con datum WGS84

# lectura coberturas ----
setwd('/home/msomos/Documentos/WRF/coberturas/')

lim <- raster('area_estudio1_WRF.tif')
plot(lim)

lim.grande <- raster('area_estudio2_WRF.tif')
plot(lim.grande)

setwd('/home/msomos/Documentos/WRF/coberturas/coberturas_ok/')

g <- readOGR('.', 'polygon_glaciares_chile_argentina_dissolve_geo')

setwd('/home/msomos/Documentos/WRF/coberturas/export/')

dem <- raster('clip_dem_sudamerica.tif')

ch <- readOGR('.', 'polygon_chile_en_blanco_19s')
ch.wgs84 <- spTransform(ch, wgs84)

sud <- readOGR('.', 'sudamerica_geo')
ocean <- readOGR('.', 'polygon_oceano__sudamerica_geo')

setwd('/home/msomos/Documentos/WRF/coberturas/V3.1_Limite_Internacional_50K/')
inter <- readOGR('.', 'V3.1_Limite_Internacional')

# fin ---


# preparacion coberturas ----
setwd('/home/msomos/Documentos/WRF/coberturas/coberturas_ok/')

dem.clip <- crop(dem, lim)
plot(dem.clip)
# writeRaster(dem.clip, filename='dem_clip.tif', format="GTiff", overwrite=TRUE)

plot(g)
plot(lim)
lim.ej <- drawExtent()
plot(lim.ej)

g.r <- rasterize(g, dem.clip, background=NA)
plot(g.r)
g2 <- rasterToPolygons(g.r, dissolve = TRUE)
plot(g2, axes=TRUE)

# writeOGR(g2, ".", "polygon_glaciares_chile_argentina_clip", driver="ESRI Shapefile", overwrite = TRUE)

dem.clip2 <- crop(dem, lim.grande)
plot(dem.clip2)
# writeRaster(dem.clip2, filename='dem2_clip.tif', format="GTiff", overwrite=TRUE)

ej0 <- drawExtent()
dem.clip3 <- crop(dem.clip2, ej0)
# writeRaster(dem.clip3, filename='dem3_clip_para_mapas.tif', format="GTiff", overwrite=TRUE)

plot(ch.wgs84)
lim.ej <- drawExtent()
plot(lim.ej)

lim2 <- lim
lim2[] <- 1
lim3 <- rasterToPolygons(lim2, dissolve = TRUE)
plot(lim3, axes=TRUE)
# writeOGR(lim3, ".", "marco_area_estudio", driver="ESRI Shapefile")

lim.grande2 <- lim.grande
lim.grande2[] <- 1
lim.grande3 <- rasterToPolygons(lim.grande2, dissolve = TRUE)
plot(lim.grande3, axes=TRUE)
# writeOGR(lim.grande3, ".", "marco_area_estudio_grande", driver="ESRI Shapefile")

plot(ocean)
lim.ej2 <- drawExtent()
sud2 <- crop(sud, lim.ej2)
plot(sud2)
# writeOGR(sud2, ".", "sudamerica_clip", driver="ESRI Shapefile")

plot(ocean, col='red')
ocean@data$valor <- 1
ocean.r <- rasterize(ocean, dem.clip, field='valor', fun='last', background=NA)
plot(ocean.r)
ocean2 <- rasterToPolygons(ocean.r, dissolve = TRUE)

ocean2.19s <- spTransform(ocean2, utm19)
ocean2.19s.buffer <- buffer(ocean2.19s, width=0)
ocean2 <- spTransform(ocean2.19s.buffer, wgs84)
plot(dem.clip)
plot(ocean2, add=TRUE, col='blue')
ocean2
ej <- data.frame(id=1)
ocean2 <- SpatialPolygonsDataFrame(ocean2, data = ej, match.ID = FALSE)
# writeOGR(ocean2, ".", "oceano_sudamerica_clip", driver="ESRI Shapefile", overwrite_layer = TRUE)

plot(ocean, col='red')
ocean@data$valor <- 1
ocean.r2 <- rasterize(ocean, dem.clip2, field='valor', fun='last', background=NA)
plot(ocean.r2)
ocean3 <- rasterToPolygons(ocean.r2, dissolve = TRUE)
plot(dem.clip2)
plot(ocean3, add=TRUE, col='blue')

ocean3.19s <- spTransform(ocean3, utm19)
ocean3.19s.buffer <- buffer(ocean3.19s, width=0)
ocean3 <- spTransform(ocean3.19s.buffer, wgs84)
plot(dem.clip)
plot(ocean3, add=TRUE, col='blue')
ocean3
ej <- data.frame(id=1)
ocean3 <- SpatialPolygonsDataFrame(ocean3, data = ej, match.ID = FALSE)

# writeOGR(ocean3, ".", "oceano2_sudamerica_clip", driver="ESRI Shapefile", overwrite_layer = TRUE)

plot(inter)
inter2 <- crop(inter, dem.clip)
plot(dem.clip)
plot(inter2, add=TRUE)

# writeOGR(inter2, ".", "limite_internacional_clip", driver="ESRI Shapefile")

plot(inter)
inter3 <- crop(inter, dem.clip2)
plot(dem.clip2)
plot(inter3, add=TRUE)

# writeOGR(inter3, ".", "limite_internacional2_clip", driver="ESRI Shapefile", overwrite_layer = TRUE)
