library(raster)
library(rgdal)
library(rgeos)
library(RColorBrewer)
library(grid)
library(stringr)
library(ggplotify)
library(cowplot)
library(prettymapr)
library(ggplot2)

rm(list=ls())
dev.off()

# funciones ----

# 1
axes.map <- function(l.i, raster.i){
  if(l.i=='lon'){l.j <- 1:2} else(l.j <- 3:4)
  if(l.i=='lon'){n.i <- 1} else(n.i <- 1)
  if(l.i=='lon'){sufijo <- '°W'} else(sufijo <- '°S')  
  l <- extent(raster.i)[l.j]
  l.min <- min(l)
  l.max <- max(l)
  l.min.j <- round(l.min, 0)
  l.max.j <- round(l.max, 0)
  seq.l.pre <- seq(l.min.j, l.max.j, by=n.i)
  seq.l <- paste( str_sub(seq.l.pre, 2, 3), sufijo, sep = '' )
  out.i <- data.frame(value=seq.l.pre, sufijo=seq.l)
  
  return(out.i)
}

# 2
etiquetas <- function(archivo_shapefile, etiquetas.i, tamanho_etiquetas, color_etiqueta, id_especifico, ubicacion_especifica){
  etiquetas.i <- archivo_shapefile@data$id
  tamanho_etiquetas <- 0.4
  color_etiqueta <- 'black'
  # i <- 36
  # archivo_shapefile <- est
  # id_especifico <- 36 #c(26, 36)
  # ubicacion_especifica <- 2 #c(4, 2)
  
  for (i in 1:nrow(archivo_shapefile@data)) {
    j <- rep(1:4, nrow(archivo_shapefile@data))
    
    if(length(id_especifico)==0){ pos <- j[i] } else( 
      if(etiquetas.i[i]%in%id_especifico[1]){ c(pos <- ubicacion_especifica[1],
                                                id_especifico <- id_especifico[-1],
                                                ubicacion_especifica <- ubicacion_especifica[-1])} else( pos <- j[i]) )
    
    if(pos==1 | pos==3){ c(pos.x <- 0, if(pos==1){pos.y <- -0.1} else(pos.y <- 0.1)) } else{c(pos.y <- 0, if(pos==2){pos.x <- -0.15} else(pos.x <- 0.15)) }
    
    x <- c(archivo_shapefile$Lon[i], archivo_shapefile$Lon[i]+pos.x)
    y <- c(archivo_shapefile$Lat[i], archivo_shapefile$Lat[i]+pos.y)
    ej <- SpatialLines(list(Lines(Line(cbind(x,y)), ID="a")), proj4string = CRS(wgs84))
    
    x_coord <- c(archivo_shapefile$Lon[i]+pos.x-0.05,  archivo_shapefile$Lon[i]+pos.x+0.05, archivo_shapefile$Lon[i]+pos.x+0.05,  
                 archivo_shapefile$Lon[i]+pos.x-0.05, archivo_shapefile$Lon[i]+pos.x-0.05)
    y_coord <- c(archivo_shapefile$Lat[i]+pos.y+0.03, archivo_shapefile$Lat[i]+pos.y+0.03, archivo_shapefile$Lat[i]+pos.y-0.03, 
                 archivo_shapefile$Lat[i]+pos.y-0.03, archivo_shapefile$Lat[i]+pos.y+0.03)
    xym <- cbind(x_coord, y_coord)
    p <- Polygon(xym)
    ps <- Polygons(list(p),1)
    sps<- SpatialPolygons(list(ps))
    
    plot(ej, add=TRUE, col='gray', lwd=1)
    plot(sps, add=TRUE, col='white', border='gray', lwd=0.1)
    text(archivo_shapefile$Lon[i]+pos.x, archivo_shapefile$Lat[i]+pos.y, etiquetas.i[i], cex=tamanho_etiquetas, col=color_etiqueta)
  }
}

# fin ---

wgs84 <- '+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'

# lectura de coberturas ----
setwd('/home/msomos/Documentos/WRF/coberturas/coberturas_ok/')

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

setwd('/home/msomos/Documentos/WRF/coberturas/export/')

cuenca <- readOGR('.', 'polygon_cuenca_baker_grass_geo')
ocean.sud <- readOGR('.', 'polygon_oceano__sudamerica_geo')

# fin ---



# area estudio ----
# col.i <- rev(c('#EBEDEF', '#212F3C'))
col.i <- colorRampPalette( rev(c('#EBEDEF', '#212F3C')))(10)
# col.i <- colorRampPalette( c('#365d16', "darkolivegreen4","yellow", "brown", '#742517'))(10) # verde a rojo
breaks.i <- seq(-500, 4000, by=500)
zlim.i <- c(minValue(dem.grande), maxValue(dem.grande))

lat.i <- axes.map('lat', dem)
lon.i <- axes.map('lon', dem)

x.e1 <- -71.76
y.e1 <- -47.96

setwd('/home/msomos/Escritorio/')

plot(dem, col=col.i, axes=FALSE, breaks=breaks.i)# legend=FALSE
axis(1, at=lon.i$value, labels=lon.i$sufijo, cex.axis = 0.8, las = 1, padj = -1.5)
axis(2, at=lat.i$value, labels=lat.i$sufijo, cex.axis = 0.8, las = 1, hadj = 0.7)
plot(g, add=TRUE, col='white', border='black', lwd=0.01)
plot(ocean, add=TRUE, col='#0b243b', border='#0b243b')
plot(inter, add=TRUE, border='black', lwd=1.5)
plot(cuenca, add=TRUE, border='red', lwd=1.5, lty=2)
etiquetas(est, id_especifico = c(11, 15, 16, 25, 26, 28, 36), ubicacion_especifica = c(1, 2, 2, 3, 4, 2, 2))
plot(est, add=TRUE, col='#0cb6e3', pch=16, cex=0.8)
legend(x.e1,y.e1, title=NULL, text.font = 2, c('Stations', 'Glaciers', 'Baker river basin', 'Boundary'),
       bty='o', lty=c(NA, NA, NA, 1), fill = c(NA, NA, NA, NA), border = c(NA, 'black', 'red', NA),
       col = c('#0cb6e3', NA, NA, 'black'), pch=c(16, NA, NA, NA),
       horiz=FALSE, cex=0.7, bg = 'white', merge = TRUE, trace = TRUE)
lat.j <- axes.map('lat', dem.grande)
lon.j <- axes.map('lon', dem.grande)

x.e2 <- -69.8
y.e2 <- -49.5

plot(dem.mapa, legend=FALSE, col='white', axes=FALSE)
plot(dem.grande, add=TRUE, col=col.i, axes=FALSE, breaks=breaks.i, legend=FALSE)#, box=0)
axis(1, at=lon.j$value, labels=lon.j$sufijo, cex.axis = 0.8, las = 1, padj = -1.5)#, line=-1)
axis(2, at=lat.j$value, labels=lat.j$sufijo, cex.axis = 0.8, las = 1, hadj = 0.7)
plot(g.grande, add=TRUE, col='white', border='white', lwd=0.01)
plot(ocean.grande, add=TRUE, col='#0b243b', border='#0b243b')
plot(inter.grande, add=TRUE, border='black', lwd=1.5)
plot(marco, add=TRUE, border='red', lwd=1.5)
addscalebar(plotepsg=4326, pos = 'bottomleft', style = 'ticks', label.col = 'yellow', linecol = 'yellow')
addnortharrow(pos = "topright", cols = c("white", "white"), text.col = "white", border = "white", scale = 0.7, padin = c(0.25, 0.25))
legend(x.e2, y.e2, title=NULL, text.font = 2, c('Glaciers', 'Study area', 'Boundary'), bty='o', lty=c(NA, NA, 1),
       fill = c(NA, NA, NA), border = c('black', 'red', NA), col = c('white', 'white', 'black'),
       horiz=FALSE, cex=0.7, bg = 'white', merge = TRUE)

# fin ---



# sudamerica ----

plot(sud, col='gray', border='white', lwd=0.5)
plot(ocean.sud, add=TRUE, col='#0b243b', border='#0b243b')
plot(marco.grande, add=TRUE, border='red', lwd=2)

# fin ---


# union de mapas ----
e1 <- as.grob(~c(plot(dem, col=col.i, axes=FALSE, breaks=breaks.i, box=0),
                 axis(1, at=lon.i$value, labels=lon.i$sufijo, cex.axis = 0.7, las = 1, padj = -1.5, line=-0.4),
                 axis(2, at=lat.i$value, labels=lat.i$sufijo, cex.axis = 0.7, las = 1, hadj = 0.7),
  plot(g, add=TRUE, col='white', border='black', lwd=0.01),
  plot(ocean, add=TRUE, col='#0b243b', border='#0b243b'), 
                 plot(inter, add=TRUE, border='black', lwd=1.5),
                 plot(cuenca, add=TRUE, border='red', lwd=1.5, lty=2),
  etiquetas(est, id_especifico = c(11, 15, 16, 25, 26, 28, 36), ubicacion_especifica = c(1, 2, 2, 3, 4, 2, 2)),
                 plot(est, add=TRUE, col='#0cb6e3', pch=16, cex=0.8),
                 addscalebar(plotepsg=4326, pos = 'bottomleft', style = 'ticks', label.col = 'yellow', linecol = 'yellow'), 
                 addnortharrow(pos = "topright", cols = c("white", "white"), text.col = "white", border = "white", scale = 0.5),
legend(x.e1,y.e1, title=NULL, text.font = 2, c('Stations', 'Glaciers', 'Baker river basin       ', 'Boundary'), 
       bty='o', lty=c(NA, NA, NA, 1), fill = c(NA, NA, NA, NA), border = c(NA, 'black', 'red', NA), 
       col = c('#0cb6e3', 'white', 'white', 'black'), pch=c(16, NA, NA, NA), 
       horiz=FALSE, cex=0.7, bg = 'white', merge = TRUE)
))

e2 <- as.grob(~c(#plot(ocean.sud, col='white', border='white', ylim=c(-53.5, 10.5)),
                 plot(sud, col='gray', border='white', lwd=0.5, box=0),
                 plot(ocean.sud, add=TRUE, col='#0b243b', border='#0b243b'),
                 plot(marco.grande, add=TRUE, border='red')))

e3 <- as.grob(~c(#plot(dem.mapa, col=col.i, axes=FALSE, legend=FALSE, breaks=breaks.i, box=0),
                 plot(dem.grande, col=col.i, axes=FALSE, legend=FALSE, breaks=breaks.i, box=0),
                 axis(1, at=lon.j$value, labels=lon.j$sufijo, cex.axis = 0.7, las = 1, padj = -1.5, line=-0.75),
                 axis(2, at=lat.j$value, labels=lat.j$sufijo, cex.axis = 0.7, las = 1, hadj = 0.7),
                 plot(g.grande, add=TRUE, col='white', border='black', lwd=0.01),
                 plot(ocean.grande, add=TRUE, col='#0b243b', border='#0b243b'),
                 plot(inter.grande, add=TRUE, border='black', lwd=1.5),
                 plot(marco, add=TRUE, border='red', lwd=1.5),
                 legend(x.e2, y.e2, title=NULL, text.font = 2, c('Glaciers', 'Study area   ', 'Boundary'), bty='o', lty=c(NA, NA, 1),
                        fill = c(NA, NA, NA), border = c('black', 'red', NA), col = c('black', 'white', 'black'),
                        horiz=FALSE, cex=0.7, bg = 'white', merge = TRUE),
                 addscalebar(plotepsg=4326, pos = 'bottomleft', style = 'ticks', label.col = 'yellow', linecol = 'yellow', padin = c(0.25, 0.25)), 
                 addnortharrow(pos = "topright", cols = c("white", "white"), text.col = "white", border = "white", scale = 0.5, padin = c(0.2, 0.2)) 
))

vp=viewport(x=.213, y=.74, width=.35, height=.3)
e4 <- as.grob(~c(grid.newpage(),
grid.draw(e3),
pushViewport(vp),
grid.draw(e2),
upViewport()))

# grid.newpage()
# grid.draw(e3)
# vp = viewport(x=.213, y=.74, width=.35, height=.3)
# pushViewport(vp)
# grid.draw(e2)
# upViewport()

# fin ---


# plot out ----
arrowA <- data.frame(x1 = 10.3, y1 = 13.4, x2 = 15, y2 = 16.6)
arrowB <- data.frame(x1 = 10.3, y1 = 8.2, x2 = 15, y2 = 5)

setwd('/home/msomos/Documentos/WRF/plots_paper/')
postscript(file = "mapa_area_estudio.eps", height = 7, width = 10)  # Una figura en cm

  ggdraw(xlim = c(0, 30), ylim = c(0, 20), clip = 'on') +
  draw_plot(e4, x = -1.5, y = 0, width = 18, height = 20) +
  draw_plot(e1, x = 12, y = 1, width = 17, height = 18) +
  geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2), data = arrowA, 
               arrow = arrow(type = 'closed', length = unit(0.1, "inches")), lineend = "round", size=0.3) +
  geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2), data = arrowB, 
               arrow = arrow(type = 'closed', length = unit(0.1, "inches")), lineend = "round", size=0.3)
dev.off()
# fin ---


# ej ----

# p1 <- as.grob(~barplot(1:10))
# p1 <- as.grob(~plot(sud, col='gray', border='white'))
# p2 <- as.grob(expression(plot(rnorm(10))))
# p3 <- as.grob(function() plot(sin))
# 
# grid.newpage()
# grid.draw(p1)
# vp = viewport(x=.35, y=.75, width=.35, height=.3)
# pushViewport(vp)
# grid.draw(p2)
# upViewport()

# library(maptools)
# plot(ocean)
# ocean.db <- fortify(ocean, region="id")
# head(ocean.db)
# 
# plot(marco)
# marco.db <- fortify(marco, region="ej__WRF")
# head(marco.db)
# 
# library(reshape2)
# plot(dem)
# dem.db <- as.data.frame(dem, xy=TRUE)
# head(dem.db)
# 
# e2 <- ggplot(dem.db, aes(x, y)) +
#   geom_raster(aes(fill=dem_clip)) +
#   geom_polygon(data = ocean.db, aes(x = long, y = lat, group=group), fill='red') +
#   geom_polygon(data = marco.db, aes(x = long, y = lat, group=group), col='red', fill='transparent')

# library(ggplot2)
# ggplot() +
#   coord_equal(xlim = c(0, 28), ylim = c(0, 20), expand = FALSE) +
#   annotation_custom(ggplotGrob(e1), xmin = 0, xmax = 14, ymin = 0, 
#                     ymax = 20) +
#   annotation_custom(ggplotGrob(e2), xmin = 14, xmax = 28, ymin = 0, 
#                     ymax = 20) +
#   geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2), data = arrowA, 
#                arrow = arrow(), lineend = "round", col='red') +
#   theme_void()

# fin ---