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


# lectura coberturas ----
# setwd('/home/msomos/Documentos/WRF/coberturas/coberturas_ok/')
setwd('C:/Users/Usuario/Documents/Francisco/WRF/coberturas/coberturas_ok/')

dem <- raster('dem_clip.tif')
pp <- readOGR('.', 'estaciones_metereologicas_pp')
rh <- readOGR('.', 'estaciones_metereologicas_rh')
t2 <- readOGR('.', 'estaciones_metereologicas_t2')
tmin <- readOGR('.', 'estaciones_metereologicas_tmin')
tmax <- readOGR('.', 'estaciones_metereologicas_tmax')
ps <- readOGR('.', 'estaciones_metereologicas_ps')
ocean <- readOGR('.', 'oceano_sudamerica_clip')

# setwd('/home/msomos/Documentos/WRF/coberturas/export/')
setwd('C:/Users/Usuario/Documents/Francisco/WRF/coberturas/export/')

cuenca <- readOGR('.', 'polygon_cuenca_baker_grass_geo')

# fin ---




# preparacion coberturas ----

db.dem <- as.data.frame(dem, xy = TRUE)
head(db.dem)
names(db.dem) <- c('x', 'y', 'altitud')

cuenca.db <- fortify(cuenca, region="extrude")
names(cuenca.db) <- c("x","y", "order","hole","piece","group","id" )
head(cuenca.db)

# fin ---




# plot 1 ----

colours.i <- rev(c('#EBEDEF', '#212F3C'))
colours.j <- rev(c('#F9EB07', '#F48507', '#CD0E3C', '#820ECD', '#590ECD', '#09096F'))
color_etiqueta <- 'white'
  
range(pp@data$SD)

pp1 <- ggplot() +
  layer_spatial(dem) +
  labs(y=NULL, x=NULL) +
  scale_fill_gradientn('Elevation', colours = colours.i, na.value = 'white') +
  coord_sf(crs=4326, expand = FALSE) +
  geom_polygon(data = cuenca.db, aes(x=x, y = y, group = group), fill='transparent', col='red') +
  geom_point(data=pp@data, aes(x=lon, y=lat, size=SD, color=Mean)) +
  theme_bw() +
  scale_size_continuous(name="SD", range=c(0,7), breaks=c(15, 30, 45, 60), limits = c(0, 60))+#, guide = guide_legend(override.aes=aes(fill=NA), order=1)) +
  scale_colour_gradientn('Mean', colours = colours.j, na.value = NA) +
  geom_label_repel(data = pp@data, aes(x=lon, y=lat,label=mjr_vlr, size=10),
                   #arrow = arrow( length = unit(0.03, "npc"), type = "closed", ends = "first"), 
                   box.padding = 0.9, segment.color = 'grey50') +
  annotation_scale(location = "br", text_col = 'white', style = 'ticks', line_col = 'white') + #, line_width = 1.5, text_cex = 1.05) +
  annotation_north_arrow(location = "tr", which_north = "true", height = unit(0.7, "cm"), width = unit(0.7, "cm")) +
  guides(color = guide_colourbar(order=1),
         size = guide_legend(override.aes=aes(fill=NA),order=2),
         fill = guide_colourbar(order=3))
pp1

colours.j <- rev(c('#F9EB07', '#F48507', '#CD0E3C', '#820ECD', '#09096F'))
range(rh@data$SD)

rh1 <- ggplot() +
  layer_spatial(dem) +
  labs(y=NULL, x=NULL) +
  scale_fill_gradientn('Elevation', colours = colours.i, na.value = 'white') +
  coord_sf(crs=4326, expand = FALSE) +
  geom_polygon(data = cuenca.db, aes(x=x, y = y, group = group), fill='transparent', col='red') +
  geom_point(data=rh@data, aes(x=lon, y=lat, size=SD, color=Mean)) +
  theme_bw() +
  scale_size_continuous(name="SD", range=c(0,7), breaks=c(1, 2, 3, 4), limits = c(0, 4)) +#, guide = guide_legend(override.aes=aes(fill=NA), order=1)) +
  scale_colour_gradientn('Mean', colours = colours.j, na.value = NA) +
  geom_label_repel(data = rh@data, aes(x=lon, y=lat,label=mjr_vlr, size=0.6),
                   #arrow = arrow( length = unit(0.03, "npc"), type = "closed", ends = "first"), 
                   box.padding = 0.9, segment.color = 'grey50') +
  annotation_scale(location = "br", text_col = 'white', style = 'ticks', line_col = 'white') + #, line_width = 1.5, text_cex = 1.05) +
  annotation_north_arrow(location = "tr", which_north = "true", height = unit(0.7, "cm"), width = unit(0.7, "cm")) +
  guides(color = guide_colourbar(order=1),
         size = guide_legend(override.aes=aes(fill=NA),order=2),
         fill = guide_colourbar(order=3))
rh1

colours.j <- rev(c('#F9EE07', '#CD0E3C', '#820ECD', '#590ECD', '#410B94', '#09096F'))
range(ps@data$SD)

ps1 <- ggplot() +
  layer_spatial(dem) +
  labs(y=NULL, x=NULL) +
  scale_fill_gradientn('Elevation', colours = colours.i, na.value = 'white') +
  coord_sf(crs=4326, expand = FALSE) +
  geom_polygon(data = cuenca.db, aes(x=x, y = y, group = group), fill='transparent', col='red') +
  geom_point(data=ps@data, aes(x=lon, y=lat, size=SD, color=Mean)) +
  theme_bw() +
  scale_size_continuous(name="SD", range=c(0,7), breaks=c(0.01, 0.02, 0.03, 0.04), limits = c(0, 0.04)) +#, guide = guide_legend(override.aes=aes(fill=NA), order=1)) +
  scale_colour_gradientn('Mean', colours = colours.j, na.value = NA) +
  geom_label_repel(data = ps@data, aes(x=lon, y=lat,label=mjr_vlr, size=0.006),
                   #arrow = arrow( length = unit(0.03, "npc"), type = "closed", ends = "first"), 
                   box.padding = 0.9, segment.color = 'grey50') +
  annotation_scale(location = "br", text_col = 'white', style = 'ticks', line_col = 'white') + #, line_width = 1.5, text_cex = 1.05) +
  annotation_north_arrow(location = "tr", which_north = "true", height = unit(0.7, "cm"), width = unit(0.7, "cm")) +
  guides(color = guide_colourbar(order=1),
         size = guide_legend(override.aes=aes(fill=NA),order=2),
         fill = guide_colourbar(order=3))
ps1

colours.j <- rev(c('#F9EE07', '#CD0E3C', '#820ECD', '#590ECD', '#410B94', '#09096F'))
range(t2@data$SD)

t21 <- ggplot() +
  layer_spatial(dem) +
  labs(y=NULL, x=NULL) +
  scale_fill_gradientn('Elevation', colours = colours.i, na.value = 'white', guide = guide_colorbar(order=1)) +
  coord_sf(crs=4326, expand = FALSE) +
  geom_polygon(data = cuenca.db, aes(x=x, y = y, group = group), fill='transparent', col='red') +
  geom_point(data=t2@data, aes(x=lon, y=lat, size=SD, color=Mean)) +
  theme_bw() +
  scale_size_continuous(name="SD", range=c(0,7), breaks=c(0.3, 0.7, 1.1, 1.5), limits = c(0, 1.5)) +#, guide = guide_legend(override.aes=aes(fill=NA), order=1)) +
  scale_colour_gradientn('Mean', colours = colours.j, na.value = NA) +
  geom_label_repel(data = t2@data, aes(x=lon, y=lat,label=mjr_vlr, size=0.2),
                   #arrow = arrow( length = unit(0.03, "npc"), type = "closed", ends = "first"), 
                   box.padding = 0.9, segment.color = 'grey50') +
  annotation_scale(location = "br", text_col = 'white', style = 'ticks', line_col = 'white') + #, line_width = 1.5, text_cex = 1.05) +
  annotation_north_arrow(location = "tr", which_north = "true", height = unit(0.7, "cm"), width = unit(0.7, "cm")) +
  guides(color = guide_colourbar(order=1),
         size = guide_legend(override.aes=aes(fill=NA),order=2),
         fill = guide_colourbar(order=3))
t21

colours.j <- rev(c('#F9EB07', '#F48507', '#CD0E3C', '#820ECD', '#590ECD', '#09096F'))
range(tmin@data$SD)

tmin1 <- ggplot() +
  layer_spatial(dem) +
  labs(y=NULL, x=NULL) +
  scale_fill_gradientn('Elevation', colours = colours.i, na.value = 'white') +
  coord_sf(crs=4326, expand = FALSE) +
  geom_polygon(data = cuenca.db, aes(x=x, y = y, group = group), fill='transparent', col='red') +
  geom_point(data=tmin@data, aes(x=lon, y=lat, size=SD, color=Mean)) +
  theme_bw() +
  scale_size_continuous(name="SD", range=c(0,7), breaks=c(0.3, 0.7, 1.1, 1.5), limits = c(0, 1.5)) +#, guide = guide_legend(override.aes=aes(fill=NA), order=1)) +
  scale_colour_gradientn('Mean', colours = colours.j, na.value = NA) +
  geom_label_repel(data = tmin@data, aes(x=lon, y=lat,label=mjr_vlr, size=0.2),
                   #arrow = arrow( length = unit(0.03, "npc"), type = "closed", ends = "first"), 
                   box.padding = 0.9, segment.color = 'grey50') +
  annotation_scale(location = "br", text_col = 'white', style = 'ticks', line_col = 'white') + #, line_width = 1.5, text_cex = 1.05) +
  annotation_north_arrow(location = "tr", which_north = "true", height = unit(0.7, "cm"), width = unit(0.7, "cm")) +
  guides(color = guide_colourbar(order=1),
         size = guide_legend(override.aes=aes(fill=NA),order=2),
         fill = guide_colourbar(order=3))
tmin1

colours.j <- rev(c('#9913F0', '#590ECD', '#09096F'))
range(tmax@data$SD)

tmax1 <- ggplot() +
  layer_spatial(dem) +
  labs(y=NULL, x=NULL) +
  scale_fill_gradientn('Elevation', colours = colours.i, na.value = 'white') +
  coord_sf(crs=4326, expand = FALSE) +
  geom_polygon(data = cuenca.db, aes(x=x, y = y, group = group), fill='transparent', col='red') +
  geom_point(data=tmax@data, aes(x=lon, y=lat, size=SD, color=Mean)) +
  theme_bw() +
  scale_size_continuous(name="SD", range=c(0,7), breaks=c(0.3, 0.7, 1.1, 1.5), limits = c(0, 1.5)) +#, guide = guide_legend(override.aes=aes(fill=NA), order=1)) +
  scale_colour_gradientn('Mean', colours = colours.j, na.value = NA) +
  geom_label_repel(data = tmax@data, aes(x=lon, y=lat,label=mjr_vlr, size=0.2),
                   #arrow = arrow( length = unit(0.03, "npc"), type = "closed", ends = "first"), 
                   box.padding = 0.9, segment.color = 'grey50') +
  annotation_scale(location = "br", text_col = 'white', style = 'ticks', line_col = 'white') + #, line_width = 1.5, text_cex = 1.05) +
  annotation_north_arrow(location = "tr", which_north = "true", height = unit(0.7, "cm"), width = unit(0.7, "cm")) +
  guides(color = guide_colourbar(order=1),
         size = guide_legend(override.aes=aes(fill=NA),order=2),
         fill = guide_colourbar(order=3))
tmax1

# fin ---



# plot final ----

p.out <- plot_grid(pp1, rh1, ps1, 
                   t21, tmin1, tmax1,
                     labels=c('AUTO'), ncol = 3, nrow = 2)
# p.out

# setwd('C:/Users/Usuario/Desktop/')
# pdf('eJ2.pdf', height = 13, width = 20)
# p.out
# dev.off()

setwd('C:/Users/Usuario/Documents/Francisco/WRF/plots_paper/')
setEPS()
postscript(file = "mapas_estaciones_BIAS.eps", height = 13, width = 20)  # Una figura en cm
par(mar=c(4,4,0,0)+0.1)
p.out
dev.off()

# fin ---




# Otros ----

# col.i <- colorRampPalette( c('#365d16', "darkolivegreen4","yellow", "brown", '#742517'))(10)
# breaks.i <- seq(-500, 4000, by=500)
# lat.i <- axes.map('lat', alt)
# lon.i <- axes.map('lon', alt)
# tamanho.etiquetas <- 0.7
#   
# setwd('/home/msomos/Escritorio/')
# 
#   pdf('ej.pdf', height = 7.2, width = 11)
#   par(mfrow=c(2,3), mar=c(0,0,1.5,0), omi=c(0.3,0.4,0,0.2))
#   
#   # pp
#   plot(alt, col=col.i, breaks=breaks.i, axes=FALSE, legend=FALSE)
#   #axis(1, at=lon.i$value, labels=lon.i$sufijo, cex.axis = 0.8, las = 1)#, padj = -1.5)
#   axis(2, at=lat.i$value, labels=lat.i$sufijo, cex.axis = 0.8, las = 1)#, line = -5.6)#, hadj = -3.1)
#   plot(ocean, add=TRUE, col='#0b243b', border='#0b243b')
#   plot(cuenca, add=TRUE, border='#09def7', lwd=1.5, lty=2)
#   plot(pp, add=TRUE, pch=20)#, col='orange', border='red')
#   text(pp, pp$m, cex=tamanho.etiquetas, halo=TRUE, hc='white', pos=2, offset=0.3)
#   text(pp, pp$sd, cex=tamanho.etiquetas, col='white', halo=TRUE, hc='#0c6d85', pos=4, offset=0.3, hw=0.1)
#   mtext('A)', side=3, line=-2, col='white', font = 2, adj=0.95)
#   
#   # rh
#   plot(alt, col=col.i, breaks=breaks.i, axes=FALSE, legend=FALSE)
#   #axis(1, at=lon.i$value, labels=lon.i$sufijo, cex.axis = 0.8, las = 1)#, padj = -1.5)
#   #axis(2, at=lat.i$value, labels=lat.i$sufijo, cex.axis = 0.8, las = 1)#, line = -5.6)#, hadj = -3.1)
#   plot(ocean, add=TRUE, col='#0b243b', border='#0b243b')
#   plot(cuenca, add=TRUE, border='#09def7', lwd=1.5, lty=2)
#   plot(rh, add=TRUE, pch=20)#, col='orange', border='red')
#   text(rh, rh$m, cex=tamanho.etiquetas, halo=TRUE, hc='white', pos=2, offset=0.3)
#   text(rh, rh$sd, cex=tamanho.etiquetas, col='white', halo=TRUE, hc='#0c6d85', pos=4, offset=0.3, hw=0.1)
#   mtext('B)', side=3, line=-2, col='white', font = 2, adj=0.95)
#   
#   # t2
#   plot(alt, col=col.i, breaks=breaks.i, axes=FALSE)
#   #axis(1, at=lon.i$value, labels=lon.i$sufijo, cex.axis = 0.8, las = 1)#, padj = -1.5)
#   #axis(2, at=lat.i$value, labels=lat.i$sufijo, cex.axis = 0.8, las = 1)#, line = -5.6)#, hadj = -3.1)
#   plot(ocean, add=TRUE, col='#0b243b', border='#0b243b')
#   plot(cuenca, add=TRUE, border='#09def7', lwd=1.5, lty=2)
#   plot(t2, add=TRUE, pch=20)#, col='orange', border='red')
#   text(t2, t2$m, cex=tamanho.etiquetas, halo=TRUE, hc='white', pos=2, offset=0.3)
#   text(t2, t2$sd, cex=tamanho.etiquetas, col='white', halo=TRUE, hc='#0c6d85', pos=4, offset=0.3, hw=0.1)
#   mtext('C)', side=3, line=-2, col='white', font = 2, adj=0.95)
#   
#   # tmin
#   plot(alt, col=col.i, breaks=breaks.i, axes=FALSE, legend=FALSE)
#   axis(1, at=lon.i$value, labels=lon.i$sufijo, cex.axis = 0.8, las = 1)#, padj = -1.5)
#   axis(2, at=lat.i$value, labels=lat.i$sufijo, cex.axis = 0.8, las = 1)#, line = -5.6)#, hadj = -3.1)
#   plot(ocean, add=TRUE, col='#0b243b', border='#0b243b')
#   plot(cuenca, add=TRUE, border='#09def7', lwd=1.5, lty=2)
#   plot(tmin, add=TRUE, pch=20)#, col='orange', border='red')
#   text(tmin, tmin$m, cex=tamanho.etiquetas, halo=TRUE, hc='white', pos=2, offset=0.3)
#   text(tmin, tmin$sd, cex=tamanho.etiquetas, col='white', halo=TRUE, hc='#0c6d85', pos=4, offset=0.3, hw=0.1)
#   mtext('D)', side=3, line=-2, col='white', font = 2, adj=0.95)
#   
#   # tmax
#   plot(alt, col=col.i, breaks=breaks.i, axes=FALSE)
#   axis(1, at=lon.i$value, labels=lon.i$sufijo, cex.axis = 0.8, las = 1)#, padj = -1.5)
#   #axis(2, at=lat.i$value, labels=lat.i$sufijo, cex.axis = 0.8, las = 1)#, line = -5.6)#, hadj = -3.1)
#   plot(ocean, add=TRUE, col='#0b243b', border='#0b243b')
#   plot(cuenca, add=TRUE, border='#09def7', lwd=1.5, lty=2)
#   plot(tmax, add=TRUE, pch=20)#, col='orange', border='red')
#   text(tmax, tmax$m, cex=tamanho.etiquetas, halo=TRUE, hc='white', pos=2, offset=0.3)
#   text(tmax, tmax$sd, cex=tamanho.etiquetas, col='white', halo=TRUE, hc='#0c6d85', pos=4, offset=0.3, hw=0.1)
#   mtext('E)', side=3, line=-2, col='white', font = 2, adj=0.95)
#         
#    dev.off()
# 
#    #  ---
#    
#    range(pp@data$Mean)
#    mybreaks <- c(-30 -25, -15, 0, 15, 25, 35)
#    # Left: use size and color
#    ej1 <- ggplot() +
#      geom_polygon(data = cuenca.db, aes(x=x, y = y, group = group), fill="grey") +
#      geom_point( data=pp@data, aes(x=lon, y=lat, size=Mean, color=Mean)) +
#      #geom_point( data=pp@data, aes(x=lon, y=lat, size=SD, color=SD)) +
#      #scale_color_viridis(option = 'C') +
#      coord_map() +
#      theme_void() +
#      guides( colour = guide_legend()) +
#      # scale_size_continuous(name="Population (in M)", range=c(0,5), breaks=mybreaks) +
#      # scale_alpha_continuous(name="Population (in M)", range=c(0, 1), breaks=mybreaks) +
#      scale_color_viridis(option="C") +#breaks=mybreaks, name="Population (in M)" )
#      geom_text(data = pp@data, aes(x=lon, y=lat, label=SD), size=3, nudge_x=0.15)
#    ej1
#    
#    ##
#    ##
# 
# 
# # funciones ----
# 
# # 1
# axes.map <- function(l.i, raster.i){
#   if(l.i=='lon'){l.j <- 1:2} else(l.j <- 3:4)
#   if(l.i=='lon'){n.i <- 1} else(n.i <- 1)
#   if(l.i=='lon'){sufijo <- '°W'} else(sufijo <- '°S')  
#   l <- extent(raster.i)[l.j]
#   l.min <- min(l)
#   l.max <- max(l)
#   l.min.j <- round(l.min, 0)
#   l.max.j <- round(l.max, 0)
#   seq.l.pre <- seq(l.min.j, l.max.j, by=n.i)
#   seq.l <- paste( str_sub(seq.l.pre, 2, 3), sufijo, sep = '' )
#   out.i <- data.frame(value=seq.l.pre, sufijo=seq.l)
#   
#   return(out.i)
# }
# 
# # fin ---
