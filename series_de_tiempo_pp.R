library(forecast)
library(tseries)
library(ggfortify)
library(PerformanceAnalytics)

rm(list=ls())
dev.off()

setwd('/home/msomos/Documentos/WRF/proyecto_WRF/')
source('funcion_db_a_serie_de_tiempo.R')
source('funcion_preparacion_db_para_serie_de_tiempo.R')

setwd('/home/msomos/Documentos/WRF/revision_paper/datos_diagrama_de_Taylor/ParaTaylorRain/ParaTaylorRain/')

# para precipitacion y sus mejores conf (9, 10 y 13)... sus mejores estaciones segun NSE, son:
# Rio Baker Angostura Chacabuco, Rio Cochrane Cochrane, Rio Colonia En Nacimiento (no tiene 2017), 
# Rio Nef Estero El Revalse, Cochrane (no esta), Bahia Murta

# db0 <- read.csv('ParaTaylorRioBakerEnAngosturaChacabuco_DGA.csv')
# nombre.estacion1 <- 'Río Baker en\nAngostura Chacabuco (DGA)'
# db1 <- preparacion.db.para.serie.de.tiempo(db0, anho_para_filtro = '2017')
# 
# db0 <- read.csv('ParaTaylorRio Cochrane en Cochrane DGA.csv')
# nombre.estacion2 <- 'Río Cochrane\nen Cochrane (DGA)'
# db2 <- preparacion.db.para.serie.de.tiempo(db0, anho_para_filtro = '2017')

db0 <- read.csv('ParaTaylorBahia Murta DGA.csv')
nombre.estacion3 <- 'Bahia Murta (DGA)'
db3 <- preparacion.db.para.serie.de.tiempo(db0, anho_para_filtro = '2017')

db0 <- read.csv('ParaTaylorRio Nef Junta Estero El Revalse DGA.csv')
nombre.estacion4 <- 'Río Nef junta\nestero El Revalse (DGA)'
db4 <- preparacion.db.para.serie.de.tiempo(db0, anho_para_filtro = '2017')

# mejores pp: 9 y 10
# peores pp: 19 y 20
conf.de.interes <- c(9, 10, 13, 8, 19, 20)
# series.de.tiempo1 <- db.a.serie.de.tiempo(db1, conf.especificas = conf.de.interes)
# series.de.tiempo2 <- db.a.serie.de.tiempo(db2, conf.especificas = conf.de.interes)
series.de.tiempo3 <- db.a.serie.de.tiempo(db3, conf.especificas = conf.de.interes)
series.de.tiempo4 <- db.a.serie.de.tiempo(db4, conf.especificas = conf.de.interes)

# plots ----
color.i <- c('#b71c1c', '#8e44ad', '#2980b9', '#27ae60', '#f1c40f', '#ff6f00')
nombre.etiquetas <- colnames(series.de.tiempo4)
longitud.conf.de.interes <- length(conf.de.interes)
# titulo1 <- paste('Precipitation', nombre.estacion1, sep = ' - ')
# titulo2 <- paste('Precipitation', nombre.estacion2, sep = ' - ')
titulo3 <- paste('Precipitation', nombre.estacion3, sep = ' - ')
titulo4 <- paste('Precipitation', nombre.estacion4, sep = ' - ')

# setwd('/home/msomos/Documentos/WRF/plots_paper/')
# png('pp_series_de_tiempo.png', width = 850, height = 750, units = "px")

# par(mfrow=c(2,2))
# 
# # 1
# c(plot.xts(as.xts(series.de.tiempo1), main=titulo1, col = c('black', color.i), #legend.loc = TRUE,
#          lty=c(1, rep(1,longitud.conf.de.interes)), lwd=c(2, rep(2,longitud.conf.de.interes))),
# 
# addLegend("topleft", 
#           legend.names=nombre.etiquetas,
#           col=c('black', color.i),
#           lty=c(1, rep(1,longitud.conf.de.interes)),
#           lwd=c(4, rep(4,longitud.conf.de.interes)),
#           ncol=1,
#           bg="white", cex=0.9) )[2]
# 
# # 2
# c(plot.xts(as.xts(series.de.tiempo2), main=titulo2, col = c('black', color.i), #legend.loc = TRUE,
#          lty=c(1, rep(1,longitud.conf.de.interes)), lwd=c(2, rep(2,longitud.conf.de.interes))),
# 
# addLegend("topleft", 
#           legend.names=nombre.etiquetas,
#           col=c('black', color.i),
#           lty=c(1, rep(1,longitud.conf.de.interes)),
#           lwd=c(4, rep(4,longitud.conf.de.interes)),
#           ncol=1,
#           bg="white", cex=0.9) )[2]

# 3
c(plot.xts(as.xts(series.de.tiempo3), main=titulo3, col = c('black', color.i), #legend.loc = TRUE,
         lty=c(1, rep(1,longitud.conf.de.interes)), lwd=c(2, rep(2,longitud.conf.de.interes))),

addLegend("topleft", 
          legend.names=nombre.etiquetas,
          col=c('black', color.i),
          lty=c(1, rep(1,longitud.conf.de.interes)),
          lwd=c(4, rep(4,longitud.conf.de.interes)),
          ncol=1,
          bg="white", cex=0.9) )[2]
mtext(expression('mm month'^-1), side = 2, line = 3.65, font = 1)

# 4
c(plot.xts(as.xts(series.de.tiempo4), main=titulo4, col = c('black', color.i), #legend.loc = TRUE,
         lty=c(1, rep(1,longitud.conf.de.interes)), lwd=c(2, rep(2,longitud.conf.de.interes))),

addLegend("topleft", 
          legend.names=nombre.etiquetas,
          col=c('black', color.i),
          lty=c(1, rep(1,longitud.conf.de.interes)),
          lwd=c(4, rep(4,longitud.conf.de.interes)),
          ncol=1,
          bg="white", cex=0.9) )[2]
mtext(expression('mm month'^-1), side = 2, line = 3.65, font = 1)

dev.off()
# ---
