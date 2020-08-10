library(forecast)
library(tseries)
library(ggfortify)
library(PerformanceAnalytics)

rm(list=ls())
dev.off()

setwd('/home/msomos/Documentos/WRF/proyecto_WRF/')
source('funcion_db_a_serie_de_tiempo.R')
source('funcion_preparacion_db_para_serie_de_tiempo.R')

setwd('/home/msomos/Documentos/WRF/revision_paper/datos_diagrama_de_Taylor/ParaTaylorT2/ParaTaylorT2/')

# para t2 y sus mejores conf (14, 2 y 22)... sus mejores estaciones segun NSE, son:
# Tamelaike, Rio Pascua Rio Quetru, Teniente Vidal Coyhaique Ad
# Rio Baker Angostura Chacabuco, Rio Ibanez  Desembocadura, Rio Nef Junta Estero El Revalse

db0 <- read.csv('ParaTaylorTamelaike_INIA.csv')
nombre.estacion1 <- 'Tamelaike (INIA)'
db1 <- preparacion.db.para.serie.de.tiempo(db0, anho_para_filtro = '2017')

db0 <- read.csv('ParaTaylorRioPascuaAntejuntaRioQuetru_DGA.csv')
nombre.estacion2 <- 'Río Pascua en\nantes junta Río Quetru (DGA)'
db2 <- preparacion.db.para.serie.de.tiempo(db0, anho_para_filtro = '2017')

# db0 <- read.csv('ParaTaylorTenienteVidalCoyhaiqueAd_DMC.csv')
# nombre.estacion3 <- 'Teniente\nVidal Coyhaique (DMC)'
# db3 <- preparacion.db.para.serie.de.tiempo(db0, anho_para_filtro = '2017')
# 
# db0 <- read.csv('ParaTaylorRioBakerEnAngosturaChacabuco_DGA.csv')
# nombre.estacion4 <- 'Río Baker en\nAngostura Chacabuco (DGA)'
# db4 <- preparacion.db.para.serie.de.tiempo(db0, anho_para_filtro = '2017')

# mejores pp: 9 y 10
# peores pp: 19 y 20
conf.de.interes <- c(14, 2, 22, 15, 4, 23)
series.de.tiempo1 <- db.a.serie.de.tiempo(db1, conf.especificas = conf.de.interes)
series.de.tiempo2 <- db.a.serie.de.tiempo(db2, conf.especificas = conf.de.interes)
# series.de.tiempo3 <- db.a.serie.de.tiempo(db3, conf.especificas = conf.de.interes)
# series.de.tiempo4 <- db.a.serie.de.tiempo(db4, conf.especificas = conf.de.interes)

# plots ----
color.i <- c('#b71c1c', '#8e44ad', '#2980b9', '#27ae60', '#f1c40f', '#ff6f00')
nombre.etiquetas <- colnames(series.de.tiempo1)
longitud.conf.de.interes <- length(conf.de.interes)
titulo1 <- paste('Temperature', nombre.estacion1, sep = ' - ')
titulo2 <- paste('Temperature', nombre.estacion2, sep = ' - ')
# titulo3 <- paste('Temperature', nombre.estacion3, sep = ' - ')
# titulo4 <- paste('Temperature', nombre.estacion4, sep = ' - ')

setwd('/home/msomos/Documentos/WRF/plots_paper/')
# png('t2_series_de_tiempo.png', width = 850, height = 750, units = "px")
# png('t2_y_pp_series_de_tiempo.png', width = 850, height = 750, units = "px")
postscript(file = "t2_y_pp_series_de_tiempo.eps", width = 20, height = 20)

par(mfrow=c(2,2), mar=c(5, 5, 4, 2))

# 1
c(plot.xts(as.xts(series.de.tiempo1), main=titulo1, col = c('black', color.i), #legend.loc = TRUE,
           lty=c(1, rep(1,longitud.conf.de.interes)), lwd=c(2, rep(2,longitud.conf.de.interes))),
  
  addLegend("top", 
            legend.names=nombre.etiquetas,
            col=c('black', color.i),
            lty=c(1, rep(1,longitud.conf.de.interes)),
            lwd=c(4, rep(4,longitud.conf.de.interes)),
            ncol=1,
            bg="white", cex=0.9) )[2]
mtext('°C', side = 2, line = 3.65, font = 1)
# 2
c(plot.xts(as.xts(series.de.tiempo2), main=titulo2, col = c('black', color.i), #legend.loc = TRUE,
           lty=c(1, rep(1,longitud.conf.de.interes)), lwd=c(2, rep(2,longitud.conf.de.interes))),
  
  addLegend("top", 
            legend.names=nombre.etiquetas,
            col=c('black', color.i),
            lty=c(1, rep(1,longitud.conf.de.interes)),
            lwd=c(4, rep(4,longitud.conf.de.interes)),
            ncol=1,
            bg="white", cex=0.9) )[2]
mtext('°C', side = 2, line = 3.65, font = 1)
# 3
# c(plot.xts(as.xts(series.de.tiempo3), main=titulo3, col = c('black', color.i), #legend.loc = TRUE,
#            lty=c(1, rep(1,longitud.conf.de.interes)), lwd=c(2, rep(2,longitud.conf.de.interes))),
#   
#   addLegend("top", 
#             legend.names=nombre.etiquetas,
#             col=c('black', color.i),
#             lty=c(1, rep(1,longitud.conf.de.interes)),
#             lwd=c(4, rep(4,longitud.conf.de.interes)),
#             ncol=1,
#             bg="white", cex=0.9) )[2]
# 
# # 4
# c(plot.xts(as.xts(series.de.tiempo4), main=titulo4, col = c('black', color.i), #legend.loc = TRUE,
#            lty=c(1, rep(1,longitud.conf.de.interes)), lwd=c(2, rep(2,longitud.conf.de.interes))),
#   
#   addLegend("top", 
#             legend.names=nombre.etiquetas,
#             col=c('black', color.i),
#             lty=c(1, rep(1,longitud.conf.de.interes)),
#             lwd=c(4, rep(4,longitud.conf.de.interes)),
#             ncol=1,
#             bg="white", cex=0.9) )[2]

# dev.off()
# ---





# otros ejemplos de plots ----

# autoplot(ej, ts.colour = "blue", ts.linetype = "dashed")
# plot(ej, plot.type = "multiple")
# plot(ej, plot.type = "single", lty = 1:24)
# chart.TimeSeries(as.xts(ej))

# fin ---