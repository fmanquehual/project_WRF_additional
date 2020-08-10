library(plotrix)
library(raster)
library(corrplot)
library(scales)

rm(list=ls())
dev.off()

# Observaciones ----

# con show.gamma, desactivas las curvas RMSD.
# las curvas RMSD, dependen de los SD de referencia y modelados.
# - rango de valores para la leyenda, que sea con el min y max de los datos por variable
# - En ves de los triangulos y cambios en el tamanho, que sea de color (- con color frio y + con calido)
# fin ---


# Llamada de funciones ----
setwd('/home/msomos/Documentos/WRF/proyecto_WRF/')
source('funcion_diagrama_de_Taylor_modificado.R')
source('funcion_nombre_estacion.R')
source('funcion_SD_diagrama_de_Taylor_modificado.R')
source('funcion_diagrama_de_Taylor_modificado_2.R')
source('funcion_bias_de_conf_segun_estacion.R')
source('funcion_tamanho_pch_bias.R')
source('funcion_xy_diagrama_de_Taylor_modificado_2.R')
source('funcion_calculo_de_maximos_y_minimos_para_leyenda.R')
# fin ---


# Lectura de archivos y generacion de db's, a nivel de carpeta (variable) ---- 
# directorio <- '/home/msomos/Documentos/WRF/revision_paper/datos_diagrama_de_Taylor/ParaTaylorRain/ParaTaylorRain/'
# directorio <- '/home/msomos/Documentos/WRF/revision_paper/datos_diagrama_de_Taylor/ParaTaylorRH/ParaTaylorRH/'
# directorio <- '/home/msomos/Documentos/WRF/revision_paper/datos_diagrama_de_Taylor/ParaTaylorT2/ParaTaylorT2/'
# directorio <- '/home/msomos/Documentos/WRF/revision_paper/datos_diagrama_de_Taylor/ParaTaylorTmax/ParaTaylorTmax/'
# directorio <- '/home/msomos/Documentos/WRF/revision_paper/datos_diagrama_de_Taylor/ParaTaylorTmin/ParaTaylorTmin/'
# directorio <- '/home/msomos/Documentos/WRF/revision_paper/datos_diagrama_de_Taylor/ParaTaylorRainForcingVarios/ParaTaylorRainForcingVarios/'
directorio <- '/home/msomos/Documentos/WRF/revision_paper/datos_diagrama_de_Taylor/ParaTaylorT2ForcingVarios/ParaTaylorT2ForcingVarios/'

# directorio.bias <- '/home/msomos/Documentos/WRF/Estadisticos/puntajes_finales_y_estaciones_para_mapas/resume_a_las_otras_dos_carpetas/'
# directorio.bias <- '/home/msomos/Documentos/WRF/Estadisticos/puntajes_finales_y_estaciones_para_mapas/forzantes_ERA5/MultipleForcing/MultipleForcing/Precipitation/'
directorio.bias <- '/home/msomos/Documentos/WRF/Estadisticos/puntajes_finales_y_estaciones_para_mapas/forzantes_ERA5/MultipleForcing/MultipleForcing/T2/'
setwd(directorio.bias)

#db.bias <- read.csv('PCBIAS_precipitationERAInterim.csv')
#db.bias <- read.csv('PCBIAS_RhERAInterim.csv')
#db.bias <- read.csv('PCBIAS_T2ERAInterim.csv')
#db.bias <- read.csv('PCBIAS_TmaxERAInterim.csv')
#db.bias <- read.csv('PCBIAS_TminERAInterim.csv')
#db.bias <- read.csv('PCBIAS_precipitationMultipleForcing.csv')
db.bias <- read.csv('BIAS_T2MultipleForcing.csv')

setwd(directorio)
nombres.db <- dir() ; nombres.db
todos.los.db.xy <- c()

for (j in 1:length(nombres.db)) {
  #j <- 1
  db <- read.csv(nombres.db[j])
  titulo.de.diagrama <- nombre_estacion(nombres.db[j])
  
  pch.i <- c(0:11, 0:11)
  color.pch <- c('black', 'red')
  taylor.diagram(db$Obs, db[,3], pos.cor=TRUE, col = color.pch[1], pcex = 1, pch = pch.i[1], normalize = TRUE, 
                 main = titulo.de.diagrama)
  
  sd.modelo.i <- sd.diagrama.de.Taylor.modificado(db$Obs, db[,3], type='model', normalize = TRUE)
  sd.ref.i <- sd.diagrama.de.Taylor.modificado(db$Obs, db[,3], type='ref', normalize = TRUE)
  r.i <- cor(db$Obs, db[,3], use = "pairwise")
  
  numero.columna.de.conf <- length(colnames(db))
  
  for (i in 4:numero.columna.de.conf) {
    # i <- 4
    pch.j <- c(0, 0, pch.i)
    
    if(i<=14){color.pch.i <- color.pch[1]} else(color.pch.i <- color.pch[2])
    taylor.diagram(db$Obs, db[,i], add=TRUE, pcex = 1, pch = pch.j[i], col = color.pch.i, normalize = TRUE)
    
    sd.modelo.j <- sd.diagrama.de.Taylor.modificado(db$Obs, db[,i], type='model', normalize = TRUE)
    sd.ref.j <- sd.diagrama.de.Taylor.modificado(db$Obs, db[,i], type='ref', normalize = TRUE)
    r.j <- cor(db$Obs, db[,i], use = "pairwise")
  
      sd.modelo.i <- c(sd.modelo.i, sd.modelo.j)
      sd.ref.i <- c(sd.ref.i, sd.ref.j)
      r.i <- c(r.i, r.j)
  }
  
  nombres.configuraciones <- colnames(db[3:numero.columna.de.conf])
  db.xy0 <- data.frame(estacion=nombre_estacion(nombres.db[j]), conf=nombres.configuraciones, r=r.i, sd.ref=sd.ref.i, 
                       sd.modelo=sd.modelo.i)
  
  for (k in 2:ncol(db.bias)) {
    #k <- 2
    db.bias.estacion <- bias.de.conf.segun.estacion(db.xy0, db.bias, k, 50)
    if( !is.null(db.bias.estacion) ){break}
  }
  
  db.xy0$conf <- as.character(db.xy0$conf)
  db.xy0$conf[db.xy0$conf%in%'ERA5_DomainConf9_3a1_40Levels_RAIN_2017.csv'] <- 'Conf9 ERA5 40 Levels'
  db.xy0$conf[db.xy0$conf%in%'FNL0832_DomainConf9_3a1_34Levels_RAIN_2017.csv'] <- 'Conf9 FNL0832 34 Levels'
  db.xy0$conf[db.xy0$conf%in%'Era5Domain2Conf10_5a1_RAIN_2017.csv'] <- 'Conf9 ERA5 5 to 1 34 Levels'
  db.xy0$conf[db.xy0$conf%in%'ERA5_DomainConf9_3a1_34Levels_RAIN_2017.csv'] <- 'Conf9 ERA5 3 to 1 34 Levels'
  db.xy0$conf[db.xy0$conf%in%'configuration9_RAIN_2017.csv'] <- 'Conf9 ERAinterim 34 Levels'
  db.xy0$conf[db.xy0$conf%in%'configuration10_RAIN_2017.csv'] <- 'Conf10 ERAinterim 34 Levels'
  
  db.bias.estacion$conf <- as.character(db.bias.estacion$conf)
  db.bias.estacion$conf[db.bias.estacion$conf%in%'ERA5 Conf9 40Levels'] <- 'Conf9 ERA5 40 Levels'
  db.bias.estacion$conf[db.bias.estacion$conf%in%'FNL0832 Conf9 34Levels'] <- 'Conf9 FNL0832 34 Levels'
  db.bias.estacion$conf[db.bias.estacion$conf%in%'Era5 Conf9 5to1  34Levels'] <- 'Conf9 ERA5 5 to 1 34 Levels'
  db.bias.estacion$conf[db.bias.estacion$conf%in%'ERA5 Conf9 3to1  34Levels'] <- 'Conf9 ERA5 3 to 1 34 Levels'
  db.bias.estacion$conf[db.bias.estacion$conf%in%'configuration9 ERAinterim  34Levels'] <- 'Conf9 ERAinterim 34 Levels'
  db.bias.estacion$conf[db.bias.estacion$conf%in%'configuration10 ERAinterim  34Levels'] <- 'Conf10 ERAinterim 34 Levels'
  
  db.xy <- merge(db.xy0, db.bias.estacion, by='conf')
  todos.los.db.xy <- rbind(todos.los.db.xy, db.xy)

  name.out <- paste('db.xy', '.', nombre_estacion(nombres.db[j]), '<-db.xy', sep = '')
  eval(parse(text=name.out))

  print( paste('Archivo', j, 'de', length(nombres.db), sep = ' ') )
 }

# fin ---





# Preparacion db final ----
todos.los.db.xy

todos.los.db.xy$estacion <- as.character(todos.los.db.xy$estacion)
nombres.conf.i <- names(tapply(todos.los.db.xy$r, todos.los.db.xy$conf, mean)) ; nombres.conf.i
r.mean.conf.i <- as.vector(tapply(todos.los.db.xy$r, todos.los.db.xy$conf, mean)) ; r.mean.conf.i
sd.ref.mean.conf.i <- as.vector(tapply(todos.los.db.xy$sd.ref, todos.los.db.xy$conf, mean)) ; sd.ref.mean.conf.i
sd.modelo.mean.conf.i <- as.vector(tapply(todos.los.db.xy$sd.modelo, todos.los.db.xy$conf, mean)) ; sd.modelo.mean.conf.i
bias.modelo.mean.conf.i <- as.vector(tapply(todos.los.db.xy$bias, todos.los.db.xy$conf, mean)) ; bias.modelo.mean.conf.i
x <- xy.diagrama.de.Taylor.modificado.2(r.mean.conf.i, sd.ref.mean.conf.i, sd.modelo.mean.conf.i)[,'x']
y <- xy.diagrama.de.Taylor.modificado.2(r.mean.conf.i, sd.ref.mean.conf.i, sd.modelo.mean.conf.i)[,'y']

db.i <- data.frame(conf=nombres.conf.i, r=r.mean.conf.i, sd.ref=sd.ref.mean.conf.i, 
                   sd.modelo=sd.modelo.mean.conf.i, bias=bias.modelo.mean.conf.i, coord.x=x, coord.y=y) ; db.i

# db.i$n <- as.numeric(gsub('Conf.', '', db.i$conf)) ; db.i # activar cuando NO sea multipleforcing
# db.i$conf <- gsub('\\.', '-', db.i$conf) ; db.i # activar cuando NO sea multipleforcing
# db.ordenado.i <- db.i[order(db.i$n),] ; db.ordenado.i # activar cuando NO sea multipleforcing
db.ordenado.i <- db.i # activar cuando sea multipleforcing

setwd('/home/msomos/Documentos/WRF/revision_paper/datos_listos_para_diagrama_de_Taylor/')
# write.csv(db.ordenado.i, 'datos_para_el_diagrama_de_Taylor_pp_ERAinterim.csv', row.names = FALSE)
# write.csv(db.ordenado.i, 'datos_para_el_diagrama_de_Taylor_t2_ERAinterim.csv', row.names = FALSE)
# write.csv(db.ordenado.i, 'datos_para_el_diagrama_de_Taylor_pp_MultipleForcing.csv', row.names = FALSE)
# write.csv(db.ordenado.i, 'datos_para_el_diagrama_de_Taylor_t2_MultipleForcing.csv', row.names = FALSE)

# fin ---


  
# detalles de plot ----

db.ordenado.i$bias <- round(db.ordenado.i$bias, 0)
min.bias <-  round(min(db.ordenado.i$bias), 0) ; min.bias
max.bias <-  round(max(db.ordenado.i$bias), 0) ; max.bias
intervalo <- 10

minimo.leyenda <- calculo.maximo.y.minimo.de.leyenda(min.bias, intervalo, 'minimo') ; minimo.leyenda
maximo.leyenda <- calculo.maximo.y.minimo.de.leyenda(max.bias, intervalo, 'maximo') ; maximo.leyenda
intervalos.para.leyenda <- seq(minimo.leyenda, maximo.leyenda, by=intervalo) ; intervalos.para.leyenda
longitud.intervalo <- length(intervalos.para.leyenda) ; longitud.intervalo
n <- seq(minimo.leyenda, maximo.leyenda, by=1)

db.bias.y.color <- data.frame( n.bias=n, color.bias=viridis_pal()(length(n)) )
db.bias.y.color$color.bias <- as.character(db.bias.y.color$color.bias)

pch.bias <- 16
tamanho.pch.bias.leyenda <- 2.2

# fin ---



# plot que integra a todas las estaciones ----
setwd('/home/msomos/Documentos/WRF/revision_paper/plots_diagramas_Taylor/')
# png('tmin.png', width = 580, height = 480, units = "px")

color.i <- db.bias.y.color$color.bias[db.bias.y.color$n.bias%in%db.ordenado.i$bias[1]]

diagrama.de.Taylor.modificado.2(R.i = db.ordenado.i$r[1], sd.ref.i = db.ordenado.i$sd.ref[1], sd.model.i = db.ordenado.i$sd.modelo[1],
                                tamanho.punto.de.referencia=1, pos.cor=TRUE, col = color.i, pcex = tamanho.pch.bias.leyenda, 
                                pch = pch.bias, show.gamma = TRUE, ancho.linea.de.pch = 0.5, main = '', xlab = 'Standard deviation (normalized)')
text(db.ordenado.i$coord.x[1], db.ordenado.i$coord.y[1], db.ordenado.i$n[1], cex=0.7, font=2, col='white')

for (i in 2:24) {
  # i <- 3
  color.i <- db.bias.y.color$color.bias[db.bias.y.color$n.bias%in%db.ordenado.i$bias[i]]
  
  diagrama.de.Taylor.modificado.2(R.i = db.ordenado.i$r[i], sd.ref.i = db.ordenado.i$sd.ref[i], sd.model.i = db.ordenado.i$sd.modelo[i],
                                add=TRUE, col = color.i, pcex = tamanho.pch.bias.leyenda, pch = pch.bias, ancho.linea.de.pch = 0.5)
  text(db.ordenado.i$coord.x[i], db.ordenado.i$coord.y[i], db.ordenado.i$n[i], cex=0.7, font=2, col='white')
}

colorlegend(db.bias.y.color$color.bias, intervalos.para.leyenda, xlim = c(1.55, 1.7), ylim = c(0.5, 1.5))
text(1.72, 1, 'Bias (%)', srt = 270)

dev.off()
# fin ---
