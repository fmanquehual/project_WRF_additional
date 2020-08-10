library(rgdal)
library(rgeos)
library(dplyr)

rm(list=ls())
dev.off()

# funciones ----

# 1
db_to_matriz <- function(data_base){
  col.j <- colnames(data_base)[-1]
  #col.j <- gsub('_', ' ', col.i)
  row.i <- as.character(data_base[,1])
  values.i <- t(as.matrix(data_base[,-1]))
  m <- matrix(values.i, nrow = nrow(values.i), ncol = ncol(values.i), byrow = FALSE, 
              dimnames = list(col.j, row.i))
  return(m)
}

# 2
estadisticos_por_estacion <- function(matrix.j, vector_con_estaciones){
  
  # matrix.j <- m1.PCBIAS
  # vector_con_estaciones <- c(10, 14, 2, 9, 22)
  matrix.i <- matrix.j[,vector_con_estaciones]
  
  values.i <- c()
  for (i in 1:nrow(matrix.i)) {
    if(i==1){values.i <- mean(matrix.i[i,])} else(values.i <- c(values.i, mean(matrix.i[i,])))  
  }
  values.j <- round(values.i, 2)
  
  values.k <- c()
  for (j in 1:nrow(matrix.i)) {
    if(j==1){values.k <- sd(matrix.i[j,])} else(values.k <- c(values.k, sd(matrix.i[j,])))  
  }
  values.l <- round(values.k, 2)
  
  names.i <- row.names(matrix.i)
  db.i <- data.frame(est=names.i, Mean=values.j, SD=values.l)
  db.i$Mean.abs <- abs(db.i$Mean)
  #db.i
  
  db.j <- db.i[order(db.i[,4], decreasing = FALSE),]
  idx <- as.numeric(row.names(db.j))
  rownames(db.j) <- 1:nrow(db.j)
  db.j

  db.j$mejor.conf <- NA
  db.j$mejor.valor <- NA
  db.j

  for (k in idx) {
    #k <- idx[1]
    fila.k <- which(idx[]==k)
    id.k <- as.vector(which.min(abs(matrix.i[k,])))
    db.j$mejor.conf[fila.k] <- colnames(matrix.i)[id.k]
    db.j$mejor.valor[fila.k] <- round(matrix.i[k,id.k],2)
  }
  
  return(db.j)
}

# fin ---

wgs84 <- '+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'
utm18 <- "+proj=utm +zone=18 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0" #utm zona 18 sur con datum WGS84
utm19 <- "+proj=utm +zone=19 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0" #utm zona 19 sur con datum WGS84


# ----
#setwd('/home/msomos/Documentos/WRF/Estadisticos/')
setwd('C:/Users/Usuario/Documents/Francisco/WRF/Estadisticos/puntajes_finales_y_estaciones_para_mapas/estaciones_para_los_mapas/')

df1 <- read.csv('PrecipitationStation.csv')
df2 <- read.csv('RHStation.csv')
df3 <- read.csv('T2Station.csv')
df4 <- read.csv('TmaxStation.csv')
df5 <- read.csv('TminStation.csv')
df6 <- rbind(df1, df2, df3, df4, df5)[,-1]


df <- distinct(df6) # quita duplicados
df$Lon <- df$Lon*-1
df$Lat <- df$Lat*-1
df

setwd('C:/Users/Usuario/Documents/Francisco/WRF/Estadisticos/puntajes_finales_y_estaciones_para_mapas/')
# write.csv(df, 'db_todas_las_instituciones_2.csv', row.names = FALSE)

# Coordenadas puestas a mano!

db.est.xy <- read.csv('db_todas_las_instituciones.csv', sep = ';')
head(db.est.xy)

# fin ---










# ----
setwd('C:/Users/Usuario/Documents/Francisco/WRF/Estadisticos/puntajes_finales_y_estaciones_para_mapas/resume_a_las_otras_dos_carpetas/')

db1.PCBIAS <- read.csv('PCBIAS_precipitationERAInterim.csv')
head(db1.PCBIAS)
tail(db1.PCBIAS)

m1.PCBIAS <- db_to_matriz(db1.PCBIAS) ; m1.PCBIAS
est.m1.PCBIAS <- estadisticos_por_estacion(m1.PCBIAS, c(10, 14, 2, 9, 22)) ; est.m1.PCBIAS
est.m1.PCBIAS.ok <- merge(est.m1.PCBIAS, db.est.xy, by.x='est', by.y='names_modi')
est.m1.PCBIAS.ok
# est.m1.PCBIAS.ok <- est.m1.PCBIAS.pre.ok[!duplicated(est.m1.PCBIAS.pre.ok$est),]
# est.m1.PCBIAS.ok
pp <- SpatialPointsDataFrame(coords=est.m1.PCBIAS.ok[,c('lon', 'lat')],data=est.m1.PCBIAS.ok, proj4string = CRS(wgs84))
plot(pp, axes=TRUE)

#setwd('/home/msomos/Documentos/WRF/coberturas/coberturas_ok/')
setwd('C:/Users/Usuario/Documents/Francisco/WRF/coberturas/coberturas_ok/')

# writeOGR(pp, ".", "estaciones_metereologicas_pp", driver="ESRI Shapefile", overwrite_layer = TRUE)

# ---
setwd('C:/Users/Usuario/Documents/Francisco/WRF/Estadisticos/puntajes_finales_y_estaciones_para_mapas/resume_a_las_otras_dos_carpetas/')
db2.PCBIAS <- read.csv('PCBIAS_RhERAInterim.csv')
head(db2.PCBIAS)
dim(db2.PCBIAS)

m2.PCBIAS <- db_to_matriz(db2.PCBIAS) ; m2.PCBIAS
est.m2.PCBIAS <- estadisticos_por_estacion(m2.PCBIAS, c(10, 14, 2, 9, 22)) ; est.m2.PCBIAS
est.m2.PCBIAS.ok <- merge(est.m2.PCBIAS, db.est.xy, by.x='est', by.y='names_modi')
# est.m2.PCBIAS.ok <- est.m2.PCBIAS.pre.ok[!duplicated(est.m2.PCBIAS.pre.ok$est),]
# est.m2.PCBIAS.ok

rh <- SpatialPointsDataFrame(coords=est.m2.PCBIAS.ok[,c('lon', 'lat')],data=est.m2.PCBIAS.ok, proj4string = CRS(wgs84))
plot(rh, axes=TRUE)

#setwd('/home/msomos/Documentos/WRF/coberturas/coberturas_ok/')
setwd('C:/Users/Usuario/Documents/Francisco/WRF/coberturas/coberturas_ok/')
# writeOGR(rh, ".", "estaciones_metereologicas_rh", driver="ESRI Shapefile", overwrite_layer = TRUE)

# ---
setwd('C:/Users/Usuario/Documents/Francisco/WRF/Estadisticos/puntajes_finales_y_estaciones_para_mapas/resume_a_las_otras_dos_carpetas/')
db3.PCBIAS <- read.csv('BIAS2_T2ERAInterim.csv')
head(db3.PCBIAS)
dim(db3.PCBIAS)

m3.PCBIAS <- db_to_matriz(db3.PCBIAS) ; m3.PCBIAS
est.m3.PCBIAS <- estadisticos_por_estacion(m3.PCBIAS) ; est.m3.PCBIAS
est.m3.PCBIAS.ok <- merge(est.m3.PCBIAS, db.est.xy, by.x='est', by.y='names_modi')
# est.m3.PCBIAS.ok <- est.m3.PCBIAS.pre.ok[!duplicated(est.m3.PCBIAS.pre.ok$est),]
# est.m3.PCBIAS.ok

t2 <- SpatialPointsDataFrame(coords=est.m3.PCBIAS.ok[,c('lon', 'lat')],data=est.m3.PCBIAS.ok, proj4string = CRS(wgs84))
plot(t2, axes=TRUE)

#setwd('/home/msomos/Documentos/WRF/coberturas/coberturas_ok/')
setwd('C:/Users/Usuario/Documents/Francisco/WRF/coberturas/coberturas_ok/')
# writeOGR(t2, ".", "estaciones_metereologicas_t2", driver="ESRI Shapefile", overwrite_layer = TRUE)

# ---
setwd('C:/Users/Usuario/Documents/Francisco/WRF/Estadisticos/puntajes_finales_y_estaciones_para_mapas/resume_a_las_otras_dos_carpetas/')
db4.PCBIAS <- read.csv('BIAS2_TmaxERAInterim.csv')
head(db4.PCBIAS)
dim(db4.PCBIAS)

m4.PCBIAS <- db_to_matriz(db4.PCBIAS) ; m4.PCBIAS
est.m4.PCBIAS <- estadisticos_por_estacion(m4.PCBIAS) ; est.m4.PCBIAS
est.m4.PCBIAS.ok <- merge(est.m4.PCBIAS, db.est.xy, by.x='est', by.y='names_modi')
# est.m4.PCBIAS.ok <- est.m4.PCBIAS.pre.ok[!duplicated(est.m4.PCBIAS.pre.ok$est),]
# est.m4.PCBIAS.ok

tmax <- SpatialPointsDataFrame(coords=est.m4.PCBIAS.ok[,c('lon', 'lat')],data=est.m4.PCBIAS.ok, proj4string = CRS(wgs84))
plot(tmax, axes=TRUE)

#setwd('/home/msomos/Documentos/WRF/coberturas/coberturas_ok/')
setwd('C:/Users/Usuario/Documents/Francisco/WRF/coberturas/coberturas_ok/')
# writeOGR(tmax, ".", "estaciones_metereologicas_tmax", driver="ESRI Shapefile", overwrite_layer = TRUE)

# ---
#setwd('/home/msomos/Documentos/WRF/Estadisticos/Estadisticos/')
#setwd('C:/Users/Usuario/Documents/Francisco/WRF/Estadisticos/Estadisticos/')
setwd('C:/Users/Usuario/Documents/Francisco/WRF/Estadisticos/puntajes_finales_y_estaciones_para_mapas/resume_a_las_otras_dos_carpetas/')

db5.PCBIAS <- read.csv('BIAS2_TminERAInterim.csv')
head(db5.PCBIAS)
dim(db5.PCBIAS)

m5.PCBIAS <- db_to_matriz(db5.PCBIAS) ; m5.PCBIAS
est.m5.PCBIAS <- estadisticos_por_estacion(m5.PCBIAS) ; est.m5.PCBIAS
est.m5.PCBIAS.ok <- merge(est.m5.PCBIAS, db.est.xy, by.x='est', by.y='names_modi')
# est.m5.PCBIAS.ok <- est.m5.PCBIAS.pre.ok[!duplicated(est.m5.PCBIAS.pre.ok$est),]
# est.m5.PCBIAS.ok

tmin <- SpatialPointsDataFrame(coords=est.m5.PCBIAS.ok[,c('lon', 'lat')],data=est.m5.PCBIAS.ok, proj4string = CRS(wgs84))
plot(tmin, axes=TRUE)

#setwd('/home/msomos/Documentos/WRF/coberturas/coberturas_ok/')
setwd('C:/Users/Usuario/Documents/Francisco/WRF/coberturas/coberturas_ok/')
# writeOGR(tmin, ".", "estaciones_metereologicas_tmin", driver="ESRI Shapefile", overwrite_layer = TRUE)

# ---
#setwd('/home/msomos/Documentos/WRF/Estadisticos/Estadisticos/')
#setwd('C:/Users/Usuario/Documents/Francisco/WRF/Estadisticos/Estadisticos/')
setwd('C:/Users/Usuario/Documents/Francisco/WRF/Estadisticos/puntajes_finales_y_estaciones_para_mapas/resume_a_las_otras_dos_carpetas/')

db6.PCBIAS <- read.csv('PCBIAS_PFSCERAInterim.csv')
head(db6.PCBIAS)
dim(db6.PCBIAS)

m6.PCBIAS <- db_to_matriz(db6.PCBIAS) ; m6.PCBIAS
est.m6.PCBIAS <- estadisticos_por_estacion(m6.PCBIAS) ; est.m6.PCBIAS
est.m6.PCBIAS.ok <- merge(est.m6.PCBIAS, db.est.xy, by.x='est', by.y='names_modi')
# est.m5.PCBIAS.ok <- est.m5.PCBIAS.pre.ok[!duplicated(est.m5.PCBIAS.pre.ok$est),]
# est.m5.PCBIAS.ok

ps <- SpatialPointsDataFrame(coords=est.m6.PCBIAS.ok[,c('lon', 'lat')],data=est.m6.PCBIAS.ok, proj4string = CRS(wgs84))
plot(ps, axes=TRUE)

#setwd('/home/msomos/Documentos/WRF/coberturas/coberturas_ok/')
setwd('C:/Users/Usuario/Documents/Francisco/WRF/coberturas/coberturas_ok/')
# writeOGR(ps, ".", "estaciones_metereologicas_ps", driver="ESRI Shapefile", overwrite_layer = TRUE)

