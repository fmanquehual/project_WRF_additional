library(rgdal)
require(xtable)

rm(list=ls())
dev.off()

# funciones ----

# 1
diferenciacion_entre_X_e_Y <- function(data_base, variable){
  #data_base <-  db8
  #variable <- 'ws'
  
  columnas_de_las_variables <- c(10:16)
  if(variable=='pp'){columna.i <- columnas_de_las_variables[1]; variable.id <- pp$id}
  if(variable=='rh'){columna.i <- columnas_de_las_variables[2]; variable.id <- rh$id}
  if(variable=='t2'){columna.i <- columnas_de_las_variables[4]; variable.id <- t2$id}
  if(variable=='tmax'){columna.i <- columnas_de_las_variables[5]; variable.id <- tmax$id}
  if(variable=='tmin'){columna.i <- columnas_de_las_variables[6]; variable.id <- tmin$id}
  if(variable=='ws'){columna.i <- columnas_de_las_variables[7]; variable.id <- ws$id}
  if(variable=='sp'){stop}
  
  data_base[,columna.i] <- as.character(data_base[,columna.i])
  match1 <- which(data_base$id%in%variable.id)
  match2 <- which(data_base[,columna.i]=='-')
  
  no.utilizado <- intersect(match1, match2)
  data_base[no.utilizado, columna.i] <- 'O'
  
  for (i in 1:length(no.utilizado)) {
    #i <- 1
    if(data_base[no.utilizado[i],'rep']>=2){est.name.rep <- data_base[no.utilizado[i],'id']} else(next)
    id.rep <- which(data_base$id%in%est.name.rep)
    
    if( length(which(data_base[id.rep, columna.i]=='X'))>=1 ){data_base[id.rep, columna.i] <- 'X'}
    
    otras.columas <- setdiff(columnas_de_las_variables, columna.i)
    data_base[id.rep,]
    
      for (j in otras.columas) {
        #j <- 14
        data_base[,j] <- as.character(data_base[,j])
        data_base[id.rep, j]
        
        if(length(which(data_base[id.rep, j]=='X'))>=1){data_base[id.rep, j] <- 'X'} else(
          if(length(which(data_base[id.rep, j]=='O'))>=1){data_base[id.rep, j] <- 'O'} else(
            if(length(which(data_base[id.rep, j]=='-'))>=1){data_base[id.rep, j] <- '-'}
          )
        )
        
      }
  }
  return(data_base)
}

# fin ---

setwd('/home/msomos/Documentos/WRF/Estadisticos/puntajes_finales_y_estaciones_para_mapas/')

db <- read.csv('db_todas_las_instituciones_y_las_usadas.csv', dec = ',')
db$names <- as.character(db$names)
db$WS <- '-'
db$id <- paste(db$names, db$institucion, sep = '_')
db


setwd('/home/msomos/Documentos/WRF/Estadisticos/puntajes_finales_y_estaciones_para_mapas/estaciones_para_los_mapas/')

pp <- read.csv('PrecipitationStation.csv', dec = ',')
pp$var <- 'pp'
pp$id <- paste(pp$Station.Name, pp$Institution, sep = '_')

rh <- read.csv('RHStation.csv', dec = ',')
rh$var <- 'rh'
rh$id <- paste(rh$Station.Name, rh$Institution, sep = '_')

t2 <- read.csv('T2Station.csv', dec = ',')
t2$var <- 't2'
t2$id <- paste(t2$Station.Name, t2$Institution, sep = '_')

tmax <- read.csv('TmaxStation.csv', dec = ',')
tmax$var <- 'tmax'
tmax$id <- paste(tmax$Station.Name, tmax$Institution, sep = '_')

tmin <- read.csv('TminStation.csv', dec = ',')
tmin$var <- 'tmin'
tmin$id <- paste(tmin$Station.Name, tmin$Institution, sep = '_')

ws <- read.csv('VelocidadVientoStation.csv', dec = ',')
ws$var <- 'ws'
ws$id <- paste(ws$Station.Name, ws$Institution, sep = '_')

t <- rbind(pp, rh, t2, tmax, tmin, ws)
t

t.2 <- t[!duplicated(t$id), ]
t.3 <- t.2[, c('Station.Name', 'Institution', 'var', 'Lat', 'Lon', 'id')]
row.names(t.3) <- 1:nrow(t.3)
t.3$Station.Name <- as.character(t.3$Station.Name)
t.3
# ---


# ---
db.names <- db$id
t.names <- t.3$id
dif.name <- setdiff(t.names, db.names)
idx <- which(t.3$id%in%dif.name)
t.4 <- t.3[idx,]

colnames(db)
colnames(t.4) <- c('names', 'institucion', 'var', 'lat', 'lon', 'id')
t.5 <- t.4[,c('names', 'institucion', 'lat', 'lon', 'id')]
t.5$names_modi <- NA
t.5$names.ori <- NA
t.5$names.modi.ERA5 <- NA
t.5$usado <- 0
t.5$PP <- '-'
t.5$RH <- '-'
t.5$SP <- '-'
t.5$T2 <- '-'
t.5$Tmax <- '-'
t.5$Tmin <- '-'
t.5$WS <- '-'
t.6 <- t.5[,c(colnames(db)[1], colnames(db)[2], colnames(db)[3], colnames(db)[4], colnames(db)[5], colnames(db)[6],
            colnames(db)[7], colnames(db)[8], colnames(db)[9], colnames(db)[10], colnames(db)[11], colnames(db)[12],
            colnames(db)[13], colnames(db)[14], colnames(db)[15], colnames(db)[16])]

t.6$lat <- t.6$lat*-1
t.6$lon <- t.6$lon*-1
t.6

db2 <- rbind(db, t.6)

id.rep <- names(table(db2$id))
num.rep <- as.vector(unlist(table(db2$id)))
db.rep <- data.frame(id=id.rep, rep=num.rep)

db3 <- merge(db2, db.rep, by='id')
db3
# ---


# preparacion tabla ----

# pp
pp$id <- as.character(pp$id)
db4 <- diferenciacion_entre_X_e_Y(db3, 'pp')

# rh
rh$id <- as.character(rh$id)
db5 <- diferenciacion_entre_X_e_Y(db4, 'rh')

# t2
t2$id <- as.character(t2$id)
db6 <- diferenciacion_entre_X_e_Y(db5, 't2')

# tmax
tmax$id <- as.character(tmax$id)
db7 <- diferenciacion_entre_X_e_Y(db6, 'tmax')

# tmin
tmin$id <- as.character(tmin$id)
db8 <- diferenciacion_entre_X_e_Y(db7, 'tmin')

# WS
ws$id <- as.character(ws$id)
estaciones.WS.ocupados <- c('Bajada Ibanez_INIA', 'Caleta Tortel_CDOM', 'Chile Chico_INIA', 'Cochrane_INIA',
                            'Rio Colonia en Nacimiento_GDGA', 'Laguna San Rafael_GDGA', 'Rio Nef Antes Junta Estero El Revalse_GDGA',
                            'HNG San Rafael_DGA', 'Tamelaike_INIA', 'Vista Hermosa_INIA')
db8$WS[db8$id%in%estaciones.WS.ocupados] <- 'X'
db9 <- db8
db9

db10 <- db9[!duplicated(db9$id),]
db11 <- db10[order(db10$names),]
row.names(db11) <- 1:nrow(db11)
db11
colnames(db11)

db12 <- db11[, c('names', 'institucion', 'lat', 'lon', 'PP', 'RH', 'SP', 'T2', 'Tmax', 'Tmin', 'WS')]
colnames(db12) <- c('Station', 'Institution', 'Lat', 'Lon', 'PP', 'RH', 'SP', 'T2', 'Tmax', 'Tmin', 'WS')
db12

length(db12$WS[db12$WS=='X'])
length(db12$WS[db12$WS=='O'])

# ---

# ---
db12$PP <- as.character(db12$PP)
db12$RH <- as.character(db12$RH)
db12$SP <- as.character(db12$SP)
db12$T2 <- as.character(db12$T2)
db12$Tmax <- as.character(db12$Tmax)
db12$Tmin <- as.character(db12$Tmin)

db12$PP[db12$PP%in%'X'] <- 'Yes'
db12$RH[db12$RH%in%'X'] <- 'Yes'
db12$SP[db12$SP%in%'X'] <- 'Yes'
db12$T2[db12$T2%in%'X'] <- 'Yes'
db12$Tmax[db12$Tmax%in%'X'] <- 'Yes'
db12$Tmin[db12$Tmin%in%'X'] <- 'Yes'
db12$WS[db12$WS%in%'X'] <- 'Yes'

db12$PP[db12$PP%in%'O'] <- 'No'
db12$RH[db12$RH%in%'O'] <- 'No'
db12$SP[db12$SP%in%'O'] <- 'No'
db12$T2[db12$T2%in%'O'] <- 'No'
db12$Tmax[db12$Tmax%in%'O'] <- 'No'
db12$Tmin[db12$Tmin%in%'O'] <- 'No'
db12$WS[db12$WS%in%'O'] <- 'No'

db12

setwd('/home/msomos/Documentos/WRF/plots_paper/')
# write.csv(db12, 'Tabla_est_usadas_y_no_usadas.csv', row.names = FALSE)

xtable(db12)
# fin ---







# SHP coordenadas de estaciones ----
wgs84 <- '+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'

db12$id <- 1:nrow(db12)
est <- SpatialPointsDataFrame(coords=db12[,c('Lon', 'Lat')],data=db12, proj4string = CRS(wgs84))
plot(est, axes=TRUE)

setwd('/home/msomos/Documentos/WRF/coberturas/coberturas_ok/')
# writeOGR(est, ".", "estaciones_metereologicas", driver="ESRI Shapefile", overwrite_layer = TRUE)