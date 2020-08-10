require(xtable)

rm(list=ls())
dev.off()

setwd('/home/msomos/Documentos/WRF/Estadisticos/puntajes_finales_y_estaciones_para_mapas/')

db <- read.csv('db_todas_las_instituciones_y_las_usadas.csv', dec = ',')
db$id <- paste(db$names, db$institucion, sep = '_')
db

db2 <- db[,c('id', 'names', 'institucion', 'lat', 'lon', 'PP', 'RH', 'SP', 'T2', 'Tmax', 'Tmin')]
colnames(db2) <- c('id', 'Station', 'Institution', 'Lat', 'Lon', 'PP', 'RH', 'SP', 'T2', 'Tmax', 'Tmin')
db2

names.i <- unique(db2$id)
subset.final <- c()

for (i in 1:length(names.i)) {
  #i <- 1
  names.i1 <- subset(db2, id==names.i[i])

# pp ----
names.i1$PP <- as.character(names.i1$PP)
is.true <- names.i1$PP=='X'
if(length(is.true[is.true%in%TRUE])!=0){names.i1$PP <- 'X'}
names.i1

# rh ----
names.i1$RH <- as.character(names.i1$RH)
is.true <- names.i1$RH=='X'
if(length(is.true[is.true%in%TRUE])!=0){names.i1$RH <- 'X'} else(names.i1$RH <- '-')
names.i1

# sp ----
names.i1$SP <- as.character(names.i1$SP)
is.true <- names.i1$SP=='X'
if(length(is.true[is.true%in%TRUE])!=0){names.i1$SP <- 'X'} else(names.i1$SP <- '-')
names.i1

# t2 ----
names.i1$T2 <- as.character(names.i1$T2)
is.true <- names.i1$T2=='X'
if(length(is.true[is.true%in%TRUE])!=0){names.i1$T2 <- 'X'} else(names.i1$T2 <- '-')
names.i1

# tmax ----
names.i1$Tmax <- as.character(names.i1$Tmax)
is.true <- names.i1$Tmax=='X'
if(length(is.true[is.true%in%TRUE])!=0){names.i1$Tmax <- 'X'} else(names.i1$Tmax <- '-')
names.i1

# tmin ----
names.i1$Tmin <- as.character(names.i1$Tmin)
is.true <- names.i1$Tmin=='X'
if(length(is.true[is.true%in%TRUE])!=0){names.i1$Tmin <- 'X'} else(names.i1$Tmin <- '-')
names.i1

# salida ----
if(i==1){subset.final <- names.i1} else(subset.final <- rbind(subset.final, names.i1))

}

subset.final

id.rep <- names(table(subset.final$id))
num.rep <- as.vector(unlist(table(subset.final$id)))
db.rep <- data.frame(id=id.rep, rep=num.rep)

db3 <- merge(subset.final, db.rep, by='id')
db3

columnas_variables_climaticas <- c(6:ncol(db3))
  for (i in 1:nrow(db3)) {
    #i <- 1
    if(db3[i,'rep']>=2){est.name.rep <- db3[i,'id']} else(next)
    id.rep <- which(db3$id%in%est.name.rep)
    
    for (j in columnas_variables_climaticas) {
      #j <- columnas_variables_climaticas[1]
    
        db3[,j] <- as.character(db3[,j])
        db3[id.rep, j]
        
        if(length(which(db3[id.rep, j]=='X'))>=1){db3[id.rep, j] <- 'X'} else(
          if(length(which(db3[id.rep, j]=='O'))>=1){db3[id.rep, j] <- 'O'} else(
            if(length(which(db3[id.rep, j]=='-'))>=1){db3[id.rep, j] <- '-'}
          )
        )
        
      }
  }
db3
# fin ---



db4 <- db3[!duplicated(db3$id),]
db5 <- db4[order(db4$Station),]
row.names(db5) <- 1:nrow(db5)
db6 <- db5[, c('Station', 'Institution', 'Lat', 'Lon', 'PP', 'RH', 'SP', 'T2', 'Tmax', 'Tmin')]
db6

xtable(db6)
