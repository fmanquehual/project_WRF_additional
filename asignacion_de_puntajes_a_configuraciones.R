require('corrplot')
library('viridis')
library('xtable')
library('BBmisc')

rm(list=ls())
dev.off()

# funciones ----

# 1
db_puntaje <- function(data_frame, estadistico){
  if(estadistico=='CORRE' | estadistico=='KGE'){decreasing.i <- TRUE} else(decreasing.i <- FALSE)
  
  # estadistico='PCBIAS'
  # data_frame <- db3.PCBIAS
  # decreasing.i=FALSE
  
  db.out <- c()
  for (i in 2:ncol(data_frame)) {
    # i <- 7
    data_frame.i <- data_frame[,c(1,i)]
    #head(data_frame.i)
    
    if(estadistico=='PCBIAS' | estadistico=='BIAS'){
      data_frame.i[,2] <- abs(data_frame.i[,2])
    }
    
    data_frame.i2 <- data_frame.i[order(data_frame.i[,2], decreasing = decreasing.i),]
    data_frame.i2$puntaje <- 1:nrow(data_frame.i2)
    #data_frame.i2
    
    data_frame.i3 <- data_frame.i2[,c('X', 'puntaje')]
    name.conf <- names(data_frame.i2)[2]
    
    names(data_frame.i3) <- c('X', name.conf)
    #head(data_frame.i3)
    
    if(i==2){db.out <- data_frame.i3} else(db.out <- merge(db.out, data_frame.i3, by='X'))
    print( paste(i-1, 'de', ncol(data_frame)-1, sep = ' ') )
    
    if(i==ncol(data_frame)){ print('Listo') }
  }
  return(db.out)
}

# 2
db_to_matriz <- function(data_base){
  col.i <- colnames(data_base)[-1]
  col.j <- gsub('_', ' ', col.i)
  row.i <- as.character(data_base[,1])
  values.i <- t(as.matrix(data_base[,-1]))
  m <- matrix(values.i, nrow = nrow(values.i), ncol = ncol(values.i), byrow = FALSE, 
              dimnames = list(col.j, row.i))
  return(m)
}

# fin ---




# Pp ----
#setwd('/home/msomos/Documentos/WRF/Estadisticos/Estadisticos/')
setwd('C:/Users/Usuario/Documents/Francisco/WRF/Estadisticos/Estadisticos/')

# Correlacion
db1.CORRE <- read.csv('CORRE_precipitationERAInterim.csv')
head(db1.CORRE)
dim(db1.CORRE)

db1.CORRE.pje <- db_puntaje(db1.CORRE, estadistico = 'CORRE') ; db1.CORRE.pje

# KGE
db1.KGE <- read.csv('KGE_precipitationERAInterim.csv')
head(db1.KGE)
dim(db1.KGE)

db1.KGE.pje <- db_puntaje(db1.KGE, estadistico = 'KGE') ; db1.KGE.pje

# Sesgo en porcentaje
db1.PCBIAS <- read.csv('PCBIAS_precipitationERAInterim.csv')
head(db1.PCBIAS)
dim(db1.PCBIAS)

db1.PCBIAS.pje <- db_puntaje(db1.PCBIAS, estadistico = 'PCBIAS') ; db1.PCBIAS.pje

# RSR
db1.RSR <- read.csv('RSR_precipitationERAInterim.csv')
head(db1.RSR)
dim(db1.RSR)

db1.RSR.pje <- db_puntaje(db1.RSR, estadistico = 'RSR') ; db1.RSR.pje

# fin ---


# Matriz ----
m1.CORRE.pje <- db_to_matriz(db1.CORRE.pje) ; m1.CORRE.pje
m1.KGE.pje <- db_to_matriz(db1.KGE.pje) ; m1.KGE.pje
m1.PCBIAS.pje <- db_to_matriz(db1.PCBIAS.pje) ; m1.PCBIAS.pje
m1.RSR.pje <- db_to_matriz(db1.RSR.pje) ; m1.RSR.pje

# pp <- m1.CORRE.pje + m1.KGE.pje + m1.PCBIAS.pje + m1.RSR.pje # 1 queda en 6to
pp <- m1.CORRE.pje + m1.KGE.pje + m1.RSR.pje # 1 queda en 5to
# pp <- m1.PCBIAS.pje + m1.KGE.pje + m1.RSR.pje # 1 queda en 7mo
# pp <- m1.KGE.pje + m1.RSR.pje # 1 queda en 6to
pp

values.i <- c()
for (j in 1:ncol(pp)) {
  if(j==1){values.i <- sum(pp[,j])} else(values.i <- c(values.i, sum(pp[,j])))  
}
values.i

pp.f0 <- data.frame(conf=colnames(pp), puntaje=values.i)
pp.f <- pp.f0[order(pp.f0$puntaje, decreasing = FALSE),]
rownames(pp.f) <- 1:nrow(pp.f)
pp.f

setwd('/home/msomos/Documentos/WRF/Estadisticos/Estadisticos/puntajes/')
# write.csv(pp.f, 'puntaje_precipitacion.csv', row.names = FALSE)

# fin ---







# HR ----
#setwd('/home/msomos/Documentos/WRF/Estadisticos/Estadisticos/')
setwd('C:/Users/Usuario/Documents/Francisco/WRF/Estadisticos/Estadisticos/')

# Correlacion
db2.CORRE <- read.csv('CORRE_RhERAInterim.csv')
head(db2.CORRE)
dim(db2.CORRE)

db2.CORRE.pje <- db_puntaje(db2.CORRE, estadistico = 'CORRE') ; db2.CORRE.pje

# KGE
db2.KGE <- read.csv('KGE_RhERAInterim.csv')
head(db2.KGE)
dim(db2.KGE)

db2.KGE.pje <- db_puntaje(db2.KGE, estadistico = 'KGE') ; db2.KGE.pje

# Sesgo en porcentaje
db2.PCBIAS <- read.csv('PCBIAS_RhERAInterim.csv')
head(db2.PCBIAS)
dim(db2.PCBIAS)

db2.PCBIAS.pje <- db_puntaje(db2.PCBIAS, estadistico = 'PCBIAS') ; db2.PCBIAS.pje

# RSR
db2.RSR <- read.csv('RSR_RhERAInterim.csv')
head(db2.RSR)
dim(db2.RSR)

db2.RSR.pje <- db_puntaje(db2.RSR, estadistico = 'RSR') ; db2.RSR.pje

# fin ---


# Matriz ----
m2.CORRE.pje <- db_to_matriz(db2.CORRE.pje) ; m2.CORRE.pje
m2.KGE.pje <- db_to_matriz(db2.KGE.pje) ; m2.KGE.pje
m2.PCBIAS.pje <- db_to_matriz(db2.PCBIAS.pje) ; m2.PCBIAS.pje
m2.RSR.pje <- db_to_matriz(db2.RSR.pje) ; m2.RSR.pje

# hr <- m2.CORRE.pje + m2.KGE.pje + m2.PCBIAS.pje + m2.RSR.pje # 1 queda en 6to
hr <- m2.CORRE.pje + m2.KGE.pje + m2.RSR.pje # 1 queda 5to 
# hr <- m2.PCBIAS.pje + m2.KGE.pje + m2.RSR.pje # 1 queda 7mo
# hr <- m2.KGE.pje + m2.RSR.pje # 1 queda 6to
hr

values.i <- c()
for (j in 1:ncol(hr)) {
  if(j==1){values.i <- sum(hr[,j])} else(values.i <- c(values.i, sum(hr[,j])))  
}
values.i

hr.f0 <- data.frame(conf=colnames(hr), puntaje=values.i)
hr.f <- hr.f0[order(hr.f0$puntaje, decreasing = FALSE),]
rownames(hr.f) <- 1:nrow(hr.f)
hr.f

setwd('/home/msomos/Documentos/WRF/Estadisticos/Estadisticos/puntajes/')
# write.csv(hr.f, 'puntaje_humedad_relativa.csv', row.names = FALSE)

# fin ---





# T2 ----
#setwd('/home/msomos/Documentos/WRF/Estadisticos/Estadisticos/')
setwd('C:/Users/Usuario/Documents/Francisco/WRF/Estadisticos/Estadisticos/')

# Correlacion
db3.CORRE <- read.csv('CORRE_T2ERAInterim.csv')
head(db3.CORRE)
dim(db3.CORRE)

db3.CORRE.pje <- db_puntaje(db3.CORRE, estadistico = 'CORRE') ; db3.CORRE.pje

# KGE
db3.KGE <- read.csv('KGE_T2ERAInterim.csv')
head(db3.KGE)
dim(db3.KGE)

db3.KGE.pje <- db_puntaje(db3.KGE, estadistico = 'KGE') ; db3.KGE.pje

# Sesgo
db3.PCBIAS <- read.csv('BIAS_T2ERAInterim.csv')
head(db3.PCBIAS)
dim(db3.PCBIAS)

db3.PCBIAS.pje <- db_puntaje(db3.PCBIAS, estadistico = 'PCBIAS') ; db3.PCBIAS.pje

# RSR
db3.RSR <- read.csv('RSR_T2ERAInterim.csv')
head(db3.RSR)
dim(db3.RSR)

db3.RSR.pje <- db_puntaje(db3.RSR, estadistico = 'RSR') ; db3.RSR.pje

# fin ---


# Matriz ----
m3.CORRE.pje <- db_to_matriz(db3.CORRE.pje) ; m3.CORRE.pje
m3.KGE.pje <- db_to_matriz(db3.KGE.pje) ; m3.KGE.pje
m3.PCBIAS.pje <- db_to_matriz(db3.PCBIAS.pje) ; m3.PCBIAS.pje
m3.RSR.pje <- db_to_matriz(db3.RSR.pje) ; m3.RSR.pje

# t2 <- m3.CORRE.pje + m3.KGE.pje + m3.PCBIAS.pje + m3.RSR.pje # 1 queda en 6to
t2 <- m3.CORRE.pje + m3.KGE.pje + m3.RSR.pje # 1 queda en 5to
# t2 <- m3.PCBIAS.pje + m3.KGE.pje + m3.RSR.pje # 1 queda en 7mo
# t2 <- m3.KGE.pje + m3.RSR.pje # 1 queda en 6to
t2

values.i <- c()
for (j in 1:ncol(t2)) {
  if(j==1){values.i <- sum(t2[,j])} else(values.i <- c(values.i, sum(t2[,j])))  
}
values.i

t2.f0 <- data.frame(conf=colnames(t2), puntaje=values.i)
t2.f <- t2.f0[order(t2.f0$puntaje, decreasing = FALSE),]
rownames(t2.f) <- 1:nrow(t2.f)
t2.f

setwd('/home/msomos/Documentos/WRF/Estadisticos/Estadisticos/puntajes/')
# write.csv(t2.f, 'puntaje_t2.csv', row.names = FALSE)

# fin ---






# TMAX ----
#setwd('/home/msomos/Documentos/WRF/Estadisticos/Estadisticos/')
setwd('C:/Users/Usuario/Documents/Francisco/WRF/Estadisticos/Estadisticos/')

# Correlacion
db4.CORRE <- read.csv('CORRE_TmaxERAInterim.csv')
head(db4.CORRE)
dim(db4.CORRE)

db4.CORRE.pje <- db_puntaje(db4.CORRE, estadistico = 'CORRE') ; db4.CORRE.pje

# KGE
db4.KGE <- read.csv('KGE_TmaxERAInterim.csv')
head(db4.KGE)
dim(db4.KGE)

db4.KGE.pje <- db_puntaje(db4.KGE, estadistico = 'KGE') ; db4.KGE.pje

# Sesgo en porcentaje
db4.PCBIAS <- read.csv('BIAS_TmaxERAInterim.csv')
head(db4.PCBIAS)
dim(db4.PCBIAS)

db4.PCBIAS.pje <- db_puntaje(db4.PCBIAS, estadistico = 'PCBIAS') ; db4.PCBIAS.pje

# RSR
db4.RSR <- read.csv('RSR_TmaxERAInterim.csv')
head(db4.RSR)
dim(db4.RSR)

db4.RSR.pje <- db_puntaje(db4.RSR, estadistico = 'RSR') ; db4.RSR.pje

# fin ---


# Matriz ----
m4.CORRE.pje <- db_to_matriz(db4.CORRE.pje) ; m4.CORRE.pje
m4.KGE.pje <- db_to_matriz(db4.KGE.pje) ; m4.KGE.pje
m4.PCBIAS.pje <- db_to_matriz(db4.PCBIAS.pje) ; m4.PCBIAS.pje
m4.RSR.pje <- db_to_matriz(db4.RSR.pje) ; m4.RSR.pje

# tmax <- m4.CORRE.pje + m4.KGE.pje + m4.PCBIAS.pje + m4.RSR.pje # 1 queda en 6to
tmax <- m4.CORRE.pje + m4.KGE.pje + m4.RSR.pje # 1 queda en 5to
# tmax <- m4.PCBIAS.pje + m4.KGE.pje + m4.RSR.pje # 1 queda en 7mo
# tmax <- m4.KGE.pje + m4.RSR.pje # 1 queda en 6to
tmax

values.i <- c()
for (j in 1:ncol(tmax)) {
  if(j==1){values.i <- sum(tmax[,j])} else(values.i <- c(values.i, sum(tmax[,j])))  
}
values.i

tmax.f0 <- data.frame(conf=colnames(tmax), puntaje=values.i)
tmax.f <- tmax.f0[order(tmax.f0$puntaje, decreasing = FALSE),]
rownames(tmax.f) <- 1:nrow(tmax.f)
tmax.f

setwd('/home/msomos/Documentos/WRF/Estadisticos/Estadisticos/puntajes/')
# write.csv(tmax.f, 'puntaje_temperatura_maxima.csv', row.names = FALSE)

# fin ---





# TMIN ----
#setwd('/home/msomos/Documentos/WRF/Estadisticos/Estadisticos/')
setwd('C:/Users/Usuario/Documents/Francisco/WRF/Estadisticos/Estadisticos/')

# Correlacion
db5.CORRE <- read.csv('CORRE_TminERAInterim.csv')
head(db5.CORRE)
dim(db5.CORRE)

db5.CORRE.pje <- db_puntaje(db5.CORRE, estadistico = 'CORRE') ; db5.CORRE.pje

# KGE
db5.KGE <- read.csv('KGE_TminERAInterim.csv')
head(db5.KGE)
dim(db5.KGE)

db5.KGE.pje <- db_puntaje(db5.KGE, estadistico = 'KGE') ; db5.KGE.pje

# Sesgo en porcentaje
db5.PCBIAS <- read.csv('BIAS_TminERAInterim.csv')
head(db5.PCBIAS)
dim(db5.PCBIAS)

db5.PCBIAS.pje <- db_puntaje(db5.PCBIAS, estadistico = 'PCBIAS') ; db5.PCBIAS.pje

# RSR
db5.RSR <- read.csv('RSR_TminERAInterim.csv')
head(db5.RSR)
dim(db5.RSR)

db5.RSR.pje <- db_puntaje(db5.RSR, estadistico = 'RSR') ; db5.RSR.pje

# fin ---


# Matriz ----
m5.CORRE.pje <- db_to_matriz(db5.CORRE.pje) ; m5.CORRE.pje
m5.KGE.pje <- db_to_matriz(db5.KGE.pje) ; m5.KGE.pje
m5.PCBIAS.pje <- db_to_matriz(db5.PCBIAS.pje) ; m5.PCBIAS.pje
m5.RSR.pje <- db_to_matriz(db5.RSR.pje) ; m5.RSR.pje

# tmin <- m5.CORRE.pje + m5.KGE.pje + m5.PCBIAS.pje + m5.RSR.pje # 1 queda en 6to
tmin <- m5.CORRE.pje + m5.KGE.pje + m5.RSR.pje # 1 queda en 5to
# tmin <- m5.PCBIAS.pje + m5.KGE.pje + m5.RSR.pje # 1 queda en 7mo
# tmin <- m5.KGE.pje + m5.RSR.pje # 1 queda en 6to
tmin

values.i <- c()
for (j in 1:ncol(tmin)) {
  if(j==1){values.i <- sum(tmin[,j])} else(values.i <- c(values.i, sum(tmin[,j])))  
}
values.i

tmin.f0 <- data.frame(conf=colnames(tmin), puntaje=values.i)
tmin.f <- tmin.f0[order(tmin.f0$puntaje, decreasing = FALSE),]
rownames(tmin.f) <- 1:nrow(tmin.f)
tmin.f

setwd('/home/msomos/Documentos/WRF/Estadisticos/Estadisticos/puntajes/')
# write.csv(tmin.f, 'puntaje_temperatura_minima.csv', row.names = FALSE)

# fin ---




# Puntaje final ----
names(pp.f) <- c('conf', 'pp')
names(hr.f) <- c('conf', 'hr')
names(t2.f) <- c('conf', 't2')
# names(tmax.f) <- c('conf', 'tmax')
# names(tmin.f) <- c('conf', 'tmin')


db1 <- merge(pp.f, hr.f, by='conf')
db2 <- merge(db1, t2.f, by='conf')
# db3 <- merge(db2, tmax.f, by='conf')
# db4 <- merge(db3, tmin.f, by='conf')
db2

# estandarizacion ---
par(mfrow=c(1,2))
boxplot(db2$pp, db2$hr, db2$t2, main='Escala original', names = c('pp', 'hr', 't2'))
db3 <- normalize(db2, method = 'range', range = c(0, 1))
db3[,2] <- round(db3[,2], 2)
db3[,3] <- round(db3[,3], 2)
db3[,4] <- round(db3[,4], 2)

boxplot(db3$pp, db3$hr, db3$t2, main='Escala nueva', names = c('pp', 'hr', 't2'))
db3[order(db3$pp),]

# db5 <- db_to_matriz(db4)
# db5
db5 <- db_to_matriz(db3)
db5

values.i <- c()
for (j in 1:ncol(db5)) {
  if(j==1){values.i <- sum(db5[,j])} else(values.i <- c(values.i, sum(db5[,j])))  
}
values.i

db6 <- data.frame(conf=colnames(db5), puntaje=values.i)
dbf <- db6[order(db6$puntaje, decreasing = FALSE),]
rownames(dbf) <- 1:nrow(dbf)
dbf

setwd('/home/msomos/Documentos/WRF/Estadisticos/Estadisticos/puntajes/')
# write.csv(dbf, 'puntaje_considerando_todas_las_variables.csv', row.names = FALSE)

# fin ---




# tabla ----

orden.i <- c(1, 10:19, 2, 20:24, 3:9)
db4$orden <- orden.i

db6 <- db4[order(db4$orden),]
db7 <- db6[,-ncol(db6)]
str(db7)

values.i <- c()
for (j in 1:nrow(db7)) {
  if(j==1){values.i <- sum(db7[j,2:6])} else(values.i <- c(values.i, sum(db7[j,2:6])))  
}
values.i

db7$Total <- values.i
rownames(db7) <- 1:nrow(db7)
db7

print(xtable(db7), include.rownames=FALSE)
