library(stringr)

rm(list=ls())
dev.off()

# funciones ----

# 1
prep_tablas <- function(matrix.i){
  # matrix.i <- n8
  
  d.i <- gsub('.csv', '', matrix.i)
  d.j <- data.frame(names=d.i)
  d.j$institucion <- NA
  
  num.nombres <- 1:length(d.j$names)
  
  gdga.i <- setdiff(num.nombres, which(!str_detect(d.j$names, 'GDGA')))
  dga.i <- setdiff(which(str_detect(d.j$names, 'DGA')), gdga.i)
  
  inia.i <- str_detect(d.j$names, 'INIA')
  dmc.i <- str_detect(d.j$names, 'DMC')
  cdom.i <- str_detect(d.j$names, 'CDOM')
  ghcn.i <- str_detect(d.j$names, 'GHCN')
  
  d.j$institucion[dga.i] <- 'DGA'
  d.j$institucion[inia.i] <- 'INIA'
  d.j$institucion[dmc.i] <- 'DMC'
  d.j$institucion[cdom.i] <- 'CDOM'
  d.j$institucion[ghcn.i] <- 'GHCN'
  d.j$institucion[gdga.i] <- 'GDGA'
  
  return(d.j)
}

# fin ---

#setwd('/home/msomos/Documentos/WRF/Estadisticos/Estadisticos_original/Estadisticos/')
setwd('C:/Users/Usuario/Documents/Francisco/WRF/Estadisticos/puntajes_finales_y_estaciones_para_mapas/resume_a_las_otras_dos_carpetas/')

d6 <- read.csv('CORRE_precipitationERAInterim.csv')
n6 <- colnames(d6)[-1]
n6

d7 <- read.csv('CORRE_RhERAInterim.csv')
n7 <- colnames(d7)[-1]
n7

d8 <- read.csv('CORRE_T2ERAInterim.csv')
n8 <- colnames(d8)[-1]
n8

d9 <- read.csv('CORRE_TmaxERAInterim.csv')
n9 <- colnames(d9)[-1]
n9

d10 <- read.csv('CORRE_TminERAInterim.csv')
n10 <- colnames(d10)[-1]
n10

# ----

d16 <- prep_tablas(n6)
d27 <- prep_tablas(n7)
d38 <- prep_tablas(n8)
d49 <- prep_tablas(n9)
d510 <- prep_tablas(n10)

d11 <- rbind(d16, d27, d38, d49, d510)
d12 <- d11[!duplicated(d11$names), ]
d13 <- d12[order(d12$names),]
d13$names.ori <- d13$names
rownames(d13) <- 1:nrow(d13)

d13$names <- gsub('_DGA', '', d13$names)
d13$names <- gsub('.DGA', '', d13$names)
d13$names <- gsub('_DMC', '', d13$names)
d13$names <- gsub('.DMC', '', d13$names)
d13$names <- gsub('_INIA', '', d13$names)
d13$names <- gsub('.INIA', '', d13$names)
d13$names <- gsub('_CDOM', '', d13$names)
d13$names <- gsub('.CDOM', '', d13$names)
d13$names <- gsub('_GHCN.', '', d13$names)
d13$names <- gsub('\\_', '', d13$names)
d13$names <- gsub('\\.', ' ', d13$names)
d13

setwd('C:/Users/Usuario/Documents/Francisco/WRF/Estadisticos/puntajes_finales_y_estaciones_para_mapas/')
# write.csv(d13, 'db_todas_las_instituciones.csv', row.names = FALSE)



# 
# # out ----
# # setwd('/home/msomos/Documentos/WRF/Estadisticos/')
# setwd('C:/Users/Usuario/Documents/Francisco/WRF/Estadisticos/puntajes_finales_y_estaciones_para_mapas/')
# 
# d13
# 
# ###
# d.dga0 <- subset(d13, dga == 1)
# row.names(d.dga0) <- 1:nrow(d.dga0)
# d.dga <- d.dga0[,c('names', 'dga')]
# d.dga$institucion <- 'dga'
# d.dga2 <- d.dga[,c('names', 'institucion')]
# 
# #write.csv(d.dga, 'db_dga.csv', row.names = FALSE)

# ###
# d.inia0 <- subset(d13, inia == 1)
# row.names(d.inia0) <- 1:nrow(d.inia0)
# d.inia <- d.inia0[,c('names_modi', 'names_ori')]
# d.inia
# 
# #write.csv(d.inia, 'db_inia.csv', row.names = FALSE)
# 
# ###
# d.dmc0 <- subset(d13, dmc == 1)
# row.names(d.dmc0) <- 1:nrow(d.dmc0)
# d.dmc <- d.dmc0[,c('names_modi', 'names_ori')]
# d.dmc
# 
# #write.csv(d.dmc, 'db_dmc.csv', row.names = FALSE)
# 
# ###
# d.cdom0 <- subset(d13, cdom == 1)
# row.names(d.cdom0) <- 1:nrow(d.cdom0)
# d.cdom <- d.cdom0[,c('names_modi', 'names_ori')]
# d.cdom
# 
# #write.csv(d.cdom, 'db_cdom.csv', row.names = FALSE)
# 
# ###
# d.ghcn0 <- subset(d13, ghcn == 1)
# row.names(d.ghcn0) <- 1:nrow(d.ghcn0)
# d.ghcn <- d.ghcn0[,c('names_modi', 'names_ori')]
# d.ghcn
# 
# #write.csv(d.ghcn, 'db_ghcn.csv', row.names = FALSE)
# 
# ###
# d.gdga0 <- subset(d13, gdga == 1)
# row.names(d.gdga0) <- 1:nrow(d.gdga0)
# d.gdga <- d.gdga0[,c('names_modi', 'names_ori')]
# d.gdga
# 
# #write.csv(d.gdga, 'db_ghcn.csv', row.names = FALSE)
# 
# # fin --- 
# 
# # LAS COORDENADAS FUERON A MANO !!!!!