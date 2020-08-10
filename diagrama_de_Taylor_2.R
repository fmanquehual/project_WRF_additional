library(plotrix)
library(raster)
library(corrplot)
library(scales)

rm(list=ls())
dev.off()

# Llamada de funciones ----
setwd('/home/msomos/Documentos/WRF/proyecto_WRF/')

source('funcion_diagrama_de_Taylor_modificado_2.R')
source('funcion_tamanho_pch_bias.R')
source('funcion_xy_diagrama_de_Taylor_modificado_2.R')
source('funcion_calculo_de_maximos_y_minimos_para_leyenda.R')
# fin ---

setwd('/home/msomos/Documentos/WRF/revision_paper/datos_listos_para_diagrama_de_Taylor/')
# db.ordenado.i <- read.csv('datos_para_el_diagrama_de_Taylor_t2_MultipleForcing.csv')
db.ordenado.i <- read.csv('datos_para_el_diagrama_de_Taylor_pp_MultipleForcing.csv')
db.ordenado.i$etiqueta <- c(1:6)

# detalles de plot ----

db.ordenado.i$bias <- round(db.ordenado.i$bias, 0) # 0 en pp, 1 decimal para t2 multipleforcing
min.bias <-  round(min(db.ordenado.i$bias), 0) ; min.bias
max.bias <-  round(max(db.ordenado.i$bias), 0) ; max.bias
intervalo <- 10 # 10 en pp, 0.25 (t2 multipleforcing)

minimo.leyenda <- calculo.maximo.y.minimo.de.leyenda(min.bias, intervalo, 'minimo') ; minimo.leyenda
maximo.leyenda <- calculo.maximo.y.minimo.de.leyenda(max.bias, intervalo, 'maximo') ; maximo.leyenda
intervalos.para.leyenda <- seq(minimo.leyenda, maximo.leyenda, by=intervalo) ; intervalos.para.leyenda
longitud.intervalo <- length(intervalos.para.leyenda) ; longitud.intervalo
n <- as.character( seq(minimo.leyenda, maximo.leyenda, by=1) ) # 1 en pp, 0.1 (t2 multipleforcing)

db.bias.y.color <- data.frame( n.bias=n, color.bias=viridis_pal()(length(n)) )
db.bias.y.color$color.bias <- as.character(db.bias.y.color$color.bias)

pch.bias <- 16
tamanho.pch.bias.leyenda <- 3

# fin ---



# plot que integra a todas las estaciones ----
setwd('/home/msomos/Documentos/WRF/revision_paper/plots_diagramas_Taylor/')

# png('pp_MultipleForcing.png', width = 580, height = 480, units = "px")
#cairo_ps("t2_y_pp_MultipleForcing.eps", width = 15)

#par(mfrow=c(1,2))

color.i <- db.bias.y.color$color.bias[as.character( db.bias.y.color$n.bias )%in%as.character( db.ordenado.i$bias[1] )]

diagrama.de.Taylor.modificado.2(R.i = db.ordenado.i$r[1], sd.ref.i = db.ordenado.i$sd.ref[1], sd.model.i = db.ordenado.i$sd.modelo[1],
                                tamanho.punto.de.referencia=1, pos.cor=TRUE, col = color.i, pcex = tamanho.pch.bias.leyenda, 
                                pch = pch.bias, show.gamma = TRUE, ancho.linea.de.pch = 0.5, main = '', xlab = 'Standard deviation (normalized)')
text(db.ordenado.i$coord.x[1], db.ordenado.i$coord.y[1], db.ordenado.i$etiqueta[1], cex=0.9, font=2, col='white')

for (i in 2:6) {
  # i <- 5
  color.i <- db.bias.y.color$color.bias[as.character( db.bias.y.color$n.bias )%in%as.character( db.ordenado.i$bias[i] )]
  
  diagrama.de.Taylor.modificado.2(R.i = db.ordenado.i$r[i], sd.ref.i = db.ordenado.i$sd.ref[i], sd.model.i = db.ordenado.i$sd.modelo[i],
                                  add=TRUE, col = color.i, pcex = tamanho.pch.bias.leyenda, pch = pch.bias, ancho.linea.de.pch = 0.5)
  text(db.ordenado.i$coord.x[i], db.ordenado.i$coord.y[i], db.ordenado.i$etiqueta[i], cex=0.9, font=2, col='white')
}

leyenda.i <- paste('(', db.ordenado.i$etiqueta, ')', ' ', db.ordenado.i$conf, sep = '') ; leyenda.i

colorlegend(db.bias.y.color$color.bias, intervalos.para.leyenda, xlim = c(1.55, 1.7), ylim = c(0.5, 1.5))
text(1.72, 1, 'Bias (%)', srt = 270) # 'Bias' en t2,  'Bias (%)' en pp
legend(0.5, 1.7, legend=leyenda.i, horiz=FALSE, cex = 0.71, ncol = 2, lty = NULL, text.font = 2)
text(1.72, 1.7, '(b)', font = 2) # '(a)' en t2, '(b)' en pp

# dev.off()

# fin ---
