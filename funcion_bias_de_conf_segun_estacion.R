library(stringi)

bias.de.conf.segun.estacion <- function(db_diagrama_Taylor, db_con_variable_de_interes, numero_columna, umbral_porcentaje_de_coincidencia=50){
  
  # db_diagrama_Taylor <- db.xy0
  # db_con_variable_de_interes <- db.bias
  # numero_columna <- 3
  # umbral_porcentaje_de_coincidencia <- 50

  # ---
  umbral.en.porcentaje <- umbral_porcentaje_de_coincidencia
  variable.columna2 <- db_con_variable_de_interes
  numero.columna2 <- numero_columna
  
  nombre1 <- unique( as.character(db_diagrama_Taylor$estacion) ) ; nombre1
  nombre2 <- nombre_estacion( colnames(variable.columna2)[numero.columna2] ) ; nombre2
  
  if(nombre1=='GLagunasanrafaelDG'){nombre1 <- 'LagunaSanRafaelDGA'}
  if(nombre1=='GriocoloniaennacimientoDGA'){nombre1 <- 'RioColoniaenNacimientoDGA'}
  
  if(stri_detect_fixed(nombre1, 'DGA') | stri_detect_fixed(nombre1, 'DG')){institucion <- 'DG'}
  if(stri_detect_fixed(nombre1, 'DMC')){institucion <- 'DMC'}
  if(stri_detect_fixed(nombre1, 'GDGA')){institucion <- 'GDGA'}
  if(stri_detect_fixed(nombre1, 'INIA')){institucion <- 'INIA'}
  if(stri_detect_fixed(nombre1, 'GHCN')){institucion <- 'GHCN'}
  if(stri_detect_fixed(nombre1, 'CDOM')){institucion <- 'CDOM'}
  
  nombre1.segregado <- unlist(strsplit(nombre1, ""))
  nombre2.segregado <- unlist(strsplit(nombre2, ""))
  
  longitud.nombre1.segregado <- length(nombre1.segregado)
  longitud.nombre2.segregado <- length(nombre2.segregado)
  
  min.i <- min(longitud.nombre1.segregado, longitud.nombre2.segregado)
  
  numero.de.verdaderos <- c()
  numero.de.falsos <- c()
  i.letras.match <- c()
  
  for (i in 1:min.i) {
    if(nombre1.segregado[i] == nombre2.segregado[i]){
      numero.de.verdaderos <- c(numero.de.verdaderos, 1)
      i.letras.match <- c(i.letras.match, i)
      } 
    else(numero.de.falsos <- c(numero.de.falsos, 1))  
  }
  
  if( is.null(i.letras.match) ){i.letras.match <- 0}
  
  nombre1.segregado2 <- nombre1.segregado[-i.letras.match]
  nombre2.segregado2 <- nombre2.segregado[-i.letras.match]
  
  longitud.nombre1.segregado2 <- length(nombre1.segregado2)
  longitud.nombre2.segregado2 <- length(nombre2.segregado2)
  
  min.j <- min(longitud.nombre1.segregado2, longitud.nombre2.segregado2)
  
  for (i in min.j:1) {
    if(min.j==0){break}
    if(nombre1.segregado2[i] == nombre2.segregado2[i]){numero.de.verdaderos <- c(numero.de.verdaderos, 1)} 
    if(!stri_detect_fixed(nombre2, institucion)){numero.de.verdaderos <- c()}
    else(numero.de.falsos <- c(numero.de.falsos, 1))  
  }
  
  numero.de.coincidencias <- length(numero.de.verdaderos)
  porcentaje.de.aciertos <- (numero.de.coincidencias*100)/min.i ; porcentaje.de.aciertos
  mensaje.de.no.match <- paste('No hubo match entre estaciones, coincidencias en el nombre es menor al ', umbral.en.porcentaje, '%', sep = '')
  
  if(porcentaje.de.aciertos>=umbral.en.porcentaje){
    nombre.conf0 <- as.character( variable.columna2[,'X'] )
    nombre.conf <- gsub('-', '\\.', nombre.conf0)
    bias.conf <- variable.columna2[,numero.columna2]
    db <- data.frame(est.var.bias=nombre2, conf=nombre.conf, bias=bias.conf)
    return(db)
  } else( message(mensaje.de.no.match) )
}
