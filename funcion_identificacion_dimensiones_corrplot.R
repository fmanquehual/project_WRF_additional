dimenciones_grafico_corrplot <- function(matriz, eje='x'){
  # matriz <- m3
  # eje <- 'x'
  # 
  if(eje=='x'){
    xy.i <- identificacion.rangos.corrplot(matriz)[1]
    xy.f <- identificacion.rangos.corrplot(matriz)[2]
  } else( c(
    xy.i <- identificacion.rangos.corrplot(matriz)[3],
    xy.f <- identificacion.rangos.corrplot(matriz)[4] )
  )
  
  if(xy.i<0 & xy.f<0){dim.xy <- abs(xy.f)-abs(xy.i)}
  if(xy.i<0 & xy.f>=0){dim.xy <- xy.f+xy.i}
  if(xy.i>=0 & xy.f>=0){dim.xy <- xy.f-xy.i}

  return(dim.xy)
}