calculo.maximo.y.minimo.de.leyenda <- function(valor, intervalo, tipo){

dividendo <- valor
divisor <- intervalo
  
  if(tipo=='maximo'){
    
    cociente <- dividendo/divisor
    residuo <- cociente-trunc(cociente)
    
    if(residuo!=0){
        repeat{
          entero <- trunc(cociente)
          decimal <- cociente - entero
          diferencia <- 1 - decimal
          dividendo <- dividendo + decimal + diferencia
          
          cociente <- dividendo/divisor
          residuo <- cociente-trunc(cociente)
          if(residuo==0){break}
        }
      maximo.leyenda <- dividendo
    } else(maximo.leyenda <- dividendo)
  return(maximo.leyenda)  
  }
  
  if(tipo=='minimo'){
    
    cociente <- dividendo/divisor
    residuo <- cociente-trunc(cociente)
    
    if(residuo!=0){
        repeat{
          entero <- trunc(cociente)
          decimal <- cociente - entero
          diferencia <- 1 - decimal
          dividendo <- dividendo - (decimal + diferencia)
     
          cociente <- dividendo/divisor
          residuo <- cociente-trunc(cociente)
          if(residuo==0){break}
        }
      minimo.leyenda <- dividendo
    } else(minimo.leyenda <- dividendo)
  return(minimo.leyenda)
  }

}