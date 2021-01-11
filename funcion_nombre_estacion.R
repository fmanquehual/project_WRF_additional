nombre_estacion <- function(nombre_del_archivo){
  ej1 <- gsub('ParaTaylor', '', nombre_del_archivo)
  ej2 <- gsub('.csv', '', ej1)
  ej3 <- gsub(' ', '', ej2)
  ej4 <- gsub('_', '', ej3)
  ej5 <- gsub('csv', '', ej4)
  ej6 <- gsub('\\.', '', ej5)
  ej7 <- gsub('Lord', '', ej6)
  ej8 <- gsub('2017', '', ej7)
  ej9 <- gsub('2010', '', ej8)
  
  return(ej9)
}
