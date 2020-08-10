tamanho.pch.bias <- function(bias){
  #bias <- -29
  # positivos
  if(bias>100){tamanho.pch.bias.i <- 4}
  if(bias>50 & bias<=100){tamanho.pch.bias.i <- 3.5}
  if(bias>10 & bias<=50){tamanho.pch.bias.i <- 3}
  if(bias>5 & bias<=10){tamanho.pch.bias.i <- 2.5}
  if(bias>1 & bias<=5){tamanho.pch.bias.i <- 2}
  if(bias>0 & bias<=1){tamanho.pch.bias.i <- 1.5}
  
  # neutro
  if(bias==0){tamanho.pch.bias.i <- 1}
  
  # negativo
  if(bias>-1 & bias<=0){tamanho.pch.bias.i <- 1.5}
  if(bias>-5 & bias<=-1){tamanho.pch.bias.i <- 2}
  if(bias>-10 & bias<=-5){tamanho.pch.bias.i <- 2.5}
  if(bias>-50 & bias<=-10){tamanho.pch.bias.i <- 3}
  if(bias>-100 & bias<=-50){tamanho.pch.bias.i <- 3.5}
  if(bias<(-100)){tamanho.pch.bias.i <- 4}
  
  return(tamanho.pch.bias.i)
}