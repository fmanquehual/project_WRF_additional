xy.diagrama.de.Taylor.modificado.2 <- function (r, sd.ref, sd.modelo, sd.method = "sample") {
  R <- r
  subn <- sd.method != "sample"
  sd.r <- sd.ref
  sd.f <- sd.modelo
  
  db.out <- data.frame(x=sd.f*R, y=sd.f*sin(acos(R)))
  return(db.out)
}
