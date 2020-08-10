sd.diagrama.de.Taylor.modificado <- function (ref, model, type='model', sd.method = "sample", normalize=FALSE) {
  
  if (is.list(ref)) 
    ref <- unlist(ref)
  if (is.list(model)) 
    ref <- unlist(model)
  SD <- function(x, subn) {
    meanx <- mean(x, na.rm = TRUE)
    devx <- x - meanx
    ssd <- sqrt(sum(devx * devx, na.rm = TRUE)/(length(x[!is.na(x)]) - 
                                                  subn))
    return(ssd)
  }
  subn <- sd.method != "sample"
  sd.r <- SD(ref, subn)
  sd.f <- SD(model, subn)
  
  if (normalize) {
    sd.f <- sd.f/sd.r
    sd.r <- 1
  }
  
  if(type=='model'){out <- sd.f} else(out <- sd.r)  
  
  return(out)
}
