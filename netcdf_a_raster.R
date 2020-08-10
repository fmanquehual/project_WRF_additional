require(ncdf4) #para tratamiento de datos nc
library(raster)
library(RColorBrewer)
library(rgdal)

rm(list=ls())
dev.off()

setwd('/home/msomos/Documentos/WRF/coberturas/')
# Preparacion de datos ----

m.cdf <- nc_open('geo_em.d02.nc')
print(m.cdf)

lat_variable <- 'XLAT_M'
lon_variable <- 'XLONG_M'
nc_variable <- 'HGT_M' # 'LAKE_DEPTH'

variable <- ncvar_get(m.cdf,nc_variable) #extracci?n valores variable
lats <- ncvar_get(m.cdf,lat_variable)
lons <- ncvar_get(m.cdf,lon_variable)

dims_variable <- dim(variable) #extracci?n dimensiones variable
tmp_mat <- matrix(variable[], nrow=dims_variable[1],ncol=dims_variable[2])
image(tmp_mat)

rotate2 <- function(x) t(apply(x, 2, rev))
rotate2(variable[])

r.i <- raster(xmn=min(lons), xmx=max(lons), ymn=min(lats), ymx=max(lats), nrow=dims_variable[1],ncol=dims_variable[2])
r.i[] <- rotate2(rotate2(rotate2(variable[])))
names(r.i) <- "altitud"
crs(r.i)
plot(r.i, col=brewer.pal(10, "BrBG"))

setwd('/home/msomos/Documentos/WRF/coberturas/')
#writeRaster(r.i, filename='area_estudio1_WRF.tif', format="GTiff", overwrite=TRUE)

r <- raster('ej_altitud_WRF.tif')
plot(r, col=brewer.pal(10, "BrBG"))
