##### Snow cover analysis through Copernicus images

setwd("C:/LAB")
install.packages("ncdf4")

library(raster)
library(ncdf4)

snowmay <- raster("c_gls_SCE500_202005180000_CEURO_MODIS_V1.0.1.nc")
cl <- colorRampPalette(c('darkblue','blue','light blue'))(100)
plot(snowmay, col=cl)

setwd("C:/LAB/snow")
# importo tutte le immagini con lappy 
rlist=list.files(pattern=".tif", full.names=T) # creo la lista di file.tif situati dentro la wd
list_rast=lapply(rlist, raster)
snow.multitemp <- stack(list_rast)
plot(snow.multitemp, col=cl) # la copertura nevosa del 2000 e del 2005 è simulata

par(mfrow=c(1,2))
plot(snow.multitemp$snow2000r,col=cl, zlim=c(0,250)) # poniamo uguale il limite della legenda in entrambe le immagini
plot(snow.multitemp$snow2020r,col=cl, zlim=c(0,250))

difsnow = snow.multitemp$snow2020r - snow.multitemp$snow2000r
cldif <- colorRampPalette(c('blue','white','red'))(100) 
plot(difsnow, col=cldif)

### Prediction
# scaricare il file "prediction.r" da IOL nella cartella "snow". QUesto contiene la parte di codice per la previsione dei dati al 2025
source("prediction.r") # importare il codice dall'esterno - processo troppo lento

predicted.snow.2025.norm <- raster("predicted.snow.2025.norm.tif") # carichiamo l'immagine già sviluppata da IOL 
plot(predicted.snow.2025.norm, col=cl)
 




