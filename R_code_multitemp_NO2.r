# R CODE for analysing NO2 data from ESA - January to March 

setwd("C:/LAB")

library(raster)
EN01 <- raster("EN_0001.png")
plot(EN01)
# utilizziamo la funzione raster per caricare il singolo layer (mono-banda, solo quello relativo a NO2) delle immagini satellitari 
# (diversa da brick che invece permette di caricare tutti i layer dei vari sensori)
EN02 <- raster("EN_0002.png")
EN03 <- raster("EN_0003.png")
EN04 <- raster("EN_0004.png")
EN05 <- raster("EN_0005.png")
EN06 <- raster("EN_0006.png")
EN07 <- raster("EN_0007.png")
EN08 <- raster("EN_0008.png")
EN09 <- raster("EN_0009.png")
EN10 <- raster("EN_0010.png")
EN11 <- raster("EN_0011.png")
EN12 <- raster("EN_0012.png")
EN13 <- raster("EN_0013.png")

cl <- colorRampPalette(c('red','orange','yellow'))(100)
plot(EN01, col=cl)
plot(EN13, col=cl)

par(mfrow=c(1,2))
plot(EN01, col=cl)
plot(EN13, col=cl)

difno2 <- EN13-EN01
cldif <- colorRampPalette(c('blue','black','yellow'))(100)
plot(difno2, col=cldif)

par(mfrow=c(4,4))
plot(EN01, col=cl)
plot(EN02, col=cl)
plot(EN03, col=cl)
plot(EN04, col=cl)
plot(EN05, col=cl)
plot(EN06, col=cl)
plot(EN07, col=cl)
plot(EN08, col=cl)
plot(EN09, col=cl)
plot(EN10, col=cl)
plot(EN11, col=cl)
plot(EN12, col=cl)
plot(EN13, col=cl)

##### Utilizziamo la funzione lapply per caricare più immagini contemporaneamente
# load("multitemp.NO2.RData")
# ls()
rlist = list.files(pattern = ".png") # chiamiamo rlist l'intero intervallo di file con estensione .png presenti all'interno della cartella "esa_no2"
# utilizziamo la funzione "lapply", e più in particolare la funzione raster al suo interno, per caricare i file di rlist
listafinale = lapply(rlist, raster)
EN <- stack(listafinale) # creiamo un pacchetto delle immagini in modo da poterle plottare
plot(EN, col=cl)



