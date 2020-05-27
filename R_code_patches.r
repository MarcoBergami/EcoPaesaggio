setwd("C:/LAB")

library(ggplot2)
library(raster)

d1c <- raster("d1c.tif")
d2c <- raster("d2c.tif")

cl <- colorRampPalette(c('black','green'))(100)
par(mfrow=c(1,2))
plot(d1c, col=cl)
plot(d2c, col=cl)
# classe 1 = agricoltura, classe 2 = foresta

# riclassificamio le immagini eliminando la classe di agricoltura attraverso la funzione "cbind"
d1c.for <- reclassify(d1c, cbind(1, NA))

par(mfrow=c(1,2))
plot(d1c, col=cl)
plot(d1c.for, col=cl)

# facciamo lo stesso per la seconda immagine
d2c.for <- reclassify(d2c, cbind(1, NA))

par(mfrow=c(1,2))
plot(d1c.for, col=cl)
plot(d2c.for, col=cl)

# creiamo le patches
d1c.for.patches <- clump(d1c.for)
d2c.for.patches <- clump(d2c.for)

# per creare i file appena riclassificati all'interno della wd
# writeRaster(d1c.for.patches, "d1c.for.patches.tif")
# writeRaster(d2c.for.patches, "d2c.for.patches.tif")

# apllichiamo una crp capace di mettre in evidenza la varie patches
clp <- colorRampPalette(c('dark blue','blue','green','orange','yellow','red'))(100)
par(mfrow=c(1,2))
plot(d1c.for.patches, col=clp)
plot(d2c.for.patches, col=clp)

d1c.for.patches
# values     : 1, 301  (min, max) 
# il valore max corrisponde al numero delle patches
# max patches d1 = 301
# max patches d1 = 1212

# creiamo il dataframe
time <- c("Before deforestation","After deforestation")
npatches <- c(301,1212)
output <- data.frame(time,npatches)
attach(output)

ggplot(output, aes(x=time, y=npatches, color="red")) + geom_bar(stat="identity", fill="white")
 






