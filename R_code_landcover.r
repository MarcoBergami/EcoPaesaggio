# R Code Land Cover

library(raster)
library(RStoolbox)
setwd("C:/LAB")

p224r63_2011 <- brick("p224r63_2011_masked.grd")
plotRGB(p224r63_2011,r=4,g=3,b=2, stretch="Lin")
p224r63_2011c <- unsuperClass(p224r63_2011, nClasses=4) # classifichiamo l'immagine, accorpando le bande dei pixel in diverse classi

# > d1c
# unsuperClass results

*************** Map ******************
$map
class      : RasterLayer 
dimensions : 478, 714, 341292  (nrow, ncol, ncell)
resolution : 1, 1  (x, y)
extent     : 0, 714, 0, 478  (xmin, xmax, ymin, ymax)
crs        : NA 
source     : memory
names      : layer 
values     : 1, 2  (min, max)


plot(p224r63_2011c$map)
clclass <- colorRampPalette(c('red', 'green', 'blue', 'black'))(100) 
plot(p224r63_2011c$map, col=clclass)

p224r63_2011c <- unsuperClass(p224r63_2011, nClasses=2)
plot(p224r63_2011c$map)
