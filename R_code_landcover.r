# R Code Land Cover

library(raster)
library(RStoolbox)
setwd("C:/LAB")

p224r63_2011 <- brick("p224r63_2011_masked.grd")
plotRGB(p224r63_2011,r=4,g=3,b=2, stretch="Lin")
p224r63_2011c <- unsuperClass(p224r63_2011, nClasses=4) # classifichiamo l'immagine, accorpando le bande dei pixel in diverse classi
plot(p224r63_2011c$map)
clclass <- colorRampPalette(c('red', 'green', 'blue', 'black'))(100) 
plot(p224r63_2011c$map, col=clclass)

p224r63_2011c <- unsuperClass(p224r63_2011, nClasses=2)
plot(p224r63_2011c$map)
