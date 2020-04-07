# CODICE R PER ANALISI DI IMMAGINI SATELLITARI

install.packages("raster")
library(raster)
setwd("C:/LAB")

p224r63_2011 <- brick("p224r63_2011_masked.grd") # assegnamo, tramite la funzione brick, un nome al file presente dentro la wd
plot(p224r63_2011)
