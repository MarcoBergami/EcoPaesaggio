# CODICE R PER ANALISI DI IMMAGINI SATELLITARI

install.packages("raster")
library(raster)
setwd("C:/LAB")

p224r63_2011 <- brick("p224r63_2011_masked.grd") # assegnamo, tramite la funzione brick, un nome al file presente dentro la wd
plot(p224r63_2011)
# B1: blue
# B2: green
# B3: red
# B4: near infrared (nir)
# B5: medium infrared
# B6: thermal infrared
# B7: medium infrared

cl <- colorRampPalette(c('black','grey','light grey'))(100) # assegnamo all'oggetto cl una diversa tavola di colori
plot(p224r63_2011,col=cl)
names(p224r63_2011) # visualizzaizmo il nome delle bande contenute nel dataset
clb <- colorRampPalette(c('dark blue','blue','light blue'))(100) # definiamo una tavola di colori per la banda del blu
plot(p224r63_2011$B1_sre,col=clb)
# per plottare soltanto la banda B1 potremmo utilizzare la funzione attach(p224r63_2011) ma purtroppo questa non funziona per il pacchetto raster. Dobbiamo quindi utilizzare il $

clnir <- colorRampPalette(c('red','orange','yellow'))(100) #facciamo lo stesso per la banda del vicino infrarosso
plot(p224r63_2011$B4_sre,col=clnir)

par(mfrow=c(2,2)) # creiamo il multipanel in grafo di visualizzare 4 immagini in 2 righe e 2 colonne
clb <- colorRampPalette(c('dark blue','blue','light blue'))(100)
plot(p224r63_2011$B1_sre,col=clb)
clg <- colorRampPalette(c('dark green','green','light green'))(100)
plot(p224r63_2011$B2_sre,col=clg)
clr <- colorRampPalette(c('dark red','red','pink'))(100) # il colore 'light red' non esiste
plot(p224r63_2011$B3_sre,col=clr)
clnir <- colorRampPalette(c('red','orange','yellow'))(100)
plot(p224r63_2011$B4_sre,col=clnir)

dev.off() # per chiudere il device, ovvero la finestra del grafico

plotRGB(p224r63_2011,r=3,g=2,b=1) # visualizzare l'immagine satellitare come la vedrebbe l'occhio umano, cioè sovrapponendo le tre bande RGB
plotRGB(p224r63_2011,r=3,g=2,b=1, stretch="Lin") # stretch: argomento a funzione per riuscire ad "allungare" la banda di colori.
# per inserire nella composizione anche l'infrarosso vicino ci occorre escludere una banda tra le tre già usate.
# Scaliamo quindi il numero delle bande sui rispettivi argomenti per inserire l'infrarosso (4) sulla componente red (r)
plotRGB(p224r63_2011,r=4,g=3,b=2, stretch="Lin") # visualizziamo la vegetazione rossa e il suolo agricolo in celeste

pdf("ImageRemoteSensing.pdf") # salvare l'immagine come pdf nella wd
plotRGB(p224r63_2011,r=4,g=3,b=2,stretch="Lin")
dev.off()

par(mfrow=c(1,2)) # visualizziamo entrambe le immagini una a fianco all'altra
plotRGB(p224r63_2011, r=3, g=2, b=1, stretch="Lin")
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin")
dev.off()

plotRGB(p224r63_2011, r=3, g=4, b=2, stretch="Lin") # visualizziamo la vegetazione verde e il suolo viola 
plotRGB(p224r63_2011, r=3, g=2, b=4, stretch="Lin") # visualizziamo la vegetazione blu e il suolo giallo
