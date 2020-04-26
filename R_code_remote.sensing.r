# CODICE R PER ANALISI DI IMMAGINI SATELLITARI

install.packages("raster")
library(raster)
setwd("C:/LAB")

p224r63_2011 <- brick("p224r63_2011_masked.grd") # assegnamo, tramite la funzione brick, un nome al file presente dentro la wd
plot(p224r63_2011) # visualizziamo le 7 immagini, una per ogni banda
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
plot(p224r63_2011$B1_sre,col=clb) # per la sola banda del blu, mettiamo in evidenza le alte riflettanze (light blue) e le basse (dark blue)
# per plottare soltanto la banda B1 potremmo utilizzare la funzione attach(p224r63_2011) ma purtroppo questa non funziona per il pacchetto raster. Dobbiamo quindi utilizzare il $

clnir <- colorRampPalette(c('red','orange','yellow'))(100) #facciamo lo stesso per la banda del vicino infrarosso
plot(p224r63_2011$B4_sre,col=clnir)

par(mfrow=c(2,2)) # creiamo il multipanel in grado di visualizzare 4 immagini in 2 righe e 2 colonne
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
plotRGB(p224r63_2011,r=3,g=2,b=1, stretch="Lin") # stretch: argomento a funzione per riuscire ad "allungare" la banda di colori se no la sovrapposizione fornisce un'immagine scura.
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

p224r63_1988 <- brick("p224r63_1988_masked.grd") # svolgiamo il medesimo esercizio sull'immagine del 1988
par(mfrow=c(2,2)) 
clb <- colorRampPalette(c('dark blue','blue','light blue'))(100)
plot(p224r63_1988$B1_sre,col=clb)
clg <- colorRampPalette(c('dark green','green','light green'))(100)
plot(p224r63_1988$B2_sre,col=clg)
clr <- colorRampPalette(c('dark red','red','pink'))(100)
plot(p224r63_1988$B3_sre,col=clr)
clnir <- colorRampPalette(c('red','orange','yellow'))(100)
plot(p224r63_1988$B4_sre,col=clnir)
# le color palette delle varie bande si potrebbero anche non riscrivere in quanto già salvate nella sessione precedente
dev.off()

plotRGB(p224r63_1988,r=3,g=2,b=1, stretch="Lin")
plotRGB(p224r63_1988,r=4,g=3,b=2, stretch="Lin")

#plot delle immagini dei due diversi anni messe a confronto, utilizzando la banda del NIR sulla componente "r"
par(mfrow=c(2,1))
plotRGB(p224r63_1988,r=4,g=3,b=2, stretch="Lin")
plotRGB(p224r63_2011,r=4,g=3,b=2, stretch="Lin")
dev.off()

#spectral indices - DVI
dvi1988 <- p224r63_1988$B4_sre - p224r63_1988$B3_sre # creiamo la nuova immagine data dalla sottrazione, pixel per pixel, tra la banda dell'infrarosso e quella del rosso
dvi2011 <- p224r63_2011$B4_sre - p224r63_2011$B3_sre # lo stesso per l'anno 2011

# multitemporal analysis
difdvi <- dvi2011 - dvi1988 # calcoliamo la differenza nell'indice per i due anni
cldifdvi <- colorRampPalette(c('red','white','blue'))(100)
plot(difdvi, col=cldifdvi)


par(mfrow=c(3,1)) # creiamo il multipanel con le immagini ad infrarosso dei due anni e la differenza nell'indice DVI
plotRGB(p224r63_1988,r=4,g=3,b=2, stretch="Lin")
plotRGB(p224r63_2011,r=4,g=3,b=2, stretch="Lin")
plot(difdvi, col=cldifdvi)
dev.off()

# Changing the grain (Resolution)
p224r63_2011 # possiamo visualizzare la natura del file e la sua risoluzione (o grana)

p224r63_2011lr <- aggregate(p224r63_2011, fact=10) # lr = low resolution con ingrandimento del pixel di 10 volte (media dei pixel originali contenuti nel nuovo pixel più grande)
par(mfrow=c(2,1))
plotRGB(p224r63_2011,r=4,g=3,b=2, stretch="Lin")
plotRGB(p224r63_2011lr,r=4,g=3,b=2, stretch="Lin")

p224r63_2011lr50 <- aggregate(p224r63_2011, fact=50) # ingrandimento di 50 volte

par(mfrow=c(3,1)) # mettiamo a confronto le varie risoluzioni sull'immagine del 2011
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin")
plotRGB(p224r63_2011lr, r=4, g=3, b=2, stretch="Lin")
plotRGB(p224r63_2011lr50, r=4, g=3, b=2, stretch="Lin")

dvi2011lr50 <- p224r63_2011lr50$B4_sre - p224r63_2011lr50$B3_sre # creiamo l'indice per il 2011 ma a bassa risoluzione
plot(dvi2011lr50)

p224r63_1988lr50 <- aggregate(p224r63_1988, fact=50) 
dvi1988lr50 <- p224r63_1988lr50$B4_sre - p224r63_1988lr50$B3_sre
difdvilr50 <- dvi2011lr50 - dvi1988lr50
cldifdvi <- colorRampPalette(c('red','white','blue'))(100)
plot(difdvilr50, col=cldifdvi)
dev.off()

par(mfrow=c(2,1)) # confrontiamo la variazione dell'indice nelle due risoluzioni
plot(difdvi, col=cldifdvi)
plot(difdvilr50, col=cldifdvi)



