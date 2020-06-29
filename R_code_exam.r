#########    R_code_exam.r    ##########

# 1. R_code_first.r   
# 2. R_code_spatial.r + R_code_spatial2.r
# 3. R_code_point_pattern
# 4. R_code_teleril.r 
# 5. R_code_landcover.r
# 6. R_code_multitemp.r
# 7. R_code_multitemp_NO2.r
# 8. R_code_snow.r 
# 9. R_code_patches.r 
# 10. R_code_crop.r
# 11. Species Distribution Modelling
# 12. Exam project





#################################################################################################################
#################################################################################################################






### 1. R_code_primocod.r - PRIMO CODICE ECOLOGIA DEL PAESAGGIO

install.packages("sp")

library(sp) # MB: richiamiamo la libreria sp prima di poterne utilizzare i dati all'interno
data(meuse) # MB: richiamiamo il dataset
meuse # MB: visualizziamo il dataset intero
names(meuse) # MB: visualizziamo solo il nome delle variabili
head(meuse) #MB: visualizziamo solo le prime 6 righe della tabella

pairs(meuse) # MB: produciamo una matrice di grafici a dispersione 
# MB: la diagonale della matrice mostra i nomi delle variabili
# MB: le altre celle della matrice del diagramma mostrano i diagrammi a dispersione di ciascuna combinazione di variabili del nostro frame di dati. 

pairs(~ cadmium + copper + lead , data = meuse) # MB: specifichiamo le sole 3 variabili su cui utilizzare la funzione pairs
pairs(~ cadmium + copper + lead + zinc , data = meuse)
pairs(meuse[,3:6], col="red") # MB: specifichiamo le varibili definendo un intervallo di colonne e cambiamo colore
pairs(meuse[,3:6], col="red", pch=19, cex=3) # MB: cambiamo "point character" (simbolo grafico) e "character exageration" (grandezza simbolo)
pairs(meuse[,3:6], col="red", pch=19, cex=3, main="Primo pairs") # MB: inseriamo un titolo al grafico

panel.correlations <- function(x, y, digits=1, prefix="", cex.cor)
{
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(0, 1, 0, 1))
    r1=cor(x,y,use="pairwise.complete.obs")
    r <- abs(cor(x, y,use="pairwise.complete.obs"))

 

    txt <- format(c(r1, 0.123456789), digits=digits)[1]
    txt <- paste(prefix, txt, sep="")
    if(missing(cex.cor)) cex <- 0.9/strwidth(txt)
    text(0.5, 0.5, txt, cex = cex * r)
}

 

panel.smoothing <- function (x, y, col = par("col"), bg = NA, pch = par("pch"),
    cex = 1, col.smooth = "red", span = 2/3, iter = 3, ...)
{
    points(x, y, pch = pch, col = col, bg = bg, cex = cex)
    ok <- is.finite(x) & is.finite(y)
    if (any(ok))
        lines(stats::lowess(x[ok], y[ok], f = span, iter = iter),
            col = 1, ...)
}

 


panel.histograms <- function(x, ...)
{
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(usr[1:2], 0, 1.5) )
    h <- hist(x, plot = FALSE)
    breaks <- h$breaks; nB <- length(breaks)
    y <- h$counts; y <- y/max(y)
    rect(breaks[-nB], 0, breaks[-1], y, col="white", ...)
}
pairs(meuse[,3:6], lower.panel = panel.correlations, upper.panel = panel.smoothing, diag.panel = panel.histograms)
# MB: parte di codice che permette di suddividere il device grafico in pannello superiore, inferiore e diagonale
# MB: pannello inferiore = valore del coeffic. di correlazione
# MB: diagonale = grafico ad istogrammi
# MB: pannello superiore = grafico di correlazione

# EXERCISE: mettere come lower panel lo smoothing, come diagonal panel gli istogrammi e come upper panel le correlazioni 
pairs(meuse[,3:6], lower.panel = panel.smoothing , upper.panel = panel.correlations, diag.panel = panel.histograms)

# funzione plot
plot(meuse$cadmium , meuse$copper)
attach(meuse) # MB: "attacchiamo" il database in modo da non doverlo specificare ogni volta
plot(cadmium,copper)
plot(cadmium,copper, pch=17, col="green", main="primo plot")
plot(cadmium,copper, pch=17, col="green", main="primo plot", xlab="cadmio", ylab="rame") # MB: cambiamo in nome delle etichette sugli assi del grafico
plot(cadmium,copper, pch=17, col="green", main="primo plot", xlab="cadmio", ylab="rame", cex.lab=2, cex=2) # MB: cambiamo grandezza delle etichette e dei simboli




#################################################################################################################
#################################################################################################################





### 2. R_code_spatial.r - Funzioni spaziali

library(sp)

data(meuse) 
head(meuse) 

# MB: plottiamo insieme i dati realtivi al cadmio e al piombo
attach(meuse)
plot(cadmium,lead,col="red",pch=19,cex=2)

# exercise: plot di copper e zinc con simbolo trinagolo (17) e colore verde 
plot(copper,zinc,col="green",pch=17,cex=2) 
plot(copper,zinc,col="green",pch=17,cex=2,xlab="rame", ylab="zinco") # MB: cambiamo in nome delle etichette sugli assi del grafico

# MB: funzione multiframe o multipanel, necessaria per mostrare più grafici insieme, si chiama "par"
par(mfrow=c(1,2)) # MB: all'interno di c definiamo il numero di righe e colonne del multipanel
plot(cadmium,lead,col="red",pch=19,cex=2)
plot(copper,zinc,col="green",pch=17,cex=2)
# MB: ovviamente per utilizzare un multipanel con 2 righe e 1 colonna basta cambiare i numeri dentro la parentsi di c.

install.packages("GGally")
library(GGally)
# MB: richiamiamo la funzione pairs in GGally specificando un sottoinsieme delle variabili
ggpairs(meuse[,3:6])
# MB: sull'asse diagonale sono presenti le singole variabili e la distribuzione di frequenza dei dati
# MB: nel pannello superiore ci sono i valori del coeff. di correlazione.
# MB: nel pannello inferiore ci sono le nuvole di punti dei grafici di dispersione

# MB: per svolgere delle analisi spaziali occorre per prima cosa far capire ad R che il dataframe contiene anche dei valori di coordinate, x e y. 
# MB: usiamo la funzione "coordinates" contenuta nel pacchetto sp.
coordinates(meuse)=~x+y # la tilde è necessaria per raggruppare più colonne del dataset
plot(meuse)

# MB: funzione spplot per plottare i dati distribuiti nello spazio, in questo caso rispetto alla variabile zinco, indicata tra "".
spplot(meuse,"zinc") 
# MB: Vediamo che i valori dello zinco riscontrati vanno aumentando verso il meandro (zona gialla) del fiume

# MB: funzione bubble del pacchetto sp. Permette di plottare allo stesso modo di prima ma usando dei pallini più o meno grandi a seconda delle concentrazioni
bubble(meuse,"zinc")
bubble(meuse,"copper",col="red")

# MB: inventiamo 2 oggetti attraverso la definizione di stringhe
foram <- c(10, 20, 35, 55, 67, 80)
carbon <- c(5, 15, 30, 70, 85, 99)
plot(foram, carbon, col="green", cex=2, pch=19) # MB: plottando possiamo notare la stretta correlazione tra le due variabili





#################################################################################################################
#################################################################################################################





### 3. R_code_point.patterns.r - codice per analisi dei POINT PATTERNS (strutture dei punti rilevati nello spazio - dati dall'esterno)


# install.packages("ggplot2")
# install.packages("spatstat")

library(ggplot2)

# MB: specifichiamo ad R la cartella di Working Directory (wd)
# MB: Attenzione a scrivere l'indirizzo con lo slash corrispondente al taso 7
setwd("C:/LAB")  

# MB: funzione per importare i dati del file, quindi visualizzare la tabella impostando la prima riga come contenente i nomi delle colonne 
covid <- read.table("covid_agg.csv",head=TRUE) 
head(covid) 
plot(covid$country,covid$cases) 
plot(covid$country,covid$cases,las=0) # MB: le etichette sono sempre scritte in modo parallelo agli assi x e y del grafico. 
plot(covid$country,covid$cases,las=1) # MB: le etichette dell'asse y diventano perpendicolari
plot(covid$country,covid$cases,las=2) # MB: lo stesso anche per le etichette dell'asse x
plot(covid$country,covid$cases,las=3) # MB: le etichette si mostrano sempre verticali
plot(covid$country,covid$cases,las=3,cex.axis=0.7) 
# MB: con cex.axis settiamo in modo diverso la grandezza delle etichette in modo da visualizzare tutti i nomi dei paesi
# MB: attach(covid) -> potremmo non utilizzare il simbolo $ per specificare l'attribuzione della singola colonna al dataset


data(mpg)
head(mpg)

ggplot(mpg, aes(x=displ,y=hwy)) + geom_point() 
# MB: funzione ggplot in cui specifichiamo i dati (mpg), l'estetica (le variabili da plottare sugli assi) e il tipo di geometria (punti)
ggplot(mpg, aes(x=displ,y=hwy)) + geom_line() # MB: cambiamo la geometira
ggplot(mpg, aes(x=displ,y=hwy)) + geom_polygon()

# ggplot di covid
ggplot(covid, aes(x=lon,y=lat,size=cases)) + geom_point() 
# MB: plottiamo i punti dei paesi ponendo le cordinate come x e y e la grandezza dei punti in relazione al numero dei casi riscontrati


##### DENSITY

# library(spatstat)

# MB: specifichiamo il database in cui sono presenti le variabili longitudine e latitudine
attach(covid) 

# MB: creiamo il point pattern "covids" per l'anilisi di densità 
covids <- ppp(lon, lat, c(-180,180), c(-90,90)) 
d <- density(covids) # MB: svolgiamo l'analisi di densità su covids
plot(d) # MB: plottiamo il grafico di densità
points(covids, pch=19) # MB: inseriamo i punti dei singoli paesi insieme alla mappa della densità


###### COASTLINES

# library(spatstat)
# library(rgdal)

setwd("C:/LAB")
load("point.patterns.RData") # MB: caricare il file RData precedentemente sviluppato

coastlines <- readOGR("ne_10m_coastline.shp") # MB: leggiamo i dati sulle coastlines messe all'interno della cartella LAB

# MB: plottiamo, insieme ai punti e alla densità (add=T), lo shapefile riguardante le coste a livello mondiale
plot(d)
points(covids,pch=19,cex=0.5)
plot(coastlines, col="yellow", add=T) 

# MB: identifichiamo una scala di colori definita con l'oggetto cl. (100) sta ad indicare il numero di gradazioni per singolo colore
cl <- colorRampPalette(c('yellow','orange','red'))(100) 
plot(d,col=cl) # MB: riplottiamo d con la scala di colori scelta
points(covids,pch=19,cex=0.5)
plot(coastlines, col="blue", add=T)

# Exercise 22/04/20 - riplottiamo i dati dell'ultima esercitazione

library(spatstat)
library(rgdal) 
setwd("C:/LAB")
load("point.pattern.RData")

ls()
coastlines <- readOGR("ne_10m_coastline.shp")
cl2 <- colorRampPalette(c('red', 'white', 'blue')) (200) 
plot(d, col=cl2, main="density")
points(covids)
plot(coastlines, add=T)


###### interpolation

# MB: "etichettiamo" il point pattern covids con i valori della colonna cases del dataset covid
marks(covids) <- covid$cases
i <- Smooth(covids) # i=interpolazione
# MB: smooth permette di stimare i valori dove questi non sono stati misurati creando una mappa di tipo continuo
plot(i)

plot(i, col=cl2, main="Interpolation: estimated number of cases")
points(covids)
plot(coastlines, add=T)


# MAPPA FINALE - Multipanel

par(mfrow=c(2,1))

# MB: plottiamo insieme densità e interpolazione
plot(d, col=cl2, main="density")
points(covids)
plot(coastlines, add=T)

plot(i, col=cl2, main="Interpolation: estimated number of cases")
points(covids)
plot(coastlines, add=T)
dev.off()


##### DATI DI SAN MARINO

load("Tesi.RData") # MB: carichiamo il dataset
ls() # MB: ispezioniamo gli oggetti contenuti nel data set
head(Tesi)
summary(Tesi) # MB: range di latitudine e longitudine scritti in forma decimale e non in gradi sessagesimali 

attach(Tesi)

# X varia da 12.42 a 12.46
# Y varia da 43.91 a 43.94
# pointpattern: x,y,c(xmin,xmax),c(ymin,ymax)
Tesippp <- ppp(Longitude, Latitude, c(12.41,12.47), c(43.9,43.95)) # MB: lasciamo dei margini negli intervalli

dT <- density(Tesippp)
plot(dT)
points(Tesippp)

marks(Tesippp) <- Species_richness # MB: associamo i valori del campo "ricchezza di specie" al pointpattern appena creato
interpol <- Smooth(Tesippp)
plot(interpol)
points(Tesippp)

# library(rgdal)

sanmarino <- readOGR("San_Marino.shp")
plot(sanmarino)
plot(interpol, add=T)
points(Tesippp)
plot(sanmarino, add=T) # MB: risovrapponiamo i confini di San Marino nel plot

par(mfrow=c(2,1))
plot(dT, main="Density of points")
points(Tesippp)
plot(interpol, main="Estimate of species richness")
points(Tesippp)
plot(sanmarino, add=T)





#################################################################################################################
#################################################################################################################






### 4. R_code_remote.sensing.r - CODICE R PER ANALISI DI IMMAGINI SATELLITARI

install.packages("raster")
library(raster)

setwd("C:/LAB")

p224r63_2011 <- brick("p224r63_2011_masked.grd") # MB: carichiamo, tramite la funzione brick, l'immagine dentro la wd
plot(p224r63_2011) # MB: visualizziamo le 7 immagini, una per ogni banda
# B1: blue
# B2: green
# B3: red
# B4: near infrared (nir)
# B5: medium infrared
# B6: thermal infrared
# B7: medium infrared

cl <- colorRampPalette(c('black','grey','light grey'))(100) 
plot(p224r63_2011,col=cl)

names(p224r63_2011) # MB: visualizzaizmo il nome delle bande contenute nel dataset

clb <- colorRampPalette(c('dark blue','blue','light blue'))(100) # MB: definiamo una tavola di colori per la banda del blu
plot(p224r63_2011$B1_sre,col=clb) 
# MB: per la sola banda del blu, mettiamo in evidenza le alte riflettanze (light blue) e le basse (dark blue)
# MB: per plottare soltanto la banda B1 potremmo utilizzare la funzione attach(p224r63_2011) 
# MB: ma purtroppo questa non funziona per il pacchetto raster -> $

clnir <- colorRampPalette(c('red','orange','yellow'))(100) # MB: facciamo lo stesso per la banda del vicino infrarosso
plot(p224r63_2011$B4_sre,col=clnir)

par(mfrow=c(2,2)) # MB: creiamo il multipanel in grado di visualizzare 4 immagini in 2 righe e 2 colonne
plot(p224r63_2011$B1_sre,col=clb)
clg <- colorRampPalette(c('dark green','green','light green'))(100)
plot(p224r63_2011$B2_sre,col=clg)
clr <- colorRampPalette(c('dark red','red','pink'))(100) # MB: il colore 'light red' non esiste
plot(p224r63_2011$B3_sre,col=clr)
clnir <- colorRampPalette(c('red','orange','yellow'))(100)
plot(p224r63_2011$B4_sre,col=clnir)

dev.off() # MB: per chiudere il device, ovvero la finestra del grafico

# MB: immagine satellitare come la vedrebbe l'occhio umano, cioè sovrapponendo le tre bande RGB
plotRGB(p224r63_2011,r=3,g=2,b=1) 

plotRGB(p224r63_2011,r=3,g=2,b=1, stretch="Lin") 
# MB: stretch: argomento a funzione per riuscire ad "allungare" la banda di colori se no la sovrapposizione fornisce un'immagine scura

# MB: per inserire nella composizione anche l'infrarosso vicino ci occorre escludere una banda tra le tre già usate.
# MB: Scaliamo quindi il numero delle bande sui rispettivi argomenti per inserire l'infrarosso (4) sulla componente red (r)
plotRGB(p224r63_2011,r=4,g=3,b=2, stretch="Lin") # MB: visualizziamo la vegetazione rossa e il suolo agricolo in celeste

pdf("ImageRemoteSensing.pdf") # MB: salvare l'immagine come pdf nella wd

par(mfrow=c(1,2)) 
plotRGB(p224r63_2011, r=3, g=2, b=1, stretch="Lin")
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin")
dev.off()

plotRGB(p224r63_2011, r=3, g=4, b=2, stretch="Lin") # MB: visualizziamo la vegetazione verde e il suolo viola 
plotRGB(p224r63_2011, r=3, g=2, b=4, stretch="Lin") # MB: visualizziamo la vegetazione blu e il suolo giallo

# MB: svolgiamo il medesimo esercizio sull'immagine del 1988
p224r63_1988 <- brick("p224r63_1988_masked.grd") 

par(mfrow=c(2,2)) 
plot(p224r63_1988$B1_sre,col=clb)
plot(p224r63_1988$B2_sre,col=clg)
plot(p224r63_1988$B3_sre,col=clr)
plot(p224r63_1988$B4_sre,col=clnir)
# MB: le color palette delle varie bande sono già state definite in precedenza
dev.off()

par(mfrow=c(2,1))
plotRGB(p224r63_1988,r=3,g=2,b=1, stretch="Lin")
plotRGB(p224r63_1988,r=4,g=3,b=2, stretch="Lin")
dev.off()

# MB: immagini dei due diversi anni messe a confronto, utilizzando la banda del NIR sulla componente "r"
par(mfrow=c(2,1))
plotRGB(p224r63_1988,r=4,g=3,b=2, stretch="Lin")
plotRGB(p224r63_2011,r=4,g=3,b=2, stretch="Lin")
dev.off()


#### spectral indices - DVI

# MB: creiamo la nuova immagine data dalla sottrazione, pixel per pixel, tra la banda dell'infrarosso e quella del rosso
dvi1988 <- p224r63_1988$B4_sre - p224r63_1988$B3_sre 
dvi2011 <- p224r63_2011$B4_sre - p224r63_2011$B3_sre # MB: lo stesso per l'anno 2011

# multitemporal analysis
difdvi <- dvi2011 - dvi1988 # MB: calcoliamo la differenza nell'indice per i due anni
cldifdvi <- colorRampPalette(c('red','white','blue'))(100)
plot(difdvi, col=cldifdvi)


par(mfrow=c(3,1)) # MB: creiamo il multipanel con le immagini ad infrarosso dei due anni e la differenza nell'indice DVI
plotRGB(p224r63_1988,r=4,g=3,b=2, stretch="Lin")
plotRGB(p224r63_2011,r=4,g=3,b=2, stretch="Lin")
plot(difdvi, col=cldifdvi)
dev.off()


### Changing the grain (Resolution)

p224r63_2011 # MB: possiamo visualizzare la natura del file e la sua risoluzione (o grana)

p224r63_2011lr <- aggregate(p224r63_2011, fact=10) 
# MB: lr = low resolution - ingrandimento del pixel di 10 volte
# MB: -> media tra i valori dei pixel originali contenuti nel nuovo pixel più grande

par(mfrow=c(2,1))
plotRGB(p224r63_2011,r=4,g=3,b=2, stretch="Lin")
plotRGB(p224r63_2011lr,r=4,g=3,b=2, stretch="Lin")

p224r63_2011lr50 <- aggregate(p224r63_2011, fact=50) # MB: ingrandimento di 50 volte

par(mfrow=c(3,1)) # MB: mettiamo a confronto le varie risoluzioni sull'immagine del 2011
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin")
plotRGB(p224r63_2011lr, r=4, g=3, b=2, stretch="Lin")
plotRGB(p224r63_2011lr50, r=4, g=3, b=2, stretch="Lin")

# MB: creiamo l'indice per il 2011 ma a bassa risoluzione
dvi2011lr50 <- p224r63_2011lr50$B4_sre - p224r63_2011lr50$B3_sre 
plot(dvi2011lr50)

p224r63_1988lr50 <- aggregate(p224r63_1988, fact=50) # MB: ingrandiamo 50 volte l'immagine del 1988
dvi1988lr50 <- p224r63_1988lr50$B4_sre - p224r63_1988lr50$B3_sre # MB: indice DVI del 1988 a lr

# MB: differenza tra gli indici dei due anni a bassa risoluzione
difdvilr50 <- dvi2011lr50 - dvi1988lr50
cldifdvi <- colorRampPalette(c('red','white','blue'))(100)
plot(difdvilr50, col=cldifdvi)
dev.off()

par(mfrow=c(2,1)) # MB: confrontiamo la variazione dell'indice nelle due risoluzioni
plot(difdvi, col=cldifdvi)
plot(difdvilr50, col=cldifdvi)





#################################################################################################################
#################################################################################################################






### 5. R_code_landcover.r 


library(raster)
library(RStoolbox)
library(ggplot2)

setwd("C:/LAB")

p224r63_2011 <- brick("p224r63_2011_masked.grd") # MB: brick -> carichiamo tutte le bande delle immagini 
plotRGB(p224r63_2011,r=4,g=3,b=2, stretch="Lin")

p224r63_2011c <- unsuperClass(p224r63_2011, nClasses=4) # MB: classifichiamo l'immagine, accorpando le bande dei pixel in diverse classi

# p224r63_2011c
# unsuperClass results

# *************** Map ******************
# $map
# class      : RasterLayer 
# dimensions : 478, 714, 341292  (nrow, ncol, ncell)
# resolution : 1, 1  (x, y)
# extent     : 0, 714, 0, 478  (xmin, xmax, ymin, ymax)
# crs        : NA 
# source     : memory
# names      : layer 
# values     : 1, 2  (min, max)


plot(p224r63_2011c$map)
clclass <- colorRampPalette(c('red', 'green', 'blue', 'black'))(100) 
plot(p224r63_2011c$map, col=clclass)
# MB: il processo di classificazione ha portato direttamente alla creazione della mappa clusterizzata
# MB: -> $map

p224r63_2011c <- unsuperClass(p224r63_2011, nClasses=2) # MB: cambiamo il numero delle classi
plot(p224r63_2011c$map)





#################################################################################################################
#################################################################################################################






### 6. R_code_multitemp.r  - analisi multitemporale di variazione della "land cover" - DEFORESTATION

defor1 <- brick("defor1.jpg") 
defor2 <- brick("defor2.jpg")
defor1 
# nella riga "names" compaiono i nome dei layer corrispondenti alle diverse bande
# names      : defor1.1, defor1.2, defor1.3
# defor1.1=NIR   defor1.2=red   defor1.3=green

par(mfrow=c(1,2)) # MB: confronto delle immagini ad infrarosso
plotRGB(defor1, r=1, g=2, b=3, stretch="Lin")
plotRGB(defor2, r=1, g=2, b=3, stretch="Lin")

d1c <- unsuperClass(defor1, nClasses=2) # MB: classificazione della prima mappa
plot(d1c$map)
# MB: gli argomenti a funzione sono solo il nome dell'immagine e il numero di classi finali

cld1c <- colorRampPalette(c('green', 'black'))(100)
plot(d1c$map, col=cld1c)

d2c <- unsuperClass(defor2, nClasses=2) # MB: classificazione della seconda mappa
plot(d2c$map, col=cld1c)

par(mfrow=c(2,1))
plot(d1c$map, col=cld1c)
plot(d2c$map, col=cld1c)


# MB: frequenza delle classi
freq(d1c$map)  
#      value  count
# [1,]     1 303540
# [2,]     2  37752
# classe 1 = foresta, classe 2 = aree aperte
# tot delle celle (pixel) di d1c = 303540 + 37752 = 341292

totd1c = 303540 + 37752

percent1 <- freq(d1c$map)*100/totd1c # MB: calcoliamo la percentuale di presenza delle classi
percent1
#             value   count
# [1,] 0.0002930042 88.9385
# [2,] 0.0005860085 11.0615
# percentuali: foresta=89%, aree aperte=11%

# MB: facciamo lo stesso per la seconda immagine
freq(d2c$map)
#      value  count
# [1,]     1 164667
# [2,]     2 178059
# classe 1 = aree aperte, classe 2 = foresta
# tot delle celle (pixel) di d2c = 164667 + 178059 = 342726

totd2c = 164667 + 178059

percent2 <- freq(d2c$map)*100/totd2c
percent2
#             value    count
# [1,] 0.0002917783 48.04625
# [2,] 0.0005835565 51.95375

# MB: per plottare i dati ottenuti creiamo un relativo dataset
cover <- c("Agriculture","Forest") # MB: creiamo l'oggetto "cover" assegnandoli i valori delle due classi
before <- c(11.1,88.9) # MB: creiamo la colonna "before" con i valori delle due classi nella prima mappa (defor1)
after <- c(48.0,52.0) # MB: colonna "after" con i valori delle classi nella seconda mappa (defor2)
output <- data.frame(cover,before,after) # MB: associamo i campi appena creati ad un dataframe (tabella) 
View(output) # MB: visualizziamo la tabella


# MB: plottiamo un istogramma delle percentuali di copertura di Foresta e Agricoltura sia prima che dopo l'attività di deforestazione
ggplot(output, aes(x=cover, y=before, color=cover)) + geom_bar(stat="identity", fill="white")
ggplot(output, aes(x=cover, y=after, color=cover)) + geom_bar(stat="identity", fill="white")

install.packages("gridExtra") 
library(gridExtra)
# MB: pacchetto necessario per plottare i due grafici insieme (stessa cosa della funzione par) in quanto ggplot non lo permette

# MB: assegnamo un nome ad entrambi i ggplot ed uno stesso limite percentuale di copertura (100%)
grafico1 <- ggplot(output, aes(x=cover, y=before, color=cover)) + geom_bar(stat="identity", fill="white") + ylim(0, 100)
grafico2 <- ggplot(output, aes(x=cover, y=after, color=cover)) + geom_bar(stat="identity", fill="white") + ylim(0, 100)
grid.arrange(grafico1, grafico2, nrow = 1) # MB: multipanel di gridExtra






#################################################################################################################
#################################################################################################################






### 7. R_code_multitemp_NO2.r - R CODE for analysing NO2 data from ESA - January to March 

setwd("C:/LAB")

library(raster)

# MB: utilizziamo la funzione raster per caricare il singolo layer (mono-banda, solo quello relativo a NO2) delle immagini satellitari 
# MB: (diversa da brick che invece permette di caricare tutte le bande dei vari sensori)
EN01 <- raster("EN_0001.png")
plot(EN01)


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

# MB: confrontiamo le situazioni di gennaio e marzo
par(mfrow=c(1,2))
plot(EN01, col=cl)
plot(EN13, col=cl)

# MB: svolgiamo una differenza tra marzo e gennaio, colorando in giallo le zone ad alta variazione
difno2 <- EN13-EN01
cldif <- colorRampPalette(c('blue','black','yellow'))(100)
plot(difno2, col=cldif)

# MB: essendo 13 immagini occorre una multipanel di 4x4 per plottarle tutte insieme
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
# library(raster)
setwd("C:/LAB/esa_no2")

# MB: nominiamo, con la funzione list.files, l'intero intervallo di file con estensione .png presenti all'interno della cartella "esa_no2"
rlist = list.files(pattern = ".png") 
# MB: utilizziamo la funzione "lapply", e più in particolare la funzione raster al suo interno, per caricare i file di rlist
listafinale = lapply(rlist, raster)
EN <- stack(listafinale) # MB: creiamo un pacchetto delle immagini in modo da poterle plottare tutte insieme
plot(EN, col=cl)

# MB: svolgiamo la differenza tra marzo e gennaio utilizzando lo stack
# MB: utilizziamo il $ per collegare i singoli file al nome dello stack
difEN <- EN$EN_0013 - EN$EN_0001 
plot(difEN, col=cldif)

#### funzione boxplot

# MB: verifichiamo quanto è variato l’azoto nel tempo
boxplot(EN) # MB: box in verticale
boxplot(EN, horizontal=T) # MB: box in orizzontale
boxplot(EN, horizontal=T, outline=F) # MB: eliminiamo anche i valori estremi ("outliner")
# MB: si potrebbe aggiungere l'argomento axes=F per eliminare gli assi. E' sottointeso come axes=T
# MB: la mediana non mostra una significativa variazione da EN_0001 a EN_0013
# MB: mentre è ben visibile la diminuzione dei massimi di concentrzione di NO2





#################################################################################################################
#################################################################################################################





### 8. R_code_snow.r - Snow cover analysis through Copernicus images

setwd("C:/LAB")
install.packages("ncdf4") # MB: pacchetto necessario per caricare immagini .nc

library(raster)
library(ncdf4)

# MB: carichiamo l'immagine - copertura nevosa europea del 18 maggio - tramite la funzione raster del medesimo pacchetto
snowmay <- raster("c_gls_SCE500_202005180000_CEURO_MODIS_V1.0.1.nc")
cl <- colorRampPalette(c('darkblue','blue','light blue'))(100)
plot(snowmay, col=cl)

# MB: carichiamo i dati di copertura nevosa degli anni 2000, 2005, 2010, 2015, 2020 presenti dentro la cartella snow
setwd("C:/LAB/snow")
# MB: importo tutte le immagini con lappy 
rlist=list.files(pattern=".tif", full.names=T) # MB: creo la lista dei file.tif situati dentro la wd
list_rast=lapply(rlist, raster)
snow.multitemp <- stack(list_rast)
plot(snow.multitemp, col=cl) # MB: la copertura nevosa del 2000, del 2005 e del 2010 è simulata

# MB: confrontiamo la prima e l'ultima immagine
par(mfrow=c(1,2))
plot(snow.multitemp$snow2000r,col=cl, zlim=c(0,250)) 
plot(snow.multitemp$snow2020r,col=cl, zlim=c(0,250))
# MB: con l'argomento a funzione "zlim" poniamo uguale il limite superiore della legenda in entrambe le immagini

# MB: differenza tra il 2020 e il 2000
difsnow = snow.multitemp$snow2020r - snow.multitemp$snow2000r
cldif <- colorRampPalette(c('blue','white','red'))(100) 
plot(difsnow, col=cldif)

### Prediction
# scaricare il file "prediction.r" da IOL nella cartella "snow". QUesto contiene la parte di codice per la previsione dei dati al 2025
source("prediction.r") # importare il codice dall'esterno 
# plot(predicted.snow.2025.norm, col=cl) 
# MB: il processo è troppo lento
predicted.snow.2025.norm <- raster("predicted.snow.2025.norm.tif") # carichiamo da IOL l'immagine già sviluppata  
plot(predicted.snow.2025.norm, col=cl)





#################################################################################################################
#################################################################################################################






### 9. R_code_patches.r

setwd("C:/LAB")

library(ggplot2)
library(raster)

# MB: utilizziamo le immagini già classificate nell'esercitazione sulla land cover
d1c <- raster("d1c.tif")
d2c <- raster("d2c.tif")

# MB: identifichiamo le due classi
cl <- colorRampPalette(c('black','green'))(100)
par(mfrow=c(1,2))
plot(d1c, col=cl)
plot(d2c, col=cl)
# classe 1 = agricoltura, classe 2 = foresta

# MB: riclassificamio le immagini eliminando la classe di agricoltura attraverso la funzione "cbind"
# MB: applichiamo alla classe 1 un valore nullo (NA = Not Assigned)
d1c.for <- reclassify(d1c, cbind(1, NA))

par(mfrow=c(1,2))
plot(d1c, col=cl)
plot(d1c.for, col=cl)

# MB: facciamo lo stesso per la seconda immagine
d2c.for <- reclassify(d2c, cbind(1, NA))

par(mfrow=c(1,2))
plot(d1c.for, col=cl)
plot(d2c.for, col=cl)

# MB: creiamo le patches raggruppando i pixel vicini - funzione clump
d1c.for.patches <- clump(d1c.for)
d2c.for.patches <- clump(d2c.for)

# MB: per creare i file appena riclassificati all'interno della wd
# writeRaster(d1c.for.patches, "d1c.for.patches.tif")
# writeRaster(d2c.for.patches, "d2c.for.patches.tif")

# MB: apllichiamo una cRP capace di mettre in evidenza la varie patches
clp <- colorRampPalette(c('dark blue','blue','green','orange','yellow','red'))(100)
par(mfrow=c(1,2))
plot(d1c.for.patches, col=clp)
plot(d2c.for.patches, col=clp)

# MB: definiamo quantitativamente il numero di patches presenti
d1c.for.patches
# values     : 1, 301  (min, max) 
# MB: il valore max corrisponde al numero delle patches
# max patches d1 = 301
d2c.for.patches
# max patches d2 = 1212

# MB: creiamo il dataframe 
time <- c("Before deforestation","After deforestation")
npatches <- c(301,1212)
output <- data.frame(time,npatches)
attach(output)

# MB: utilizziamo la funzione ggplot per graficare i risultati
ggplot(output, aes(x=time, y=npatches, color="red")) + geom_bar(stat="identity", fill="white")
 




#################################################################################################################
#################################################################################################################





### 10. R_code_crop.r

setwd("C:/LAB/snow")

library(raster)

# MB: carichiamo le immagini di copertura nevosa tramite lapply
rlist=list.files(pattern="snow2", full.names=T) 
list_rast=lapply(rlist, raster)
snow.multitemp <- stack(list_rast)

clb <- colorRampPalette(c('dark blue','blue','light blue'))(100)
plot(snow.multitemp,col=clb)


# ZOOM

snow.multitemp # MB: guardiamo il nome dei vari file dello stack
plot(snow.multitemp$snow2010r, col=clb) # MB: plottiamo solamente l'immagine del 2010

# MB: applichiamo uno zoom definendo matematicamente l'estensione
extension <- c(6, 18, 40, 50) # MB: definiamo l'intervallo (longitudine min e max, e latitudine min e max) chiamandolo "extension"
zoom(snow.multitemp$snow2010r, ext=extension) # MB: utilizziamo la funzione zoom di raster inserendo l'estensione scelta
extension <- c(6, 20, 35, 50) # MB: correggiamo gli intervalli
zoom(snow.multitemp$snow2010r, ext=extension) 

# MB: definiamo manualmente, con il mouse, sull'immagine, il rettangolo dell'estensione che ci interessa
# MB: occorre svolgere l'operazione su un plot nuovo
# dev.off
# plot(snow.multitemp$snow2010r, col=clb)
zoom(snow.multitemp$snow2010r, ext=drawExtent()) 
# MB: per definire il rettangolo partire da un vertice cliccando sulla mappa, spostarsi tenendo premuto, rilasciare, e cliccare nuovamente.


# CROP

# MB: possiamo svolgere la stessa azione di zoom ma usando la funzione "crop"
# MB: anzichè zoomare facciamo un ritaglio sull'area interessata
# extension <- c(6, 20, 35, 50)
snow2010r.italy <- crop(snow.multitemp$snow2010r, extension) 
# MB: notare che non viene scritto ext= ... ma si specifica solamente l'argomento "extension" scritto per esteso
plot(snow2010r.italy, col=clb)

# Crop the Italy extent on the whole stack of snow layers
snow.italy <- crop(snow.multitemp, extension)
plot(snow.italy, col=clb)

# MB: verifichiamo se l'intervallo di valori massimi e minimi indicati in legenda sono gli stessi per tutte le immagini
snow.italy 
plot(snow.multitemp.italy, col=clb, zlim=c(20,200)) # MB: settiamo lo stesso intervallo di legenda per tutte le immagini dello stack

# BOXPLOT

boxplot(snow.multitemp.italy, horizontal=T, outline=F)
# MB: notare la diminuzione dei valori massimi di copertura nevosa (contrazione delle barre) all'aumentare del tempo
 




#################################################################################################################
#################################################################################################################





### 11. Species Distribution Modelling

# install.packages("sdm")

library(sdm)
library(raster)
library(rgdal)

# MB: importiamo il file di sistema "species.shp" (shapefile) contenuto nel pacchetto sdm
# MB: essendo un file si sistema utilizziamo la funzione system.file
file <- system.file("external/species.shp", package="sdm") 
species <- shapefile(file) # MB: carichiamo la parte grafica del file con la funzione "shapefile"

species # MB: interroghiamo il dataset -> dataframe di punti spaziali

# class       : SpatialPointsDataFrame 
# features    : 200 
# extent      : 110112, 606053, 4013700, 4275600  (xmin, xmax, ymin, ymax)
# crs         : +proj=utm +zone=30 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0 
# variables   : 1
# names       : Occurrence 
# min values  :          0 
# max values  :          1 

species$Occurrence # MB: interroghiamo l'unica variabile del dataset
# MB: vediamo un elenco di 0 e 1, i quali dovrebbero corrispondere a valori di presenza/assenza all'interno del dataset
# MB: i numeri sono disposti secondo il sdr UTM, zona 30
plot(species)
# MB: vengono visualizzati sia i punti di campionamento di presenza della specie, sia quelli di assenza.

plot(species[species$Occurrence == 1,],col='blue',pch=16) # MB: identificahiamo solo le presenze, colorandole di blu
points(species[species$Occurrence == 0,],col='red',pch=16) # MB: coloriamo in rosso le assenze, aggiungendo i punti nel precendente plot

### MB: aggiungiamo delle variabili ambientali (PREDITTORI)

path <- system.file("external", package="sdm") # MB: carichiamo i file presenti nella cartella external nel pacchetto sdm
lst <- list.files(path=path,pattern='asc$',full.names = T) # MB: creiamo una lista dei file asci presenti in external ("path") 
lst # MB: controlliamo i livelli/layer presenti
#[1] "C:/Users/Marco Bergami/Documents/R/win-library/3.5/sdm/external/elevation.asc"
#[2] "C:/Users/Marco Bergami/Documents/R/win-library/3.5/sdm/external/precipitation.asc"
#[3] "C:/Users/Marco Bergami/Documents/R/win-library/3.5/sdm/external/temperature.asc"  
#[4] "C:/Users/Marco Bergami/Documents/R/win-library/3.5/sdm/external/vegetation.asc"  

preds <- stack(lst) # facciamo uno stack della lista dei "predittori", cioè i layer.asc che sono in grado di predire la presenza/assenza 
cl <- colorRampPalette(c('blue','orange','red','yellow')) (100)
plot(preds, col=cl)

plot(preds$elevation, col=cl) # plottiamo solamente l'elevazione
points(species[species$Occurrence == 1,], pch=16)
# MB: deduciamo che la specie è maggiormente presente con basse altitudini

plot(preds$temperature, col=cl)
points(species[species$Occurrence == 1,], pch=16)
# MB: deduciamo che la specie è maggiormente presente con alte temperature

plot(preds$precipitation, col=cl)
points(species[species$Occurrence == 1,], pch=16)
# MB: deduciamo che la specie è maggiormente presente con precipitazioni intermedie

plot(preds$vegetation, col=cl)
points(species[species$Occurrence == 1,], pch=16)
# MB: deduciamo che la specie è maggiormente presente con copertura vegetazionele abbastanza forte

### Generalized Linear Model

# MB: creiamo un oggetto che contenga i dati del modello, cioè i "train" (insieme dei punti di test/campionamento) e i predittori
d <- sdmData(train=species, predictors=preds)

# MB: specifichiamo il modello
# MB: modello lineare con y = occurence e x,z,... le altre varibili ambientali
m1 <- sdm(Occurrence ~ elevation + precipitation + temperature + vegetation, data=d, methods='glm') 

p1 <- predict(m1, newdata=preds) # MB: svolgiamo la previsione, indicando il modello e l'insieme dei predittori
plot(p1, col=cl)
points(species[species$Occurrence == 1,], pch=16)
 




#################################################################################################################
#################################################################################################################





### 12. Exam project


setwd("C:/LAB/SWI")

library(raster)
library(ncdf4)
library(RStoolbox)
library(gridExtra)

rlist = list.files(pattern = ".nc") # chiamiamo rlist l'intero intervallo di file con estensione .nc presenti all'interno della cartella "SWI"
# utilizziamo la funzione "lapply", e più in particolare la funzione raster al suo interno, per caricare i file di rlist
listafinale = lapply(rlist, raster)
swi <- stack(listafinale)

cl <- colorRampPalette(c('white','blue'))(100) # alti valori = blu, bassi valori = bianco
plot(swi, col=cl) 
# non si notano grandi variazioni ad occhio, molto territorio compare bianco

# se limitiamo la legenda (0,6) è meglio possibile vedere le variazioni dell'indice nelle parti precedentemente colorate di bianco
# le zone blu variano invece intorno ai valori più alti della legenda (~ 250), ma non subiscono forti variazioni durante l'anno
plot(swi, col=cl, zlim=c(0,6))

# confronto tra le gli estremi dell'intervallo temporale 15/05/19 - 15/05/2020

par(mfrow=c(1,2))
plot(swi$Surface.State.Flag.1,col=cl, zlim=c(0,6), main="15/05/2019")
plot(swi$Surface.State.Flag.13,col=cl, zlim=c(0,6), main="15/05/2020")
# situazione simile -> ciclicità annuale

# confronto tra agosto (15/08/2019 - Surface.State.Flag.4 ) e gennaio (15/01/2020 - Surface.State.Flag.9)

par(mfrow=c(1,2))
plot(swi$Surface.State.Flag.4,col=cl, zlim=c(0,6), main="15/05/2019")
plot(swi$Surface.State.Flag.9,col=cl, zlim=c(0,6), main="15/01/2020")

# differenza tra agosto (15/08/2019 - Surface.State.Flag.4 ) e gennaio (15/01/2020 - Surface.State.Flag.9)

cldif <- colorRampPalette(c('red','white','blue'))(100)
difswi <- swi$Surface.State.Flag.9 - swi$Surface.State.Flag.4
plot(difswi, col=cldif)

# classificazione

swi.15.08 <- unsuperClass(swi$Surface.State.Flag.4, nClasses=4) # classificazione del 15/08/2019
plot(swi.15.08$map, col=cldif)

swi.15.01 <- unsuperClass(swi$Surface.State.Flag.9, nClasses=4) # classificazione del 15/01/2020
plot(swi.15.01$map, col=cldif)

clc <- colorRampPalette(c('red','green','light blue','blue'))(100)

par(mfrow=c(2,2))
plot(swi.15.08$map, col=clc)
plot(swi$Surface.State.Flag.4,col=cl, zlim=c(0,6), main="15/08/2020")
plot(swi.15.01$map, col=clc)
plot(swi$Surface.State.Flag.9,col=cl, zlim=c(0,6), main="15/01/2020")

# i mari e gli oceani vengono sempre rappresentati dalla classe 2 (verde)

## 15/18
# classe 1: rosso = valori bassi
# classe 2: verde = mari/oceani
# classe 3: azzurro = valori medi
# classe 4: blu = valori alti

## 15/01
# classe 1: rosso = valori bassi
# classe 2: verde = mari/oceani
# classe 3: azzurro = valori medi
# classe 4: blu = valori alti

# riclassifichiamo per eliminare la classe 2 (verde) dei mari/oceani

swi.15.08r <- reclassify(swi.15.08$map, cbind(2, NA))
swi.15.01r <- reclassify(swi.15.01$map, cbind(2, NA))

par(mfrow=c(1,2))
plot(swi.15.08r, col=clc)
plot(swi.15.01r, col=clc)

# frequenza delle classi

freq(swi.15.08r)

#     value    count
#[1,]     1  1361264 -> B
#[2,]     3 12149265 -> M
#[3,]     4     4203 -> A
#[4,]    NA 14797076

# tot di celle = 1361264 + 12149265 + 4203 = 13.514.732
totswi.15.08r = 1361264 + 12149265 + 4203

percentswi.15.08r <- freq(swi.15.08r)*100/totswi.15.08r # percentuale

percentswi.15.08r
#            value       count
#[1,] 7.399333e-06  10.0724454 -> 10.07 %
#[2,] 2.219800e-05  89.8964552 -> 89.90 %
#[3,] 2.959733e-05   0.0310994 -> 0.03 %
#[4,]           NA 109.4884900


freq(swi.15.01r)

#     value    count
#[1,]     1  5735788 -> B
#[2,]     3  6417680 -> M
#[3,]     4  1361264 -> A
#[4,]    NA 14797076

# tot di celle = 5735788 + 6417680 + 1361264 = 13.514.732‬
totswi.15.01r = 5735788 + 6417680 + 1361264

percentswi.15.01r <- freq(swi.15.01r)*100/totswi.15.01r # percentuale

#            value     count
#[1,] 7.399333e-06  42.44100 -> 42.44 %
#[2,] 2.219800e-05  47.48655 -> 47.49 %
#[3,] 2.959733e-05  10.07245 -> 10.07 %
#[4,]           NA 109.48849

# per plottare i dati ottenuti creiamo un relativo dataframe/tabella
LevelSWI <- c("Low","Medium","High") # creo il campo relativo al livello dell'indice
agosto19 <- c(10.07,89.90,0.03) # campo con i valori delle tre classi nella prima mappa (swi.15.08r)
gennaio20 <- c(42.44,47.49,10.07) # campo con i valori delle classi nella seconda mappa (swi.15.01r)
output <- data.frame(LevelSWI,agosto19,gennaio20) # associamo i campi appena creati ad un dataframe (tabella) 
View(output) # visualizziamo la tabella

# multipanel istogramma finale
ist.agosto <- ggplot(output, aes(x=LevelSWI, y=agosto19, color=LevelSWI)) + geom_bar(stat="identity", fill="white") + ylim(0, 100)
ist.gennaio <- ggplot(output, aes(x=LevelSWI, y=gennaio20, color=LevelSWI)) + geom_bar(stat="identity", fill="white") + ylim(0, 100)
grid.arrange(ist.agosto, ist.gennaio, nrow = 1) 









