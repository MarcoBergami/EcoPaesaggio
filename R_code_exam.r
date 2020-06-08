#########    R_code_exam.r    ##########

# 1. R_code_first.r   
# 2. R_code_spatial.r   
# 3. R_code_spatial2.r
# 4. R_code_point_pattern   
# 5. R_code_teleril.r   
# 6. R_code_landcover.r   
# 7. R_code_multitemp.r   
# 8. R_code_multitemp_NO2.r   
# 9. R_code_snow.r   
# 10. R_code_patches.r   
# 11. R_code_crop.r
# 12. Species Distribution Modelling


### R_code_primocod.r - PRIMO CODICE ECOLOGIA DEL PAESAGGIO
install.packages("sp")

library(sp) # richiamiamo la libreria sp prima di .....
data(meuse)
meuse
pairs(meuse)
pairs(~ cadmium + copper + lead , data = meuse)
pairs(~ cadmium + copper + lead + zinc , data = meuse)
pairs(meuse[,3:6], col="red") # cambiamo colore
pairs(meuse[,3:6], col="red", pch=19, cex=3) # cambiamo "point character" (simbolo grafico) e "character exageration" (grandezza simbolo)
pairs(meuse[,3:6], col="red", pch=19, cex=3, main="Primo pairs") # inseriamo un titolo al grafico

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
#parte di codice che permette di mostrare le tre fuzioni inserite nei grafici, suddivedendole in pannello superiore, inferiore e diagonale.

#funzione plot
plot(meuse$cadmium , meuse$copper)
attach(meuse)
plot(cadmium,copper)
plot(cadmium,copper, pch=17, col="green", main="primo plot")
plot(cadmium,copper, pch=17, col="green", main="primo plot", xlab="cadmio", ylab="rame") # cambiamo in nome delle etichette sugli assi del grafico
plot(cadmium,copper, pch=17, col="green", main="primo plot", xlab="cadmio", ylab="rame", cex.lab=2, cex=2) # cambiamo grandezza delle etichette e dei simboli




#################################################################################################################
#################################################################################################################





### R_code_spatial.r - funzioni spaziali
library(sp)
data(meuse) # richiamare il dataset
head(meuse) # diversamente, utilizzando names(meuse) si visualizzano solo i nomi delle variabili, non più le prime righe di tabella
# incominciamo richiamando la funzione plot tra cadmio e piombo
attach(meuse)     #alleghiamo il dataframe
plot(cadmium,lead,col="red",pch=19,cex=2)
plot(copper,zinc,col="green",pch=17,cex=2)  #plot di rame e zinco usando come simbolo il triangolo e colore verde
plot(copper,zinc,col="green",pch=17,cex=2,xlab="rame", ylab="zinco")

# funzione multiframe o multipanel, necessaria per mostrare più grafici insieme, si chiama "par", in questo caso composto da 1 riga e 2 colonne.
par(mfrow=c(1,2))
plot(cadmium,lead,col="red",pch=19,cex=2)
plot(copper,zinc,col="green",pch=17,cex=2)
# ovviamente per utilizzare un multipanel con 2 righe e 1 colonna basta cambiare i numeri dentro la parentsi di c.

install.packages("GGally")
library(GGally)
#richiamiamo la funzione pairs in GGally specificando un sottoinsieme delle variabili
ggpairs(meuse[,3:6])
# sull'asse diagonale sono presenti le singole variabili e la distribuzione di frequenza dei dati, nel pannello superiore ci sono i valori del coeff. di correlazione.

# per svolgere delle analisi spaziali occorre per prima cosa far capire ad R che il dataframe contiene anche dei valori di coordinate, x e y. per farlo usiamo una funzione "coordinates" contenuta nel pacchetto sp.
coordinates(meuse)=~x+y # la tilde è necessaria per raggruppare più colonne del dataset
plot(meuse)
spplot(meuse,"zinc") #funzione spplot per plottare i dati distribuiti nello spazio, in questo caso rispetto alla variabile zinco, indicata tra "". Vediamo che i valori dello zinco riscontrati vanno aumentando verso il meandro (zona gialla) del fiume
# funzione bubble del pacchetto sp. Permette di plottare allo stesso modo di prima ma usando dei pallini più o meno grandi a seconda delle concentrazioni
bubble(meuse,"zinc")
bubble(meuse,"copper",col="red")

# inventiamo 2 oggetti attraverso la definizione di stringhe
foram <- c(10, 20, 35, 55, 67, 80)
carbon <- c(5, 15, 30, 70, 85, 99)
plot(foram, carbon, col="green", cex=2, pch=19) # plottando possiamo notare la stretta correlazione tra le due variabili





#################################################################################################################
#################################################################################################################





### R_code_point.patterns.r - codice per analisi dei POINT PATTERNS (strutture dei punti rilevati nello spazio - dati dall'esterno)

install.packages("ggplot2")
install.packages("spatstat")

setwd("C:/LAB") # specifichiamo ad R la cartella di Working Directory (wd). Attenzione a scrivere l'indirizzo con lo slash corrispondente al taso 7
covid <- read.table("covid_agg.csv",head=TRUE) # funzione per importare i dati del file, quindi visualizzare la tabella impostando la prima riga come contenente i nomi delle colonne 
head(covid) 
plot(covid$country,covid$cases) # se facessimo prima attach(covid) potremmo non utilizzare il simbolo $ per specificare l'attribuzione della singola colonna al dataset: plot(country,cases)
plot(covid$country,covid$cases,las=0) # le etichette sno sempre scritte in modo parallelo agli assi x e y del grafico. Proviamo quindi con valori diversi da 0..
plot(covid$country,covid$cases,las=1) # le etichette dell'asse y diventano perpendicolari
plot(covid$country,covid$cases,las=2) # lo stesso anche per le etichette dell'asse x
plot(covid$country,covid$cases,las=3) # le etichette si mostrano sempre verticali
plot(covid$country,covid$cases,las=3,cex.axis=0.7) # con cex.axis settiamo in modo diverso la grandezza delle etichette in modo da visualizzare tutti i nomi dei paesi

library(ggplot2) # richiamiamo il pacchetto
data(mpg)
head(mpg)
ggplot(mpg, aes(x=displ,y=hwy)) + geom_point() # funzione ggplot in cui specifichiamo i dati (mpg), l'estetica, ovvero le variabili da plottare sugli assi, e il tipo di geometria a punti
ggplot(mpg, aes(x=displ,y=hwy)) + geom_line() # cambiamo la geometira
ggplot(mpg, aes(x=displ,y=hwy)) + geom_polygon()

# ggplot di covid
ggplot(covid, aes(x=lon,y=lat,size=cases)) + geom_point() # plottiamo i punti dei paesi ponendo le cordinate come x e y e la grandezza dei punti in relazione al numero dei casi riscontrati

##### DENSITY
library(spatstat)
attach(covid) # specifichiamo il database in cui sono presenti le variabili longitudine e latitudine
covids <- ppp(lon, lat, c(-180,180), c(-90,90)) # creiamo l'oggetto covidS che, a differenza dell'altro, utilizziamo per l'analisi di densità
d <- density(covids) # chiamiamo "d" il risultato dell'analisi di densità su covids
plot(d) # plottiamo il grafico di densità
points(covids, pch=19) # inseriamo i punti dei singoli paesi insieme alla mappa della densità

###### COASTLINES
library(spatstat)
library(rgdal)

setwd("C:/LAB")
load("point.patterns.RData") # se si vuole caricare il file RData precedentemente sviluppato

coastlines <- readOGR("ne_10m_coastline.shp") #leggiamo i dati sulle coastlines messe all'interno della cartella LAB

plot(d)
points(covids,pch=19,cex=0.5)
plot(coastlines, col="yellow", add=T) # plottiamo, insieme ai punti e alla densità (add=T), lo shapefile riguardante le coste a livello mondiale

cl <- colorRampPalette(c('yellow','orange','red'))(100) # identifichiamo una scala di colori definita con l'oggetto cl. (100) sta ad indicare il numero di gradazioni per singolo colore
plot(d,col=cl) # riplottiamo d con la scala di colori scelta
points(covids,pch=19,cex=0.5)
plot(coastlines, col="blue", add=T)

# Exercise 22/04/20
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
marks(covids) <- covid$cases
i <- Smooth(covids) # i=interpolazione, smooth permette di stimare i valori dove questi non sono stati misurati creando una mappa di tipo continuo
plot(i)

plot(i, col=cl2, main="Interpolation: estimated number of cases")
points(covids)
plot(coastlines, add=T)

# MAPPA FINALE - Multipanel
par(mfrow=c(2,1))

plot(d, col=cl2, main="density")
points(covids)
plot(coastlines, add=T)

plot(i, col=cl2, main="Interpolation: estimated number of cases")
points(covids)
plot(coastlines, add=T)
dev.off()

##### DATI DI SAN MARINO
load("Tesi.RData") # carichiamo il dataset
ls() # ispezioniamo la tabella del data set
head(Tesi)
summary(Tesi) # visualizziamo i range di latitudine e longitudine scritti in forma decimale e non in gradi sessagesimali 
attach(Tesi)

# X varia da 12.42 a 12.46
# Y varia da 43.91 a 43.94
# pointpattern: x,y,c(xmin,xmax),c(ymin,ymax)
Tesippp <- ppp(Longitude, Latitude, c(12.41,12.47), c(43.9,43.95)) # lasciamo dei margini negli intervalli

dT <- density(Tesippp)
plot(dT)
points(Tesippp)

marks(Tesippp) <- Species_richness # associamo i valori del campo "ricchezza di specie" al pointpattern appena creato
interpol <- Smooth(Tesippp)
plot(interpol)
points(Tesippp)

library(rgdal)
sanmarino <- readOGR("San_Marino.shp")
plot(sanmarino)
plot(interpol, add=T)
points(Tesippp)
plot(sanmarino, add=T) # risovrapponiamo i confini di SanMarino nel plot

par(mfrow=c(2,1))
plot(dT, main="Density of points")
points(Tesippp)
plot(interpol, main="Estimate of species richness")
points(Tesippp)
plot(sanmarino, add=T)





#################################################################################################################
#################################################################################################################






### R_code_remote.sensing.r - CODICE R PER ANALISI DI IMMAGINI SATELLITARI

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





#################################################################################################################
#################################################################################################################






### R_code_multitemp.r  - analisi multitemporale di variazione della "land cover"

setwd("C:/LAB")
library(raster)

defor1 <- brick("defor1.jpg") # carichiamo tutte le bande delle immagini 
defor2 <- brick("defor2.jpg")
defor1 
# nella riga "names" compaiono i nome dei layer corrispondenti alle diverse bande
# names      : defor1.1, defor1.2, defor1.3
# defor1.1=NIR   defor1.2=red   defor1.3=green

par(mfrow=c(1,2))
plotRGB(defor1, r=1, g=2, b=3, stretch="Lin")
plotRGB(defor2, r=1, g=2, b=3, stretch="Lin")

library(RStoolbox)
d1c <- unsuperClass(defor1, nClasses=2) # classificazione della prima mappa
plot(d1c$map)
cld1c <- colorRampPalette(c('green', 'black'))(100)
plot(d1c$map, col=cld1c)

d2c <- unsuperClass(defor2, nClasses=2) # classificazione della seconda mappa
plot(d2c$map, col=cld1c)

par(mfrow=c(2,1))
plot(d1c$map, col=cld1c)
plot(d2c$map, col=cld1c)

freq(d1c$map) # frequenza delle classi 
#      value  count
# [1,]     1 303540
# [2,]     2  37752
# classe 1 = foresta, classe 2 = aree aperte
# tot delle celle (pixel) di d1c = 303540 + 37752 = 341292
totd1c = 303540 + 37752
percent1 <- freq(d1c$map)*100/totd1c
percent1
#             value   count
# [1,] 0.0002930042 88.9385
# [2,] 0.0005860085 11.0615
# percentuali: foresta=89%, aree aperte=11%

# facciamo lo stesso per la seconda immagine
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

# per plottare i dati ottenuti creiamo un relativo dataset
cover <- c("Agriculture","Forest") # creiamo la colonna "cover" assegnandoli i due valori delle due classi
before <- c(11.1,88.9) # creiamo la seconda colonna "before" con i valori delle due classi nella prima mappa (defor1)
after <- c(48.0,52.0) # colonna "after" con i valori delle classi nella seconda mappa (defor2)
output <- data.frame(cover,before,after) # associamo i campi appena creati ad un dataframe (tabella) 
View(output) # visualizziamo la tabella

library(ggplot2)
# plottiamo un istogramma delle percentuali di copertura di Foresta e Agricoltura sia prima che dopo l'attività di deforestazione
ggplot(output, aes(x=cover, y=before, color=cover)) + geom_bar(stat="identity", fill="white")
ggplot(output, aes(x=cover, y=after, color=cover)) + geom_bar(stat="identity", fill="white")

install.packages("gridExtra") # pacchetto necessario per plottare i due grafici insieme (stessa cosa della funzione par) in quanto ggplot non lo permette
library(gridExtra)
# assegnamo un nome ad entrambi i ggplot
grafico1 <- ggplot(output, aes(x=cover, y=before, color=cover)) + geom_bar(stat="identity", fill="white") + ylim(0, 100)
grafico2 <- ggplot(output, aes(x=cover, y=after, color=cover)) + geom_bar(stat="identity", fill="white") + ylim(0, 100)
grid.arrange(grafico1, grafico2, nrow = 1) 






#################################################################################################################
#################################################################################################################






### R_code_multitemp_NO2.r - R CODE for analysing NO2 data from ESA - January to March 

setwd("C:/LAB")

library(raster)
EN01 <- raster("EN_0001.png")
plot(EN01)
# utilizziamo la funzione raster per caricare il singolo layer (mono-banda, solo quello relativo a NO2) delle immagini satellitari 
# (diversa da brick che invece permette di caricare tutte le bande dei vari sensori)
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
setwd("C:/LAB/esa_no2")

rlist = list.files(pattern = ".png") # chiamiamo rlist l'intero intervallo di file con estensione .png presenti all'interno della cartella "esa_no2"
# utilizziamo la funzione "lapply", e più in particolare la funzione raster al suo interno, per caricare i file di rlist
listafinale = lapply(rlist, raster)
EN <- stack(listafinale) # creiamo un pacchetto delle immagini in modo da poterle plottare
plot(EN, col=cl)

difEN <- EN$EN_0013 - EN$EN_0001 

#### funzione boxplot
boxplot(EN) # barre in verticale
boxplot(EN, horizontal=T) # barre in orizzontale
boxplot(EN, horizontal=T,outline=F) # si potrebbe aggiungere l'argomento axes=F per eliminare gli assi. E' sottointeso come axes=T
# la mediana non mostra una significativa variazione da EN_0001 a EN_0013, mentre è ben visibile la diminuzione dei massimi di concentrzione di NO2





#################################################################################################################
#################################################################################################################





### R_code_snow.r - Snow cover analysis through Copernicus images

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





#################################################################################################################
#################################################################################################################






### R_code_patches.r

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
 




#################################################################################################################
#################################################################################################################




### R_code_crop.r

setwd("C:/LAB/snow")
library(raster)

rlist=list.files(pattern="snow2", full.names=T) # creo la lista di file.tif situati dentro la wd
list_rast=lapply(rlist, raster)
snow.multitemp <- stack(list_rast)

clb <- colorRampPalette(c('dark blue','blue','light blue'))(100)
plot(snow.multitemp,col=clb)

# ZOOM

snow.multitemp # guardiamo il nome corretto di un file
plot(snow.multitemp$snow2010r, col=clb)

extension <- c(6, 18, 40, 50) # definiamo l'intervallo di longitudine e latitudine chiamandolo "estensione"
zoom(snow.multitemp$snow2010r, ext=extension) # utilizziamo la funzione zoom di raster inserendo l'estensione scelta
extension <- c(6, 20, 35, 50) # correggiamo gli intervalli
zoom(snow.multitemp$snow2010r, ext=extension) 

zoom(snow.multitemp$snow2010r, ext=drawExtent()) # definiamo manualmente, con il mouse, sull'immagine, il rettangolo di estensione che ci interessa
# definire il rettangolo: 

# CROP

# extension <- c(6, 20, 35, 50)
snow2010r.italy <- crop(snow.multitemp$snow2010r, extension) # anzichè zoomare facciamo un ritaglio sull'area interessata
# notare che non viene scritto ext= ...
plot(snow2010r.italy, col=clb)

# Crop the Italy extent on the whole stack of snow layers
snow.italy <- crop(snow.multitemp, extension)
plot(snow.italy, col=clb)

snow.italy # verifichiamo l'intervallo di valori massimi e minimi indicati in legenda
plot(snow.multitemp.italy, col=clb, zlim=c(20,200)) # settiamo lo stesso intervallo di legenda per tutte le immagini dello stack

# BOXPLOT

boxplot(snow.multitemp.italy, horizontal=T,outline=F)
# notare la diminuzione dei valori massimi di copertura nevosa (contrazione delle barre) all'aumentare del tempo
 




#################################################################################################################
#################################################################################################################




### Species Distribution Modelling

# install.packages("sdm")
library(sdm)
library(raster)
library(rgdal)

file <- system.file("external/species.shp", package="sdm") # carichiamo il file attraverso la funzione system.file
species <- shapefile(file) 
species # guardiamo di cosa si tratta il dataset
species$Occurrence # interroghiamo l'unica variabile del dataset
# vediamo un elenco di 0 e 1, i quali dovrebbero corrispondere a valori di presenza/assenza all'interno del dataset, disposti secondo il sdr UTM, zona 30
plot(species)
# vengono visualizzati sia i punti di campionamento di presenza della specie, sia quelli di assenza.

plot(species[species$Occurrence == 1,],col='blue',pch=16) # identificahiamo solo le presenze, colorandole di blu
points(species[species$Occurrence == 0,],col='red',pch=16) # coloriamo in rosso le assenze, aggiungendo i punti nel precendente plot

# aggiungiamo delle variabili ambientali 
path <- system.file("external", package="sdm") # carichiamo i file presenti nella cartella external nel pacchetto sdm
lst <- list.files(path=path,pattern='asc$',full.names = T) # creiamo una lista dei file asci presenti dentro path 
lst # controlliamo i livelli/layer presenti
#[1] "C:/Users/Marco Bergami/Documents/R/win-library/3.5/sdm/external/elevation.asc"
#[2] "C:/Users/Marco Bergami/Documents/R/win-library/3.5/sdm/external/precipitation.asc"
#[3] "C:/Users/Marco Bergami/Documents/R/win-library/3.5/sdm/external/temperature.asc"  
#[4] "C:/Users/Marco Bergami/Documents/R/win-library/3.5/sdm/external/vegetation.asc"  

preds <- stack(lst) # facciamo uno stack della lista dei "predittori", cioè i layer.asc che sono in grado di predire la presenza/assenza 
cl <- colorRampPalette(c('blue','orange','red','yellow')) (100)
plot(preds, col=cl)

plot(preds$elevation, col=cl) # plottiamo solamente l'elevazione
points(species[species$Occurrence == 1,], pch=16)
# deduciamo che la specie è maggiormente presente con basse altitudini

plot(preds$temperature, col=cl)
points(species[species$Occurrence == 1,], pch=16)
# deduciamo che la specie è maggiormente presente con alte temperature

plot(preds$precipitation, col=cl)
points(species[species$Occurrence == 1,], pch=16)
# deduciamo che la specie è maggiormente presente con precipitazioni intermedie

plot(preds$vegetation, col=cl)
points(species[species$Occurrence == 1,], pch=16)
# deduciamo che la specie è maggiormente presente con copertura vegetazionele abbastanza forte

# Generalized Linear Model

d <- sdmData(train=species, predictors=preds)
# train = insieme dei punti di campionamento
m1 <- sdm(Occurrence ~ elevation + precipitation + temperature + vegetation, data=d, methods='glm') # specifichiamo il modello
p1 <- predict(m1, newdata=preds) # specifichiamo la previsione, indicando il modello e l'insieme dei predittori
plot(p1, col=cl)
points(species[species$Occurrence == 1,], pch=16)
 




#################################################################################################################
#################################################################################################################









