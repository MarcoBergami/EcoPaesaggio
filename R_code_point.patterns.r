#### codice per analisi dei POINT PATTERNS (strutture dei punti rilevati nello spazio - dati dall'esterno)

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
load("Tesi.RData")
ls()
head(Tesi)
attach(Tesi)

# X varia da 12.42 a 12.46
# y varia da 43.91 a 43.94
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







