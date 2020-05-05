#### R CODE - analisi multitemporale di variazione della land cover

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
     value  count
[1,]     1 164667
[2,]     2 178059
# classe 1 = aree aperte, classe 2 = foresta
# tot delle celle (pixel) di d2c = 164667 + 178059 = 342726
totd2c = 164667 + 178059 = 342726
percent2 <- freq(d2c$map)*100/totd2c
            value    count
[1,] 0.0002917783 48.04625
[2,] 0.0005835565 51.95375

cover <- c("Agriculture","Forest")
before <- c(11.1,88.9)
after <- c(48.0,52.0)
output <- data.frame(cover,before,after)
View(output)

library(ggplot2)
# plottiamo un istogramma delle percentuali di copertura di Foresta e Agricoltura sia prima che dopo l'attività di deforestazione
ggplot(output, aes(x=cover, y=before, color=cover)) + geom_bar(stat="identity", fill="white")
ggplot(output, aes(x=cover, y=after, color=cover)) + geom_bar(stat="identity", fill="white")

install.packages("gridExtra") # pacchetto necessario per plottare i due grafici insieme (stessa cosa della funzione par) in quanto ggplot non lo permette
library(gridExtra)
# assegnamo un nome ad entrambi i ggplot
grafico1 <- ggplot(output, aes(x=cover, y=before, color=cover)) + geom_bar(stat="identity", fill="white")
grafico2 <- ggplot(output, aes(x=cover, y=after, color=cover)) + geom_bar(stat="identity", fill="white")
grid.arrange(grafico1, grafico2, nrow = 1) 
# i valori sull'asse y rimangono diversi per i due grafici, bisognerebbe settarli entrambi tra 0 e 100 %

















