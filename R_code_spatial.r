# funzioni spaziali - 24.03
library(sp)
data(meuse)
head(meuse)
# incominciamo richiamando la funzione plot tra cadmio e piombo
attach(meuse)     #alleghiamo il dataframe
plot(cadmium,lead,col="red",pch=19,cex=2)
plot(copper,zinc,col="green",pch=17,cex=2)  #plot di rame e zinco usando come simbolo il triangolo e colore verde
plot(copper,zinc,col="green",pch=17,cex=2,xlab="rame", ylab="zinco")
# funzione multiframe o multipanel, cioè per mostrare più grafici insieme, si chiama "par", in questo caso composto da 1 riga e 2 colonne.
par(mfrow=c(1,2))
plot(cadmium,lead,col="red",pch=19,cex=2)
plot(copper,zinc,col="green",pch=17,cex=2)
# ovviamente per utilizzare un multipanel con 2 righe e 1 colonna basta cambiare i numeri dentro la parentsi di c.
install.packages("GGally")
library(GGally)
#richiamiamo la funzione pairs in GGally, ma facendo un sottoinsieme delle analisi che vengono svolte
ggpairs(meuse[,3:6])
# sull'asse diagonale sono presenti le singole variabili e la distribuzione di frequenza dei dati, nel pannello superiore ci sono i valori del coeff. di correlazione.
# la correlazione risulta essere positiva in tutti i casi e anche abbastanza alta, vicino ad 1 tranne nel caso del cadmio. 
# per svolgere delle analisi spaziali occorre per prima cosa far capire ad R che il dataframe contiene anche dei valori di coordinate, x e y. per farlo usiamo una funzione "coordinates" contenuta nel pacchetto sp.
coordinates(meuse)=~x+y
plot(meuse)
spplot(meuse,"zinc") #funzione spplot per plottare i dati distribuiti nello spazio. vediamo che i valori dello zinco riscontrati vanno aumentando verso il meandro (zona gialla) del fiume
