# funzioni spaziali - 24.03
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

