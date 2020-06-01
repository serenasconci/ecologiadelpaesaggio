### R spatial:funzioni spaziali in Ecologia del paesaggio

library(sp)
#RICHIAMO IL PACCHETTO
 
data(meuse)
#PER RICHIAMARE I DATI

head(meuse)
#FA VEDERE LE PRIME SEI RIGHE
 
attach(meuse)
#IL DATABASE E' COLLEGATO AL PERCORSO DI RICERCA R 

plot(cadmium,lead,col="red",pch=19, cex=2)
#pch=point character  cex=character exageration

#ESERCIZIO: rame e zinco, carattere triangolo e colore verde
plot(copper,zinc, col="green",pch=17,cex=2)
 
plot(copper,zinc, col="green",pch=17,cex=2, xlab="rame",ylab="zinco")
#PER CAMBIARE LE ETICHETTE DEL GRAFICO SI USANO I COMANDI xlab e ylab
#IL NOME VA TRA VIRGOLETTE

#multipanel o multiframe
par(mfrow=c(1,2))
#LA FUNZIONE par() CREA UN MULTIPANEL, PIU' PLOT DELLE 2 O + IMMAGINI/GRAFICI
#row=riga
#VANNO INSERITI SUCCESSIVAMENTE I PLOT DEI DUE GRAFICI
plot(cadmium,lead,col="red",pch=19, cex=2)
plot(copper,zinc, col="green",pch=17,cex=2)

#invertiamo i grafici da riga/colonna a colonna/riga
par(mfrow=c(2,1))
plot(cadmium,lead,col="red",pch=19, cex=2)
plot(copper,zinc, col="green",pch=17,cex=2)

#multiframe automatico
install.packages("GGally")
library(GGally)
ggpairs(meuse[,3:6])
#LA FUNZIONE CREA UNA MATRICE DI GRAFICI CON UN DETERMINATO SET DI DATI

#Spatial

head(meuse)

coordinates(meuse)=~x+y
plot(meuse)

spplot(meuse,"zinc")
#spplot() serve per plottare i dati spazialmente

#VETTORE: QUANDO SI RIPETONO NUMERI O CARATTERI

#CRAN MIRROR: SOFTWARE UNICO CHE VIENE PROGETTATO IN TUTTO IL MONDO


