# R spatial: funzioni spaziali in Ecologia del paesaggio

library(sp)
#richiamo il pacchetto
 
data(meuse)
#per richiamare i dati

head(meuse)
#fa vedere le prime sei righe
 
attach(meuse)
#il database è collegato al percorso di ricerca R 
#è possibile accedere agli oggetti nel database semplicemente assegnandone il nome

plot(cadmium,lead,col="red",pch=19, cex=2)
#pch=point character  cex=character exageration

#ESERCIZIO: rame e zinco, carattere triangolo e colore verde
plot(copper,zinc, col="green",pch=17,cex=2)
 
plot(copper,zinc, col="green",pch=17,cex=2, xlab="rame",ylab="zinco")
#per cambiare etichette del grafico xlab e ylab, il nome va tra virgolette

#multipanel o multiframe
par(mfrow=c(1,2))
#row=riga
#vanno inseritisuccessivamente i plot dei due grafici
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

# Spatial
head(meuse)

coordinates(meuse)=~x+y
plot(meuse)

#plottare i dati spazialmente
spplot(meuse,"zinc")

#vettore: quando si ripetono numeri o caratteri

#cran mirror: Software unico ma viene progettato in tutto il mondo


