# R spatial: funzioni spaziali in Ecologia del paesaggio

#library(): richiamo il pacchetto
library(sp)

 #per richiamare i dati
 data(meuse)
 
 #head():fa vedere le prime sei righe
 head(meuse)
 
 #attach(): blocca 
 attach(meuse)
 
 #pch=point character  cex=character exageration
 plot(cadmium,lead,col="red",pch=19, cex=2)
 
 #esercizio: rame e zinco, carattere triangolo e colore verde
 plot(copper,zinc, col="green",pch=17,cex=2)
 
 #per cambiare etichette del grafico
plot(copper,zinc, col="green",pch=17,cex=2, xlab="rame",ylab="zinco")

#multipanelo multiframe
par(mfrow=c(1,2))
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
