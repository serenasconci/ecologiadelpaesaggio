##############R code analisi temporale della variazione della land cover

setwd("C:/lab/")

library(raster)

#brick()carica i dati dall'esterno, e nel caso dell'immagine satellitare carica tutte le bande. Funzione di raster.
defor1 <- brick("defor1_.jpg")
defor2 <- brick("defor2_.jpg")

defor1
#sono presenti tre bande
#names: defor1_.1,defor1_.2, defor1_.3 (nir; rosso; verde)
#defor1_.1 = NIR
#defor1_.2 = ROSSO
#defor1_.3 =VERDE

plotRGB(defor1, r=1, g=2, b=3, stretch="Lin")

#ESERCIZIO:plot di defor2

plotRGB(defor2, r=1, g=2,b=3, stretch="Lin")

#caricare entrambe le immagini

par(mfrow=c(2,1))
plotRGB(defor1, r=1, g=2, b=3, stretch="Lin")
plotRGB(defor2, r=1, g=2,b=3, stretch="Lin")

#creazione di due classi (classifcazione non supervisionata) mediante la funzione unsuperClass()
#necessario il pacchetto RStoolbox
library(RStoolbox)

#(immagine originale e numero di classi)
d1c <- unsuperClass(defor1, nClasses=2)
 
 
plot(d1c$map)
#cambio colori dell'immagine 
cl <- colorRampPalette(c('black','green'))(100)
plot(d1c$map, col=cl)



#ESERCIZIO: classificazione immagine satellitare defor2 con due classi
d2c <- unsuperClass(defor2, nClasses=2)
plot(d2c$map, col=cl)
#classe 1 non foresta, classe 2 foresta

dev.off()

#plot delle de mappe ottenute
par(mfrow=c(1,2))
plot(d1c$map, col=cl)
plot(d2c$map, col=cl)
#oppure
par(mfrow=c(2,1))
plot(d1c$map, col=cl)
plot(d2c$map, col=cl)

#stima dei valori variabili
#misurare la frequenza delle due classi nella prima mappa defor1
freq(d1c$map)
#aree aperte=34181
#foresta=307111

totd1 <- 34181 + 307111
#totale=341292

#calcolare la percentuale 
percent1 <- freq(d1c$map) * 100 / totd1
percent1
#89.9 per le foreste
#10.1 per le aree aperte


#frequeza mappa defor2
freq(d2c$map)
#aree aperte=164170
#foresta=178556

totd2 <- 178556 + 164170
#totd2=342726


#percentuale defor2
#aree aperte= 47.9
#foreste= 52.1


#plot finale con i dati ottenuti
cover <- c("Agriculture", "Forest")
before <- c(10.1, 89.9)
after <- c(47.9,52.1)

#dataframe finale
#crea una tabella con i dati 
output <-data.frame(cover, before, after)
#per visualizzare la tabella
View(output)



library(ggplot2)

##############DAY2
setwd("C:/lab")

load("defor.RData")

ls()
#richiamare le librerie
library(raster)

#mappa multitemporale
par(mfrow=c(1,2))
cl <- colorRampPalette(c('black','green'))(100) # 
plot(d1c$map, col=cl)
plot(d2c$map, col=cl)

library(ggplot2)
#grafico
#histograms of the % cover before deforestation
ggplot(output, aes(x=cover, y=before, color=cover)) +
geom_bar(stat="identity", fill="white")
#identity: si prendono direttamente i valori di copertura
 

#ESERCIZIO: % cover after deforestation
ggplot(output, aes(x=cover, y=after, color=cover)) +
geom_bar(stat="identity", fill="white")

#plot di entrambi gli istogrammi

install.packages("gridExtra")
library(gridExtra) #o require()

#ESERCIZIO: use grid.arrange to plot the two graphs
#prende i vari plot e li mette insieme in un unico grafico, ha la stessa funzione del par()
#grid.arrange(plot1, plot2, nrow=1)

grafico1 <- ggplot(output, aes(x=cover, y=before, color=cover)) + 
geom_bar(stat="identity", fill="white")

grafico2 <- ggplot(output, aes(x=cover, y=after, color=cover)) + 
geom_bar(stat="identity", fill="white")


grid.arrange(grafico1, grafico2, nrow=1)














