##############R code analisi temporale della variazione della land cover

setwd("C:/lab/")

library(raster)

#brick()CARICA I DATI DALL'ESTERNO, NEL CASO DI UN'IMMAGINE SATELLITARE CARICA TUTTE LE BANDE (FUNZIONE DI RASTER) 
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

#CARICARE ENTRAMBE LE IMMAGINI CON LA FUNZIONE par()
par(mfrow=c(2,1))
plotRGB(defor1, r=1, g=2, b=3, stretch="Lin")
plotRGB(defor2, r=1, g=2,b=3, stretch="Lin")

#MEDIANTE LA FUNZIONE unsuperClass() ANDIAMO A CREARE DUE CLASSI (classifcazione non supervisionata)
#E' NECESSARIO IL PACCHETTO RStoolbox
library(RStoolbox)

d1c <- unsuperClass(defor1, nClasses=2)
#IMMAGINE ORIGINALE PIU' IL NUMERO DI CLASSI DESIDERATE
 
plot(d1c$map)
#CAMBIO DI PALETTE
cl <- colorRampPalette(c('black','green'))(100)
plot(d1c$map, col=cl)

#ESEMPIO sul significato del $
#mappageologica <- geomap(im_sat, nClasses=...)
#plot(mappageologica$lito)
#plot(mappageologica$lineaments)

#ESERCIZIO: classificazione immagine satellitare defor2 con due classi
d2c <- unsuperClass(defor2, nClasses=2)
plot(d2c$map, col=cl)
#CLASSE 1 NON FORESTA
#CLASSE2 FORESTA

dev.off()

#plot delle de mappe ottenute
par(mfrow=c(1,2))
plot(d1c$map, col=cl)
plot(d2c$map, col=cl)
#INVERSIONE NUMERO DI RIGHE E NUMERO DI COLONNE
par(mfrow=c(2,1))
plot(d1c$map, col=cl)
plot(d2c$map, col=cl)

#STIMA DEI VALORI VARIABILI
#MISURARE LA FREQUENZA DELLE DUE CLASSI NELLA PRIMA MAPPA defor1
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

#PLOT FINALE CON DATI OTTENUTI
cover <- c("Agriculture", "Forest")
before <- c(10.1, 89.9)
after <- c(47.9,52.1)

#dataframe finale
output <-data.frame(cover, before, after)
#crea una tabella con i dati 

View(output)
#PER VISUALIZZARE LA TABELLA

#DAY2

setwd("C:/lab")

load("defor.RData")

ls()

library(raster)

#MAPPA MULTITEMPORALE
par(mfrow=c(1,2))
cl <- colorRampPalette(c('black','green'))(100) # 
plot(d1c$map, col=cl)
plot(d2c$map, col=cl)

library(ggplot2)
#grafico
#histograms of the % cover before deforestation
ggplot(output, aes(x=cover, y=before, color=cover)) +
geom_bar(stat="identity", fill="white")
#identity: VENGONO PRESI DIRETTAMENTE I VALORI DI COPERTURAsi 

#ESERCIZIO: % cover after deforestation
ggplot(output, aes(x=cover, y=after, color=cover)) +
geom_bar(stat="identity", fill="white")

#plot di entrambi gli istogrammi
install.packages("gridExtra")
library(gridExtra) #require()

#ESERCIZIO: use grid.arrange to plot the two graphs
#grid.arrange PRENDE I VARI PLOT E LI METTE INSIEME IN UN UNICO GRAFICO, HA LA STESSA FUNZIONE DELLA FUNZIONE par()
grafico1 <- ggplot(output, aes(x=cover, y=before, color=cover)) + 
geom_bar(stat="identity", fill="white")

grafico2 <- ggplot(output, aes(x=cover, y=after, color=cover)) + 
geom_bar(stat="identity", fill="white")

grid.arrange(grafico1, grafico2, nrow=1)
#grid.arrange(plot1, plot2, nrow=1)

#DAY 3

library(ggplot2)

cover <- c("Agriculture","Forest")
before <- c(10.9,89.1)
after <- c(48.2,51.8)

output <- data.frame(cover,before,after)
output

library(gridExtra) # require()

grafico1 <- ggplot(output, aes(x=cover, y=before, color=cover)) +
geom_bar(stat="identity", fill="white")

grafico2 <- ggplot(output, aes(x=cover, y=after, color=cover)) +
geom_bar(stat="identity", fill="white")

# mette insieme i due grafici
grid.arrange(grafico1, grafico2, nrow = 1)

grafico1 <- ggplot(output, aes(x=cover, y=before, color=cover)) + 
geom_bar(stat="identity", fill="white") +
ylim(0, 100)
#ylim() IMPOSTA I LIMITI DELL'ASSE Y
grafico2 <- ggplot(output, aes(x=cover, y=after, color=cover)) + 
geom_bar(stat="identity", fill="white") +
ylim(0, 100)

# Exercise: use grid.arrange to plot the two graphs 
grid.arrange(grafico1, grafico2, nrow = 1)
