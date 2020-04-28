##################CODICE PER ANALISI DEI POINT PATTERNS

install.packages("ggplot2")
# per richiamare il pacchetto library() oppure require()
install.packages("spatstat")

setwd("C:/lab")

#per importare i dati
covid <- read.table("covid_agg.csv", head=T)

#head() fa visualizzare le prime righe 
head(covid)

plot(covid$country,covid$cases)
#attach() in questo modo si evita il $.
attach(covid)
plot(country,cases)

plot(covid$country,covid$cases,las=0) #etichette parallele (labels)
plot(covid$country,covid$cases,las=1) #etichette orizzontali
plot(covid$country,covid$cases,las=2)#etichette perpendicolari all'asse
#las serve per cambiare le etichette
plot(covid$country,covid$cases,las=3) #etichette verticali

#cex:character exageration
plot(covid$country,covid$cases,las=3,cex.lab=0.5, cex.axis=0.5)

#visualizzazione spaziale

#ggplot2
data(mpg)
head(mpg)

#ggplot di esempio: data, aes (aestethics: variabili che comporranno l'estetica finale del grafico), tipo di geometria 
ggplot(mpg,aes(x=displ,y=hwy))+geom_point()
#cambio di geometria, linee
ggplot(mpg,aes(x=displ,y=hwy))+geom_line()
#cambio geometria, poligoni
ggplot(mpg,aes(x=displ,y=hwy))+geom_polygon()

#grafico covid, ggplot
names(covid)
ggplot(covid,aes(x=lon,y=lat,size=cases))+geom_point()
#esercizio density: densità di punti presenti in una certa area

library(spatstat)
#creare data set per spatstat
attach(covid)
covids <- ppp(lon,lat,c(-180,180), c(-90,90))

d <- density(covids)
plot(d)

##########################per inserie i punti: POINTS PATTERN2
#pacchetti utilizzati
install.packages("rgdal")
library(rgdal)

points(covids,pch=19)
plot(d)
points(covid)

#Save the .RData

setwd("C:/lab")

# carichiamo i point pattern in Rdata
load(".RData")

#lista dei vari file
ls()
library(spatstat)
plot(d)
#cambiare la gamma dei colori: palette(in questo caso i minimi valori verranno rappresentati in giallo, i massimi in rosso)
#(100) indica le gradazioni
cl <- colorRampPalette(c('yellow','orange','red')) (100)
plot(d, col=cl)

#esercizio: plot della mappa della densità dal verde al blu.
cl <- colorRampPalette(c('green','yellow','blue')) (150)
plot(d,col=cl)

#per aggiungere i punti del covid si utilizza points()
points(covids)

#caricare dei dati dall'esterno. Inserire i confini dei vari stati.
library(rgdal)

coastlines <- readOGR("ne_10m_coastline.shp")

plot(coastlines, add=T)
#add serve ad aggiungere al plot precedente il nuovo plot, ovvero le coastlines.

#esercizio: plot della mappa di densità con una nuova colorazione ed aggiunta delle coastlines.
plot(d)
cl <- colorRampPalette(c('yellow','orange','red')) (100)
plot(d,col=cl)
points(covids)
plot(coastlines, add=T)

# ESERCIZIO: Caricare il dato .RData workspace point_pattern.RData con la funzione load() e creare un grafico della mappa di densità.
library(spatstat)
library(rgdal) #per poter inserire le coastlines

 setwd("~/lab/")
load("point_pattern2.RData")
ls()

cl5 <- colorRampPalette(c('blue', 'purple', 'red')) (200) 
plot(d, col=cl5, main="density")
points(covids)
coastlines <- readOGR("ne_10m_coastline.shp")
plot(coastlines, add=T)

#interpolazione
head(covid)
#per isualizzare l'intera tabella
View(covid)

#marks del point pattern
marks(covids) <- covid$cases
#Smooth() mappa continua dei vari punti di covid in base ai casi
s <- Smooth(covids)
#s è la mappa dei casi
plot(s)

#Esercizio:plot(s) with points and coastlines
cl5 <- colorRampPalette(c('cyan', 'purple', 'red')) (200) 
plot(s, col=cl5, main="estimate of cases")
points(covids)
coastlines <- readOGR("ne_10m_coastline.shp")
plot(coastlines, add=T)
#chiudere il grafico
dev.off()

##### mappa finale
par(mfrow=c(2,1))

# densità dei punti di covid
cl5 <- colorRampPalette(c('cyan', 'purple', 'red')) (200) 
plot(d, col=cl5, main="density")
points(covids)
coastlines <- readOGR("ne_10m_coastline.shp")
plot(coastlines, add=T)

# interpolazione del numero di casi di covid
cl5 <- colorRampPalette(c('cyan', 'purple', 'red')) (200) 
plot(s, col=cl5, main="estimate of cases")
points(covids)
coastlines <- readOGR("ne_10m_coastline.shp")
plot(coastlines, add=T)

 #la maggior parte dei casi sono stati presi in situ

########## San Marino

setwd("C:/lab")
load("Tesi.RData")
head(Tesi)

#plot della densità
library(spatstat)
#si allega la tabella
attach(Tesi)
summary(Tesi)

#point pattern: x(lon), y(lat), c(xmin, xmax) , c(ymin, ymax)

#x varia da 12.42 a 12. 46
#y varia da 43.91 a 43.94

Tesippp <- ppp(Longitude, Latitude, c(12.41, 12.47), c(43.9,43.95))
#densità
dT <-  density(Tesippp)
plot(dT)
points(Tesippp, col="black")

##################### DAY 3


setwd("C:/lab/")
#le virgolette servono a caricare file esterni
load("sanmarino.RData")
ls()
#dT:mappa di densità ; Tesi: dataset originale ; Tesippp: point pattern (Longitudine e Latitudine) 

#associare i valori che vogliamo stimare nello spazio (interpolazione)

library(spatstat)
#required(): altro modo per richiamare il pacchetto

plot(dT)
points(Tesippp, col="green")
#la densità è direttamente proporzionata ai prati aridi 

head(Tesi)

#associa i valori della variabile al point pattern marks()
#prendere i singoli punti di campionamento ed associarli ai singoli punti del point pattern, in questo caso species richness
marks(Tesippp) <- Tesi$Species_richness

#per vedere l'utilizzo della singola funzione va inserito su R il nome della funzione

#interpolazione: 
interpol <- Smooth(Tesippp)
#plottare la mappa ed inserire i punti
plot(interpol)
points(Tesippp, col="green")

#i valori di richezza specifica piu bassi sono nella parte a nord e sud ovest del data set, più alti ovest e sud-est (RISCRIVI CON REGISTRAZIONE)

library(rgdal) # per leggere i file vettoriali
sanmarino <- readOGR("San_Marino.shp")

plot(sanmarino)

#per aggiungere mappa interpol va aggiunto add=TRUE
plot(interpol, add=TRUE)
#aggiungere i punti
points(Tesippp,col="green")

plot(sanmarino, add=TRUE)
#Esercizio:plot multiframe di densità e interpolazione
#par() crea un multiframe
par(mfrow=c(2,1))
#densità
plot(dT,main="Density of points")
points(Tesippp, col="black")
#interpolazione
plot(interpol, main="Estimate of species richness")
points(Tesippp, col="black")

#main= ; inserisce il titolo al grafico

#Esercizio::plot multiframe di densità e interpolazione con due colonne ed una riga 
par(mfrow=c(1,2))
#densità
plot(dT,main="Density of points")
points(Tesippp, col="black")
#interpolazione
plot(interpol, main="Estimate of species richness")
points(Tesippp, col="black")













