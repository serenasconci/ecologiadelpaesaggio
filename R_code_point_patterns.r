###CODICE PER ANALISI DEI POINT PATTERNS

install.packages("ggplot2")
# per richiamare il pacchetto library() oppure require()
install.packages("spatstat")

#PACCHETTI UTILIZZATI
library(ggplot2)
library(spatstat)

setwd("C:/lab")

#PER IMPORTARE I DATI IN FORMATO TABELLA
covid <- read.table("covid_agg.csv", head=T)

head(covid)
#per visualizzare le prime righe

plot(covid$country,covid$cases)
#attach() in questo modo si evita il $.
attach(covid)
plot(country,cases)

plot(covid$country,covid$cases,las=0) #etichette parallele (labels)
plot(covid$country,covid$cases,las=1) #etichette orizzontali
plot(covid$country,covid$cases,las=2)#etichette perpendicolari all'asse
plot(covid$country,covid$cases,las=3) #etichette verticali
#las=n SERVE PER CAMBIARE IL MODELLO DI ETICHETTA

#cex:character exageration
plot(covid$country,covid$cases,las=3,cex.lab=0.5, cex.axis=0.5)
#cex.lab DEFINISCE LA DIMENSIONE DEL CARATTERE DELLE ETICHETTE
#cex.axis DEFINISCE LA DIMENSIONE DEL CARATTERE DEGLI ASSI 

#visualizzazione spaziale

#ggplot2
data(mpg)
head(mpg)

#ggplot di esempio
#data, aes (aestethics: VARIABILI CHE COMPORRANNO L'ESTETICA FINALE DEL GRAFICO), tipo di geometria 
ggplot(mpg,aes(x=displ,y=hwy))+geom_point()
#CAMBIO DI GEOMETRIA=PUNTI

ggplot(mpg,aes(x=displ,y=hwy))+geom_line()
#CAMBIO DI GEOMETRIA=LINEE

ggplot(mpg,aes(x=displ,y=hwy))+geom_polygon()
#CAMBIO DI GEOMETRIA=POLIGONI

#ggplot di covid
names(covid)
ggplot(covid,aes(x=lon,y=lat,size=cases))+geom_point()

#ESERCIZIO density: DENSITA' DI PUNTI PRESENTI IN UNA CERTA AREA
library(spatstat)
#creare data set per spatstat
attach(covid)
covids <- ppp(lon, lat, c(-180,180), c(-90,90))
#LA FUNZIONE ppp CREA UNA CLASSE, RAPPRESENTA UN SET DI DATI DEL MODELLO A PUNTI NEL PIANO BIDIMENSIONALE
#LON MAX E MIN (-180,180)
#LAT MAX E MIN (-90,90)
d <- density(covids)
plot(d)

##########################per inserie i punti: POINTS PATTERN2
#PACCHETTI UTILIZZATI
install.packages("rgdal")
library(rgdal)
library(spatstat)

points(covids, pch=19)
plot(d)
points(covid)

#Save the .RData

setwd("C:/lab")

load(".RData")
#LA FUNZIONE load() SERVE PER CARICARE I POINT PATTERN IN RData

ls()
# PERMETTE DI VEDERE LA LISTA DEI VARI FILES DELL'RData

plot(d)

#PER CAMBIARE LA GAMMA DI COLORI VIENE USATA LA FUNZIONE colorRampPalette() 

cl <- colorRampPalette(c('yellow','orange','red')) (100)
#IN QUESTO CASO I MINIMI VALORI VERRANNO RAPPRESENTATI IN GIALLO MENTRE I MASSIMI VERRANNO RAPPRESENTATI IN ROSSO
#(100) INDICA IL NUMERO DELLE GRADAZIONI DI COLORI PRESENTI NELLA PALETTE
plot(d, col=cl)
#PLOT DELLA DENSITA' CON USO DELLA PALETTE DENOMINATA cl

#ESERCIZIO: plot della mappa della densità dal verde al blu.
cl <- colorRampPalette(c('green','yellow','blue')) (150)
plot(d,col=cl)

#PER AGGIUNGERE I PUNTI DEL COVID SI UTILIZZA LA FUNZIONE POINT()
points(covids)


library(rgdal)
#PERMETTE DI CARICARE DEI DATI DALL'ESTERNO

coastlines <- readOGR("ne_10m_coastline.shp")
# PER INSERIRE I CONFINI DEI VARI STATI

plot(coastlines, add=T)
#add AGGIUNGE AL PLOT PRECEDENTE UN NUOVO PLOT, IN QUESTO CASO LE COASTLINES 

#ESERCIZIO: plot della mappa di densità con una nuova colorazione ed aggiunta delle coastlines
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

#Interpolazione
head(covid)

View(covid)
#PER VISUALIZZARE L'INTERA TABELLA

marks(covids) <- covid$cases
#ESTRAE IL POINT PATTERN (CASES) E LO ASSOCIA A COVID 

#Smooth()FUNZIONE CHE PERMETTE DI OTTENERE UNA MAPPA CONTINUA DEI VARI PUNTI DI COVID IN BASE AI CASI
s <- Smooth(covids)
#s E' LA MAPPA DEI CASI
plot(s)

#ESERCIZIO:plot(s) with points and coastlines
cl5 <- colorRampPalette(c('cyan', 'purple', 'red')) (200) 
plot(s, col=cl5, main="estimate of cases")
points(covids)
coastlines <- readOGR("ne_10m_coastline.shp")
plot(coastlines, add=T)

dev.off()
#PER CHIUDERE IL GRAFICO

# mappa finale
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

 #LA MAGGIOR PARTE DEI CASI VENGONO PRESI IN SITU

###TESI San Marino

setwd("C:/lab")
load("Tesi.RData")
head(Tesi)

#plot della densità
library(spatstat)

attach(Tesi)
#SI ALLEGA LA TABELLA

summary(Tesi)
#FUNZIONE CHE FA VISUALIZZARE UN RIEPILOGO

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
#LE VIRGOLETTE SERVONO A CARICARE FILES DALL'ESTERNO 
load("sanmarino.RData")
ls()
#dT:mappa di densità ; Tesi: dataset originale ; Tesippp: point pattern (Longitudine e Latitudine) 

#ASSOCIARE I VALORI CHE VOGLIAMO STIMARE NELLO SPAZIO (interpolazione)

library(spatstat)
#required(): MODALITA' ALTERNATIVA DI RICHIAMARE UN PACCHETTO

plot(dT)
points(Tesippp, col="green")
#LA DENSITA' E' DIRETTAMENTE PROPORZIONATA AI PRATI ARIDI

head(Tesi)

marks(Tesippp) <- Tesi$Species_richness
#marks() ASSOCIA I VALORI DELLA VARIABILE AL POINT PATTERN
#PRENDERE I SINGOLI PUNTI DI CAMPIONAMENTOED ASSOCIARLI AI SINGOLI PUNTI DEL POINT PATTERN
#IN QUESTO CASO SPECIES RICHNESS

#PER VEDERE L'UTILIZZO DI UNA SINGOLA FUNZIONE VA INSERITO SU R IL NOME DELLA FUNZIONE 

#interpolazione
interpol <- Smooth(Tesippp)
#plottare la mappa ed inserire i punti
plot(interpol)
points(Tesippp, col="green")

#I VALORI DI RICCHEZZA SPECIFICA PIU' BASSI SONO NELLA PARTE A NORD E SUD-OVEST DEL DATA SET
#I VALORI PIU' ALTI SONO AD OVEST E SUD-EST

library(rgdal) # per leggere i file vettoriali
sanmarino <- readOGR("San_Marino.shp")

plot(sanmarino)

#PER AGGIUNGERE LA MAPPA interpol VA AGGIUNTO add=TRUE
plot(interpol, add=TRUE)
#PER AGGIUNGERE I PUNTI
points(Tesippp,col="green")

plot(sanmarino, add=TRUE)

#ESERCIZIO:plot multiframe di densità e interpolazione
#par() CREA UN MULTIFRAME
par(mfrow=c(2,1))
#densità
plot(dT,main="Density of points")
points(Tesippp, col="black")
#interpolazione
plot(interpol, main="Estimate of species richness")
points(Tesippp, col="black")

#main= INSERISCE IL TITOLO AL GRAFICO

#ESERCIZIO:plot multiframe di densità e interpolazione con due colonne ed una riga 
par(mfrow=c(1,2))
#densità
plot(dT,main="Density of points")
points(Tesippp, col="black")
#interpolazione
plot(interpol, main="Estimate of species richness")
points(Tesippp, col="black")













