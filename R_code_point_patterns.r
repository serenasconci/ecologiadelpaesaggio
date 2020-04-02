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







