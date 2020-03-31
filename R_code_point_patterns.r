##################CODICE PER ANALISI DEI POINT PATTERNS

install.packages("ggplot2")
#library(ggplot2) oppure require(ggplot2)
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

#ggplot di esempio: data, aes (aestethics: variabili che omporranno l'estetica finale del grafico), tipo di geometria 
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
#per inserie i punti
points(covids,pch=19)
plot(d)
points(covid)

