################### R spatial

#libreria
library(sp)

#dati da usare
data(meuse)

head(meuse)

#coordinate del dataset
coordinates(meuse)=~x+y

#spplot dei dati di zinco
spplot(meuse,"zinc")

#ESERCIZIO:spplot dati di rame
head(meuse)
spplot(meuse,"copper")

#bubble altro modo per plottare i dati
bubble(meuse,"zinc")
#ESERCIZIO: bubble del rame, colorato di rosso
bubble(meuse,"copper",col="red")

#foraminiferi (Sofia), carbon capture (Marco)
foram <- c(10, 20, 35, 55, 67, 80)
carbon <- c(5, 15, 30, 70, 85, 99)
plot(foram,carbon,col="green",cex=2,pch=19)

#dati dall'esterno:covid-19
#specificare la cartella dove si andrà a lavorare. C:/lab
setwd("C:/lab") #Windows

covid <- read.table("covid_agg.csv",head=TRUE)
#read.table("nome file, head=TRUE")permette di leggere la tabella
head(covid)


