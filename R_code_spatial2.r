### R spatial

library(sp)

#dati da usare
data(meuse)

head(meuse)

coordinates(meuse)=~x+y
#COORDINATE DEL DATA SET

spplot(meuse,"zinc")
#spplot dei dati di zinco

#ESERCIZIO:spplot dati di rame
head(meuse)
spplot(meuse,"copper")

bubble(meuse,"zinc")
#LA FUNZIONE bubble() E' UN ALTRO MODO PER PLOTTARE I DATI
#CREA UN GRAFICO A BOLLE DI DATI SPAZIALI

#ESERCIZIO: bubble del rame, colorato di rosso
bubble(meuse,"copper",col="red")

#esempio dati ottenuti da tesi
#foraminiferi (Sofia), carbon capture (Marco)
foram <- c(10, 20, 35, 55, 67, 80)
carbon <- c(5, 15, 30, 70, 85, 99)
plot(foram,carbon,col="green",cex=2,pch=19)

# <- PERMETTE DI DARE UN TITOLO 

#dati dall'esterno:covid-19

setwd("C:/lab") #Windows
#IL SET WORKING DIRECTORY SERVE AD INDICARE LA CARTELLA DOVE SI ANDRA' A LAVORARE
#IN QUESTO CASO VERRA' UTILIZZATA LA CARTELLA lab

covid <- read.table("covid_agg.csv",head=TRUE)
#LA FUNZIONE read.table(NOME DEL FILE, head=TRUE")PERMETTE DI LEGGERE UN FILE IN FORMATO TABELLA
#CREA UN FRAME DI DATI
#A QUESTA TABELLA E' STATO ASSEGNATO IL NOME covid

head(covid)


