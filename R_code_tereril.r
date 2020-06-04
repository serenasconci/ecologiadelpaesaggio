################Codice R per le analisi di immagini satellitari

#pacchetto raster
install.packages("raster")
library(raster)

setwd("C:/lab")

p224r63_2011 <- brick("p224r63_2011_masked.grd")
#brick()IMPORTA UN'IMMAGINE SATELLITARE CON TUTTE LE BANDE SU R 
#IL SIMBOLO <- DA IL NOME ALL'IMMAGIN, E' PREFERIBILE IN QUESTO CASO INSERIRE ANCHE L'ANNO

plot(p224r63_2011)

#save .RData

#DAY 2

setwd("C:/lab")

load("teleril.RData")
#load()RICARICA IL FILE SALVATO IN PRECEDENZA
#IL NOME DEL FILE VA INSERITO TRA VIRGOLETTE

ls()
#LISTA DEI NOMI DEI VARI FILE SALVARI IN .RData

library(raster)

plot(p224r63_2011)

# B1: blue
# B2: green
# B3: red
# B4: near infrared (nir)
# B5: medium infrared
# B6: thermal infrared
# B7: medium infrared
#LE PRIME TRE BANDE FANNO PARTE DEL VISIBILE

cl <- colorRampPalette(c('black','grey','light grey'))(100) 
#(100) SONO LE MICROCLASSI DI COLORI DAL NERO AL GRIGIO CHIARO
#CON col= SI INSERISCE LA PALETTE
plot(p224r63_2011, col=cl)

#ESERCIZIO: inserire 5 microclassi, (L'IMMAGINE CON 5 MICROCLASSI SARA' PIU' SGRANATA
cllow <- colorRampPalette(c('black','grey','light grey'))(5) 
plot(p224r63_2011, col=cllow)

#ESERCIZIO: gamma di colore in 'blue'
names(p224r63_2011)
#SI OTTENGONO I NOMI DELLE BANDE: "B1_sre" "B2_sre" "B3_sre" "B4_sre" "B5_sre" "B6_bt"  "B7_sre"
clb <- colorRampPalette(c('dark blue','blue','light blue'))(100) 
plot(p224r63_2011$B1_sre, col=clb)
#attach() NON FUNZIONA CON IL PACCHETTO RASTER
#IL SIMBOLO CHE LEGA LA COLONNA(banda) AL DATA SET (immagine satellitare)E' IL $

#ESERCIZIO: plottare la banda dell'infrasosso vicino (nir) con colorRampPalette in rosso, arancione e giallo.
clnir <- colorRampPalette(c('red','orange','yellow')) (100)
plot(p224r63_2011$B4_sre, col=clnir)
#LE PIANTE RIFLETTONO MOLTO IN NEAR INFRARED(nir), QUINDI IN QUESTO CASO PROBABILMENTE C'E' MOLTA VEGETAZIONE

#multiframe
#par() FUNZIONE CHE PERMETTE DI UTILIZZARE A BLOCCHI LA FIESTRA
#row=riga
#DIVIDIAMO IL PANNELLO IN 2x2
par(mfrow=c(2,2))
#blue
clb <- colorRampPalette(c('dark blue','blue','light blue'))(100) 
plot(p224r63_2011$B1_sre, col=clb)
#green
clg <- colorRampPalette(c('dark green','green','light green'))(100) 
#VARIA ANCHE IL CODICE DELLA BANDA
plot(p224r63_2011$B2_sre, col=clg)
#red
clr <- colorRampPalette(c('dark red','red','pink'))(100) 
plot(p224r63_2011$B3_sre, col=clr)
#near infrared
clnir <- colorRampPalette(c('red','orange','yellow')) (100)
plot(p224r63_2011$B4_sre, col=clnir)

dev.off()
#CHIUDE LA FINESTRA GRAFICA

#natural colours, 3 componenti R,G e B.
#3 bands: R= banda del rosso, G= banda del verde, B= banda del blu.
# B1: blue - 1
# B2: green - 2
# B3: red - 3
# B4: near infrared (nir) - 4
plotRGB(p224r63_2011, r=3, g=2, b=1) 
#PER ALLARGARE I COLORIstretch() stretch="Lin"= lineare, E' IL PIU' UTILIZZATO 
plotRGB(p224r63_2011, r=3, g=2, b=1, stretch="Lin")

#nir
#false colours
#componente red= infrarosso vicino, FARA' VEDERE LA VEGETAZIONE
#IN CELESTE VENGONO RAPPRESENTATE LE ZONE A SUOLO NUDO, AGRICOLE
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin")


#PER SALVARE IN PDF
pdf("primo grafico.pdf")
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin")
dev.off()
#nir nella componente R(red)
par(mfrow=c(2,1))
plotRGB(p224r63_2011, r=3, g=2, b=1, stretch="Lin")
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin")
dev.off()

#ESERCIZIO: nir nella componente G(green). In precedenza era stato montato in red.
plotRGB(p224r63_2011, r=3, g=4, b=2, stretch="Lin")
#LA VEGETAZIONE DIVENTA VERDE
#LA ZONA AGRICOLA E' RAPPRESENTATA IN VIOLA

#ESERCIZIO: nir nella componente B(blu)
plotRGB(p224r63_2011, r=3, g=2, b=4, stretch="Lin")
#LA VEGETAZIONE DIVENTA BLU
#LA ZONA AGRICOLA DIVENTA GIALLA

# DAY3

library(raster)

setwd("C:/lab")
#RICHIAMA LA CARTELLA CHE UTILIZZEREMO PER USARE E SALVARE I DATI

load("teleril2.RData")
#CARICA IL FILE SALVATO IN PRECEDENZA

ls()
#LISTA DEI DATI

#carico file del 1988 - p224r63_1988_masked
#brick() UTILIZZATO PER IMPORTARE LE IMMAGINI SATELLITARI IN TUTTE LE BANDE

p224r63_1988 <- brick("p224r63_1988_masked.grd")
#grd=griglia di pixel (riga e colonna)
plot(p224r63_1988)

#multiframe
par(mfrow=c(2,2))
#blu
clb <- colorRampPalette(c('dark blue','blue','light blue'))(100) 
plot(p224r63_1988$B1_sre, col=clb)
#green
clg <- colorRampPalette(c('dark green','green','light green'))(100) 
plot(p224r63_1988$B2_sre, col=clg)
#red
clr <- colorRampPalette(c('dark red','red','pink'))(100) 
plot(p224r63_1988$B3_sre, col=clr)
#infrarosso vicino
clnir <- colorRampPalette(c('red','orange','yellow')) (100)
plot(p224r63_1988$B4_sre, col=clnir)

dev.off()

#RGB 3 componenti
# B1: blue - 1
# B2: green - 2
# B3: red - 3
# B4: near infrared (nir) - 4
#stretch() PERMETTE DI VEDERE MEGLIO I COLORI "ALLUNGANDOLI"
#plot con colori naturali, rosso verde e blu.
plotRGB(p224r63_1988,r=3,g=2,b=1, stretch="Lin")
#ESERCIZIO: plotta l'immagine usando nir nella componente "r" in RGB
plotRGB(p224r63_1988,r=4,g=3,b=2, stretch="Lin")

#plot delle due immagini 2011 e 1988

par(mfrow=c(2,1))
plotRGB(p224r63_1988,r=4,g=3,b=2, stretch="Lin",main="1988")
plotRGB(p224r63_2011,r=4,g=3,b=2, stretch="Lin",main="2011")
dev.off()
#CON RASTER NON VIENE CONSIDERATO main CHE HA LA FUNZIONE DI DARE UN TITOLO ALL'IMMAGINE
#LA FOGLIA DI UNA PIANTA SANA RIFLETTE MOLTO IN NIR E POCO NELLA BANDA DEL ROSSO PERCHE' VIENE ASSORBITA PER FARE LA FOTOSINTESI
#LA FOGLIA DI UNA PIANTA MALATA RIFLETTE MENO IL NIR, LA BANDA DEL ROSSO AUMENTA PERCHE' NON FA PIU' FOTOSINTESI QUINDI NON VIENE ASSORBITO
#SPECTRAL INDICES
#L'INDICE USATO PER VALUTARE LO STATO DELLA VEGETAZIONE E' IL DVI (difference vegetation index)
#DVI=NIR-RED
#dvi1988=nir1988-red1988
dvi1988 <- p224r63_1988$B4_sre - p224r63_1988$B3_sre
plot(dvi1988)

#dvi2011=nir-red2011
dvi2011 <- p224r63_2011$B4_sre -p224r63_2011$B3_sre
plot(dvi2011)

#cambiare la colorRampPalette
cldvi <- colorRampPalette(c('light blue','light green', 'green')) (100)
plot(dvi2011, col=cldvi)
#MULTITEMPORAL ANALYSIS= DIFFERENZA NEL TEMPO TRA I DUE INDICI
difdvi <- dvi2011-dvi1988
plot(difdvi)
#cambio palette
cldifdvi <- colorRampPalette(c('red','white','blue'))(100) # 
plot(difdvi, col=cldifdvi)

#visualize the output
#multiframe= 1988rgb, 2011rgb e difdiv
par(mfrow=c(3,1))
plotRGB(p224r63_1988, r=4, g=3, b=2, stretch="Lin")
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin")

plot(difdvi, col=cldifdvi)

dev.off()

#RISOLUZIONE O GRANA 
#Changing the grain (resolution)
#AGGREGARE I PIXEL PER RENDERLI PIU' GRANDI(MENO DEFINIZIONE)
#factor è x10 INDICA QUANTO GRANDE SARA' IL PIXEL
p224r63_2011lr <- aggregate(p224r63_2011, fact=10)

p224r63_2011 
p224r63_2011lr

#confonto tra le due risoluzioni
par(mfrow=c(2,1))
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin")
plotRGB(p224r63_2011lr, r=4, g=3, b=2, stretch="Lin")


#lower resolution
p224r63_2011lr50 <- aggregate(p224r63_2011, fact=50)
p224r63_2011lr50 
#original 30 m -> resampled 1500m

#PLOT IMMAGINE ORIGINALE- IMMAGINE A BASSA RISOLUZIONE CON FATTORE 10 E CON FATTORE 50 
par(mfrow=c(3,1))
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin")
plotRGB(p224r63_2011lr, r=4, g=3, b=2, stretch="Lin")
plotRGB(p224r63_2011lr50, r=4, g=3, b=2, stretch="Lin")

#DVI DEL 2011 A BASSA RISOLUZIONE
dvi2011lr50 <- p224r63_2011lr50$B4_sre - p224r63_2011lr50$B4_sre

#DVI del 19988
#DIMINUIRE LA RISOLUZIONE
p224r63_1988lr50 <- aggregate(p224r63_1988, fact=50)
#CALCOLO INDICE DI VEGETAZIONE DEL 1988
dvi1988lr50 <- p224r63_1988lr50$B4_sre - p224r63_1988lr50$B3_sre
#differenza del DVI low resolution
difdvilr50 <- dvi2011lr50 - dvi1988lr50
plot(difdvilr50)
plot(difdvilr50,col=cldifdvi)
 
#multiframe 
par(mfrow=c(2,1))
plot(difdvi, col=cldifdvi)
plot(difdvilr50, col=cldifdvi)
