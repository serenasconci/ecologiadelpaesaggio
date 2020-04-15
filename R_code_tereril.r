################Codice R per le analisi di immagini satellitari
#pacchetto raster
install.packages("raster")
library(raster)

setwd("C:/lab")

#il simbolo <- da il nome all'immagine, è preferibile in questo caso inserire anche l'anno
#brick() importa un'immagine satellitare con tutte le bande su R 

p224r63_2011 <- brick("p224r63_2011_masked.grd")

plot(p224r63_2011)

#save .RData

################# DAY 2

setwd("C:/lab")

#load() ricarica il file salvato in precedenza, va inserito il nome del file tra virgolette
load("teleril.RData")

#lista dei nomi dei vari file
ls()

library(raster)

plot(p224r63_2011)

#prime tre bande fanno parte del visibile
# B1: blue
# B2: green
# B3: red
# B4: near infrared (nir)
# B5: medium infrared
# B6: thermal infrared
# B7: medium infrared

cl <- colorRampPalette(c('black','grey','light grey'))(100) 
#con 100 si intendono le microclassi di colori dal nero al grigio chiaro
#con col si inserisce la palette 
plot(p224r63_2011, col=cl)

#Esercizio: inserire 5 microclassi, ovviamente l'immagine sarà più sgranata
cllow <- colorRampPalette(c('black','grey','light grey'))(5) 
plot(p224r63_2011, col=cllow)

#Esercizio:gamma di colore in 'blue'
names(p224r63_2011)
#si ottengono i nomi delle bande: "B1_sre" "B2_sre" "B3_sre" "B4_sre" "B5_sre" "B6_bt"  "B7_sre"
clb <- colorRampPalette(c('dark blue','blue','light blue'))(100) 
plot(p224r63_2011$B1_sre, col=clb)
#attach() non funziona con il pacchetto raster
#il simbolo che lega la colonna(banda) al dataset(immagine satellitare)è il $

#esercizio: plottare la banda dell'infrasosso vicino(nir) con colorRampPalette in rosso, arancione e giallo.
clnir <- colorRampPalette(c('red','orange','yellow')) (100)
plot(p224r63_2011$B4_sre, col=clnir)
#piante riflettono molto in near infrared(nir), probabilmente c'è molta vegetazione.

#multiframe
#par() funzione che permette di utilizzare a blocchi la finestra
#row=riga
#dividiamo il pannello in 2x2
par(mfrow=c(2,2))
#blue
clb <- colorRampPalette(c('dark blue','blue','light blue'))(100) 
plot(p224r63_2011$B1_sre, col=clb)
#green
clg <- colorRampPalette(c('dark green','green','light green'))(100) 
#varia ovviamente anche il codice della banda 
plot(p224r63_2011$B2_sre, col=clg)
#red
clr <- colorRampPalette(c('dark red','red','pink'))(100) 
plot(p224r63_2011$B3_sre, col=clr)
#near infrared
clnir <- colorRampPalette(c('red','orange','yellow')) (100)
plot(p224r63_2011$B4_sre, col=clnir)

#chiude la finestra grafica
dev.off()

#natural colours, 3 componenti R,G e B.
#3 bands: R= banda del rosso, G= banda del verde, B= banda del blu.
# B1: blue - 1
# B2: green - 2
# B3: red - 3
# B4: near infrared (nir) - 4
plotRGB(p224r63_2011, r=3, g=2, b=1) 
#per allargare i colori stretch() stretch="Lin"= lineare, il più utilizzato.
plotRGB(p224r63_2011, r=3, g=2, b=1, stretch="Lin")

#nir
#false colours
#componente red= infrarosso vicino, farà vedere la vegetazione
#in celeste vengono rappresentate le zone a suolo nudo, agricole.
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin")


#salvare in pdf
pdf("primo grafico.pdf")
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin")
dev.off()
#nir nella componente R(red)
par(mfrow=c(2,1))
plotRGB(p224r63_2011, r=3, g=2, b=1, stretch="Lin")
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin")
dev.off()

#Esercizio:nir nella componente G(green). In precedenza era stato montato in red.
plotRGB(p224r63_2011, r=3, g=4, b=2, stretch="Lin")
#vegetazione diventa verde
#zona agricola rappresentata in viola

#Esercizio: nir nella componente B(blu)
plotRGB(p224r63_2011, r=3, g=2, b=4, stretch="Lin")
#vegetazione diventa blu
#zona agricola, suolo nudo di colore giallo

################## DAY3

library(raster)
#richiama la cartella che utilizzeremo per usare e salvare i dati
setwd("C:/lab")
#carica il file salvato in precendenza
load("teleril2.RData")

#lista dei dati 
ls()

#carico file del 1988 - p224r63_1988_masked
#brick utilizzato per importare le immagini satellitari in tutte le bande
#grd=griglia di pixel (riga e colonna)
p224r63_1988 <- brick("p224r63_1988_masked.grd")
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
#serve a chiudere la finestra
dev.off()

#RGB 3 componenti
# B1: blue - 1
# B2: green - 2
# B3: red - 3
# B4: near infrared (nir) - 4
#stretch, permette di vedere meglio i colori "allungandoli"
#plot con colori naturali, rosso verde e blu.
plotRGB(p224r63_1988,r=3,g=2,b=1, stretch="Lin")
#Esercizio, plotta l'immagine usando nir nella componente "r" in RGB
plotRGB(p224r63_1988,r=4,g=3,b=2, stretch="Lin")

#plot delle due immagini 2011 e 1988

par(mfrow=c(2,1))
plotRGB(p224r63_1988,r=4,g=3,b=2, stretch="Lin",main="1988")
plotRGB(p224r63_2011,r=4,g=3,b=2, stretch="Lin",main="2011")
dev.off()
#con raster non viene considerato main, che inserisce un titolo all'immagine
#foglia pianta sana riflette molto in nir e poco nella banda del rosso perchè viene assorbita per fare la fotosintesi
#foglia pianta malata riflette meno il nir ed aumenta la banda del rosso perchè non fa più fotosintesi 
#SPECTRAL INDICES
#indice per valutare lo stato della vegetazione DVI (difference vegetation index)
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
#differenza nel tempo tra i due indici= mulitemporal analysis
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

#risoluzione o grana
#Changing the grain (resolution)
#aggregare i pixel per renderli più grandi, meno definita
#factor è x10 ed indica quanto grande sarà il pixel
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

#plot imm. originale, imm. a bassa risoluzione con fattore 10 e con fattore 50 
par(mfrow=c(3,1))
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin")
plotRGB(p224r63_2011lr, r=4, g=3, b=2, stretch="Lin")
plotRGB(p224r63_2011lr50, r=4, g=3, b=2, stretch="Lin")

#DVI del 2011 a bassa risoluzione
dvi2011lr50 <- p224r63_2011lr50$B4_sre - p224r63_2011lr50$B4_sre

#DVI del 19988
#diminuire la risoluzione
p224r63_1988lr50 <- aggregate(p224r63_1988, fact=50)
#calcolo indice di vegetazione del 1988
dvi1988lr50 <- p224r63_1988lr50$B4_sre - p224r63_1988lr50$B3_sre
#differenza del DVI lo resolution
difdvilr50 <- dvi2011lr50 - dvi1988lr50
plot(difdvilr50)
plot(difdvilr50,col=cldifdvi)
 
#multiframe 
par(mfrow=c(2,1))
plot(difdvi, col=cldifdvi)
plot(difdvilr50, col=cldifdvi)
