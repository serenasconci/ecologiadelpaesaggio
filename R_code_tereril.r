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


