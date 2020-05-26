####R Code Patches

setwd("C:/lab")
library(raster)

install.packages("igraph")
library(igrafh) #for patches
library(ggplot2)

#brick() importa più bande
#raster() importa una singola banda
d1c <- raster("d1c.tif")
d2c <- raster("d2c.tif")

#plot dei due file
#par per inserire più plot in unn grafico
par(mfrow=c(1,2))
#in questo caso avremo solo due classi
cl <- colorRampPalette(c('green','black'))(100) #
plot(d1c,col=cl)
plot(d2c,col=cl)
#in questo caso la classe nera è la foresta mentre la zona verde è la zona agricola

#inverto le due classi
par(mfrow=c(1,2))
cl <- colorRampPalette(c('black','green'))(100) #
plot(d1c,col=cl)
plot(d2c,col=cl)

#forest:class 2, agriculture: classe 1
#non valore NA, eliminiamo tutto quello che non è foresta
#reclassify() deriva dal pacchetto raster, riclassifica un'immagine

d1c.for <- reclassify(d1c, cbind(1,NA))
#cbind()funzione che annulla alcuni valori e in questo caso il valore 1

par(mfrow=c(1,2))
cl <- colorRampPalette(c('black','green'))(100) #
plot(d1c,col=cl)
plot(d1c.for)
#abbiamo reso nullo il valore 1

#cambio di palette
par(mfrow=c(1,2))
cl <- colorRampPalette(c('black','green'))(100) #
plot(d1c,col=cl)
plot(d1c.for, col=cl)

#per vedere i valori
d1c.for

#operazione per il secondo periodo
d2c.for <- reclassify(d2c, cbind(1,NA))

#plot
par(mfrow=c(1,2))
plot(d1c)
plot(d2c)
#mappe solo con le foreste

#creating patches
#clump()patches di celle connesse tra di loro, si trova nel pacchetto raster
#clump applicata alle mappe
d1c.for.patches <- clump(d1c.for)
d2c.for.patches <- clump(d2c.for)

#Writeraster() scriviamo il file appena creato all'interno della cartella lab
#salvare i dati all'esterno
writeRaster(d1c.for.pacthes, "d1c.for.patches.tif")
writeRaster(d2c.for.pacthes, "d2c.for.patches.tif")

#per importare file raster() o brick()
#per esportare file writeRaster()

#ESERCIZIO: plottare le mappe una accanto all'altra
par(mfrow=c(1,2))
plot(d1c.for.pacthes)
plot(d2c.for.pacthes)

#palette
clp <- colorRampPalette(c('dark blue','blue','green','orange','yellow','red'))(100) # 
par(mfrow=c(1,2))
plot(d1c.for.patches, col=clp)
plot(d2c.for.patches, col=clp)

#definire quantitativamente le patches
#nella prima mappa d1 il valore di patches massimo è di 301
#patches d2= 1212

#dataframe 
time <- c("Before deforestation","After deforestation")
npatches <- c(301,1212)

output <- data.frame(time,npatches)
attach(output)

library(ggplot2) 
ggplot(output, aes(x=time, y=npatches, color="red")) + geom_bar(stat="identity", fill="white")

#perso l'area nel nostro trand
#foresta frammentata in molte patches
#situazione molto pericolosa






