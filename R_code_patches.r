###10. R_code_patches.r

setwd("C:/lab")

library(raster)

install.packages("igraph")
library(igrafh) #for patches
library(ggplot2)

#brick() IMPORTA TUTTE LE BANDE
#raster() IMPORTA UNA SINGOLA BANDA
d1c <- raster("d1c.tif")
d2c <- raster("d2c.tif")

#plot dei due file
#par() PER INSERIRE PIU' PLOT IN UN GRAFICO
par(mfrow=c(1,2))
#IN QUESTO CASO AVREMO SOLO DUE CLASSI
cl <- colorRampPalette(c('green','black'))(100) #
plot(d1c,col=cl)
plot(d2c,col=cl)
#IN QUESTO CASO LA CLASSE NERA E' LA FORSTA MENTRE LA ZONA VERDE E' LA ZONA AGRICOLA

#INVERTIAMO LE DUE CLASSI
par(mfrow=c(1,2))
cl <- colorRampPalette(c('black','green'))(100) #
plot(d1c,col=cl)
plot(d2c,col=cl)

#forest:class 2
#agriculture:classe 1

#reclassify() DERIVA DAL PACCHETTO RASTER E RICLASSIFICA UN'IMMAGINE
d1c.for <- reclassify(d1c, cbind(1,NA))
#cbind()FUNZIONE CHE ANNULLA ALCUNI VALORI, IN QUESTO CASO IL VALORE 1
#non valore NA, ELIMINIAMO TUTTO QUELLO CHE NON E' FORESTA

par(mfrow=c(1,2))
cl <- colorRampPalette(c('black','green'))(100) #
plot(d1c,col=cl)
plot(d1c.for)
#IL VALORE 1 E' STATO RESO NULLO

#cambio di palette
par(mfrow=c(1,2))
cl <- colorRampPalette(c('black','green'))(100) #
plot(d1c,col=cl)
plot(d1c.for, col=cl)

#PER VEDERE TUTTI I VALORI
d1c.for

#operazione per il secondo periodo
d2c.for <- reclassify(d2c, cbind(1,NA))

#plot
par(mfrow=c(1,2))
plot(d1c)
plot(d2c)
#MAPPE SOLO CON LE FORESTE

#creating patches
#clump()PATCHES DI CELLE CONNESSE TRA DI LORO, SI TROA NEL PACCHETTO RASTER
#clump()APPLICARA ALLE MAPPE
d1c.for.patches <- clump(d1c.for)
d2c.for.patches <- clump(d2c.for)

#Writeraster() SCRIVIAMO IL FILE APPENA CREATO ALL'INTERNO DELLA CARTELLA lab
#salvare i dati all'esterno
writeRaster(d1c.for.pacthes, "d1c.for.patches.tif")
writeRaster(d2c.for.pacthes, "d2c.for.patches.tif")

#PER IMPORTARE I FILES file raster() o brick()
#PER ESPORTARE I FILESwriteRaster()

#ESERCIZIO: plottare le mappe una accanto all'altra
par(mfrow=c(1,2))
plot(d1c.for.pacthes)
plot(d2c.for.pacthes)

#palette
clp <- colorRampPalette(c('dark blue','blue','green','orange','yellow','red'))(100) # 
par(mfrow=c(1,2))
plot(d1c.for.patches, col=clp)
plot(d2c.for.patches, col=clp)

#DEFINIRE QUANTITATIVAMENTE LE PATCHES
#PRIMA MAPPA d1 IL VALORE MAX DELLE PATCHES E' 301
#SECONDA MAPPA d2 IL VALORE MAX DELLE PATCHES E' 1212

#dataframe 
time <- c("Before deforestation","After deforestation")
npatches <- c(301,1212)

output <- data.frame(time,npatches)
attach(output)

library(ggplot2) 

ggplot(output, aes(x=time, y=npatches, color="red")) + geom_bar(stat="identity", fill="white")

#E' STATA PERSA AREA NEL NOSTRO TRAND
#LA FORESTA RISULTA FRAMMENTATA IN MOLTE PATCHES
#SITUAZIONE MOLTO PERICOLOSA

