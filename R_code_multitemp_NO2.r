###8. R_code_multitemp_NO2.r

#R code for analysing NO2 data from ESA - January to March 2020

library(raster)

setwd("C:/lab")

EN01 <- raster("EN_0001.png")
#raster()FUNZIONE CHE PERMETTE DI CARICARE UN'IMMAGINE CON UNA SOLA BANDA
#FUNZIONE CONTRARIA A brick() CHE CARICA TUTTE LE BANDE
plot(EN01)

#Esercizio: import all the images 
EN01 <- raster("EN_0001.png")
EN02 <- raster("EN_0002.png")
EN03 <- raster("EN_0003.png")
EN04 <- raster("EN_0004.png")
EN05 <- raster("EN_0005.png")
EN06 <- raster("EN_0006.png")
EN07 <- raster("EN_0007.png")
EN08 <- raster("EN_0008.png")
EN09 <- raster("EN_0009.png")
EN10 <- raster("EN_0010.png")
EN11 <- raster("EN_0011.png")
EN12 <- raster("EN_0012.png")
EN13 <- raster("EN_0013.png")

#cambio palette
cl <- colorRampPalette(c('red','orange','yellow'))(100) 
#SITUAZIONE INIZIALE
plot(EN01, col=cl)
#SITUAZIONE FINALE
plot(EN13, col=cl)

#GRAFICO CON ENTRAMBE LE IMMAGINI
par(mfrow=c(1,2))
plot(EN01, col=cl)
plot(EN13, col=cl)

dev.off()

#differenza EN01 e EN02
difno2 <- EN13 - EN01
cldif <- colorRampPalette(c('blue','black','yellow'))(100) 
plot(difno2, col=cldif)

par(mfrow=c(4,4))
plot(EN01, col=cl)
plot(EN02, col=cl)
plot(EN03, col=cl)
plot(EN04, col=cl)
plot(EN05, col=cl)
plot(EN06, col=cl)
plot(EN07, col=cl)
plot(EN08, col=cl)
plot(EN09, col=cl)
plot(EN10, col=cl)
plot(EN11, col=cl)
plot(EN12, col=cl)
plot(EN13, col=cl)

#plot (EN01, EN02, EN03, EN04, EN05, EN06, EN07, EN08, EN09, EN10, EN11, EN12, col=cl)

library(raster)

setwd("~/lab/esa_no2")
# put all files into the folder 

rlist=list.files(pattern=".png", full.names=T) 
#save raster into list
#TUTTI I FILE SALVATI IN FORMATO .png VERRANNO INSERITI NELLA LISTA

#PER EVITARE DI INSERIRE TUTTE LE IMMAGINI MANUALMENTE E' POSSIBILE UTILIZZARE LA FUNZIONE lapply()
#con lapply
list_rast=lapply(rlist, raster)

#con ciclo for
list_rast=list()
for(i in 1:length(rlist)){
  r=raster(rlist[[i]])
  list_rast[[i]]=r
}

#DAY 2

setwd("C:/lab")

load("NO2.RData")
#CARICARE IL FILE SALVATO IN PRECENDENZA

ls()

setwd("C:/lab/esa_no2")
#INDICARE LA CARTELLA SU CUI SI ANDRA' A LAVORARE

rlist <- list.files(pattern=".png")
#LA LISTA VERRA' CREATA CON TUTTI I FILES PRESENTI ALL'INTERNO DELLA CARTELLA esa_no2 CON CONFIGURAZIONE .png (pattern)
rlist
#FA VEDERE TUTTI I FILES .png

listafinale <- lapply(rlist, raster)
#lapply() SERVE AD APPLICARE UNA FUNZIONE SU UNA LISTA O UN VETTORE (UNA SERIE DI ELEMENTI)
#IN QUESTO CASO VIENE APPLICATO ALLA LISTA rlist LA FUNZIONE raster
#TUTTI I DATI VENGONO IMPORTATI INSIEME
#AL CONTRARIO DELLA FUNZIONE RASTER lapply UTILIZZA UN'INTERA LISTA DI DATI
#stack, crea un'immagine unica con tutte e 13 le immagini

EN <- stack(listafinale)

cl <- colorRampPalette(c('red','orange','yellow'))(100) 

plot(EN, col=cl)

#PASSAGGI
#1.CREARE UNA CARTELLA CON TUTTI I FILES
#2LISTA DI FILES
#3.IMPORTARE LA LISTA
#4.FARE UNO STACK DI TUTTE LE BANDE IMPORTATE IN UNA SINGOLA IMMAGINE

# DAY 2

library(raster)

setwd("C:/lab/esa_no2")

rlist <- list.files(pattern=".png")
listafinale <- lapply(rlist, raster)
EN <- stack(listafinale)
#LISTA DEI FILES

#DIFFERENZA PRIMA E ULTIMA IMMAGINE
difEN <- EN$EN_0013 - EN$EN_0001
cld <- colorRampPalette(c('blue','white','red'))(100) # 
plot(difEN, col=cld)

cl <- colorRampPalette(c('red','orange','yellow'))(100) #
plot(EN, col=cl)
 
#boxplot di EN
boxplot(EN)
#GRAFICO IN ORIZZONTALE
boxplot(EN,horizontal=T)
boxplot(EN, horizontal=T,outline=F)
#OUTLINE NON VENGONO PRESI IN CONSIDERAZIONE
boxplot(EN, horizontal=T,outline=F,axes=T)
#L'AZOTO RIMANE QUASI LO STESSO MA C'E' UN CAMBIAMENTO DEI MASSIMI VALORI      
