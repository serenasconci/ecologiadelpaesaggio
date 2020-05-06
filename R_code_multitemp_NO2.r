### R code for analysing NO2 data from ESA - January to March 2020

library(raster)

setwd("C:/lab")

#raster() caricare l'immagine con una sola banda, al contrario di brick() che carica tutte le bande
EN01 <- raster("EN_0001.png")
plot(EN01)

Esercizio: import all the images 
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
plot(EN01, col=cl)
#situazione finale
plot(EN13, col=cl)

#grafico di entrambe le immagini
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

#plot ( EN01, EN02, EN03, EN04, EN05, EN06, EN07, EN08, EN09, EN10, EN11, EN12, col=cl)

library(raster)

setwd("~/lab/esa_no2")
# put all files into the folder 

rlist=list.files(pattern=".png", full.names=T) 

#save raster into list

#con lapply
list_rast=lapply(rlist, raster)

#con ciclo for
list_rast=list()
for(i in 1:length(rlist)){
  r=raster(rlist[[i]])
  list_rast[[i]]=r
}










