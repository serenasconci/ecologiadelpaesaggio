######R_code_snow.r

setwd("C:/lab")

install.packages("ncdf4") 
#QUESTO PACCHETTO PERMETTE DI VEDERE I DATI CON ESTENSIONE .nc
library(ncdf4)
library(raster)

#PER VISUALIZZARE IL FILE .nc VA PRIMA IMPORTATO
#LA FUNZIONE raster()IMPORTA UNA SINGOLA BANDA
#LA FUNZIONE brick() IMPORTA VARI LIVELLI CON DIVERSE BANDE, SI TROVA NEL PACCHETTO RASTER
snowmay <- raster("c_gls_SCE500_202005180000_CEURO_MODIS_V1.0.1.nc")

cl <- colorRampPalette(c('darkblue','blue','light blue'))(100)

#ESERCIZIO:plot snow cover vit the cl palette
cl <- colorRampPalette(c('darkblue','blue','light blue'))(100)
plot(snowmay, col=cl)

#import snow data
#VA CAMBIATA LA WORKING DIRECTORY PERHE' I FILES CHE DOBBIAMO UTILIZZARE SI TROVANO IN UNA NUOVA CARTELLA
#ALL'INTERNO DELLA CARTELLA LAB
setwd("C:/lab/snow")

#save raster into list con lapply
rlist <- list.files(pattern=".tif")
rlist
list_rast <- lapply(rlist, raster)
#lapply() IMPORTA UNA LISTA DI FILES INSIEME

snow.multitemp <- stack(list_rast)
plot(snow.multitemp,col=cl)
#stack() PERMETTE DI UNIRE TUTTI I FILES IN UN FILE UNICO 

#multitemp
par(mfrow=c(1,2))
plot(snow.multitemp$snow2000r, col=cl)
plot(snow.multitemp$snow2020r, col=cl)

#zlim
par(mfrow=c(1,2))
plot(snow.multitemp$snow2000r, col=cl, zlim=c(0,250))
plot(snow.multitemp$snow2020r, col=cl, zlim=c(0,250))

difsnow = snow.multitemp$snow2020r - snow.multitemp$snow2000r
difsnow = snow.multitemp$snow2020r - snow.multitemp$snow2000r
cldiff <- colorRampPalette(c('blue','white','red'))(100) 
plot(difsnow, col=cldiff)

#previsione
#IOL and download prediction.r into the folder snow
#source + NOME DELLO SCRIPT (PER CARICARE LO SCRIPT DALL'ESTERNO)
source("prediction.r")

predicted.snow.2025.norm <- raster("predicted.snow.2025.norm.tif")
plot(predicted.snow.2025.norm, col=cl)
 
 

