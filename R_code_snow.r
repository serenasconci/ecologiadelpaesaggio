######R_code_snow.r

setwd("C:/lab")

install.packages("ncdf4") #permette di vedere i dati con estensione.nc
library(ncdf4)
library(raster)

# per visualizzare il file .nc va prima importato
#raster importa un singolo livello, una singola banda 
# brick importa vari livelli con diverse bande, si trova nel pacchetto raster
snowmay <- raster("c_gls_SCE500_202005180000_CEURO_MODIS_V1.0.1.nc")

cl <- colorRampPalette(c('darkblue','blue','light blue'))(100)

#ESERCIZIO:plot snow cover vit the cl palette
cl <- colorRampPalette(c('darkblue','blue','light blue'))(100)
plot(snowmay, col=cl)

#import snow data
#va cambiata la working directory perchè i files si trovano in una nuova cartella all'interno di lab
setwd("C:/lab/snow")

#save raster into list con lapply
#lapply importa una lista di files insieme 
    
rlist <- list.files(pattern=".tif")
rlist
list_rast <- lapply(rlist, raster)

snow.multitemp <- stack(list_rast)
plot(snow.multitemp,col=cl)
 
#stack è l'insieme di tutti i files#stack è l'insieme di tutti i files

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
#source + nome dello script, per caricare lo script dall'esterno
source("prediction.r")

predicted.snow.2025.norm <- raster("predicted.snow.2025.norm.tif")
plot(predicted.snow.2025.norm, col=cl)
 
 

