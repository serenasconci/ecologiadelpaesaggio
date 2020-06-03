###R_code_exam.r

#Copernicus data: https://land.copernicus.vgt.vito.be/PDF/portal/Application.html#Home

#1.R_code_first.r
#2.R_code_spatial.r
#3.R_code_spatial2.r
#4.
#5.
#6.
#7.
#8.
#9.
#10.
#11. R_code_crop.r


### 11. R code crop

#crop sui dati della criosfera
#serie di immagini 
#stack funzione che va ad applicare la funzione di riferimento, ad esempio raster ad un intera lista di file

library(raster)

setwd("C:/lab/snow")

#ESERCIZIO: Upload the whole snow set (2000, 2005, 2010, 2015, 2020)

rlist <- list.files(pattern="snow")
rlist 

#lista file da poter inserire all'interno di R
#invece del pattern .tif mettiamo snow per escludere il file della prediction

list_rast <- lapply(rlist, raster)
snow.multitemp <- stack(list_rast)
#in questo modo abbiamo importato tutti i file
clb <- colorRampPalette(c('dark blue','blue','light blue'))(100) # 
plot(snow.multitemp,col=clb)

#funzione zoom(nome immagine e estensione)
#definizione dell'estenzione
#zoom è una funzione di raster

#il dollaro lega il file interno di snow.multitemp
plot(snow.multitemp$snow2010r, col=clb)

#nuova estensione
#l'italia cade circa tra i 40 e i 50 gradi latitudine
#longitudine 6 18 gradi

extension <- c(6, 18, 40, 50)
zoom(snow.multitemp$snow2010r, ext=extension)

#non è precisa quindi va cambiato
extension <- c(6, 18, 35, 50)
zoom(snow.multitemp$snow2010r, ext=extension)

#altro accorgimento
extension <- c(6, 20, 35, 50)
zoom(snow.multitemp$snow2010r, ext=extension)

#ora viene presa tutta l'italia 
plot(snow.multitemp$snow2010r, col=clb)
 
#zoom (nome immagine ed estensione)
zoom(snow.multitemp$snow2010r, ext=drawExtent())


#crop
#in crop non va dichiarato l'extent
#la differenza con zoom è che va messo un argomento a fnzione 
#cn crop no ext=, basta mettere l'immagine e l'estensione che si va ad utilizzare
extension <- c(6, 20, 35, 50)
snow2010r.italy <- crop(snow.multitemp$snow2010r, extension)
plot(snow2010r.italy, col=clb)

#stack serie multitemporale che abbiamo creato in precedenza con lapply

#crop di un intero stack
#ESERCIZIO: crop the Italy extent on the whole stack of snow layers
#partire dallo stack e creare un crop
snow.multitemp.italy <- crop(snow.multitemp, extension)
 plot(snow.multitemp.italy, col=clb)


#creare una legenda uguale in modo tale da poterle mettere a confronto
#cambio del range
#tutti i valori minimi sono 20 mentre invece il massimo è 195
plot(snow.multitemp.italy, col=clb, zlim=c(20,200))
#zlim serve per definire la variazione
#variazione da 20 a 200
#boxplot,parte orizzontale, gli outlaiers=falso valori molto esterni
boxplot(snow.multitemp.italy, horizontal=T,outline=F)
#molta meno copertura nevosa perche il valore massimo che è molto alto nel 2000 è molto più basso nel 2020











 
