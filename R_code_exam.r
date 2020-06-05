###R_code_exam.r

#Copernicus data: https://land.copernicus.vgt.vito.be/PDF/portal/Application.html#Home

#1.R_code_primocod.r
#2.R_code_spatial.r
#3.R_code_spatial2.r
#4.R_code_point_patterns.r
#5.R_code_tereril.r
#6.R_code_landcover.r
#7.
#8.
#9.
#10.
#11. R_code_crop.r

###1. R_code_primocod.r
#PRIMO CODICE R ECOLOGIA DEL PAESAGGIO

#FUNZIONE PER INSTALLARE I PACCHETTI
install.packages("sp")
#LE VIRGOLETTE SERVONO PER USCIRE DAL SOFTWARE

#PER RICHIAMARE I PACCHETTI library() o require()
library(sp)
#require(sp) 

data(meuse)
#FUNZIONE CHE RICHIAMA I DATI DA USARE (data set)

head(meuse)
#FUNZIONE CHE PERMETTE DI VEDERE LE PRIME RIGHE DEL DATA SET

names(meuse)
#FUNZIONE CHE PERMETTE DI VISUALIZZARE IL NOME DELLE VARIABILI

summary(meuse)
#FUNZIONE CHE RESTITUISCE LE INFORMAZIONI PER IL DATA SET

pairs(meuse)
#FUNZIONE CHE PERMETTE DI CREARE UNA MATRICE DI GRAFICI A DISPERSIONE
pairs(~ cadmium + copper + lead , data = meuse)

#ESERCIZIO: aggiungere lo zinco
pairs(~ cadmium + copper + lead + zinc , data = meuse)

pairs(meuse[,3:6])
#IN QUESTO MODO VENGONO RICHIAMATE LE VARIABILI SENZA SCRIVERE I NOMI MA INDICANDO LE POSIZIONI

pairs(meuse[,3:6] col="red")
#PER IMPOSTARE IL COLORE SI UTILIZZA IL COMANDO col METTENDO TRA VIRGOLETTE IL COLORE CHE SI DESIDERA
#L'INSIEME DEI CARATTERI RACCHIUSI DA DOPPIE VIRGOLETTE CREA UN VETTORE STRINGA

pairs(meuse[,3:6] col="red" , pch=19)
#IL COMANDO pch SI UTILIZZA PER INSERIRE UN TIPO DI PUNTO NEL GRAFICO

pairs(meuse[,3:6], col="red", pch=19, cex=3,main="Primo pairs")
#cex (character exageration) COMANDO CHE REGOLA LE DIMENSIONI DEI CARATTERI
#cex < 1 piccole dimensioni, cex > 1 dimensioni maggiori
#IL COMANDO main PERMETTE DI INSERIRE IL TITOLO AL GRAFICO

panel.correlations <- function(x, y, digits=1, prefix="", cex.cor)
{
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(0, 1, 0, 1))
    r1=cor(x,y,use="pairwise.complete.obs")
    r <- abs(cor(x, y,use="pairwise.complete.obs"))

 

    txt <- format(c(r1, 0.123456789), digits=digits)[1]
    txt <- paste(prefix, txt, sep="")
    if(missing(cex.cor)) cex <- 0.9/strwidth(txt)
    text(0.5, 0.5, txt, cex = cex * r)
}

 

panel.smoothing <- function (x, y, col = par("col"), bg = NA, pch = par("pch"),
    cex = 1, col.smooth = "red", span = 2/3, iter = 3, ...)
{
    points(x, y, pch = pch, col = col, bg = bg, cex = cex)
    ok <- is.finite(x) & is.finite(y)
    if (any(ok))
        lines(stats::lowess(x[ok], y[ok], f = span, iter = iter),
            col = 1, ...)
}

 


panel.histograms <- function(x, ...)
{
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(usr[1:2], 0, 1.5) )
    h <- hist(x, plot = FALSE)
    breaks <- h$breaks; nB <- length(breaks)
    y <- h$counts; y <- y/max(y)
    rect(breaks[-nB], 0, breaks[-1], y, col="white", ...)
}

pairs(meuse[,3:6], lower.panel = panel.correlations, upper.panel = panel.smoothing, diag.panel = panel.histograms)

pairs(meuse[,3:6], lower.panel = panel.smoothing, upper.panel = panel.correlations, diag.panel = panel.histograms)

attach(meuse)
#IL DATABASE E' COLLEGATO AL PERCORSO DI RICERCA R, E' POSSIBILE ACCEDERE AGLI OGGETTI NEL DATABASE ASSEGNANDO SEMPLICEMENTE IL NOME
# AL POSTO DELLA FUNZIONE ATTACH SI PUO' ANCHE UTILIZZARE IL SIMBOLO $ 

#funzione plot(meuse$cadmium , meuse$copper)
plot(cadmium , copper)

plot(cadmium , copper , pch=18 , col="blue" , main="primo plot" , xlab="cadmio" , ylab="rame")

plot(cadmium , copper , pch=18 , col="blue" , main="primo plot" , xlab="cadmio" , ylab="rame" , cex.lab=2 , cex=2)

#############################################################################################################################
#############################################################################################################################
#############################################################################################################################

###2. R_code_spatial.r
#funzioni spaziali in Ecologia del paesaggio

library(sp)
#RICHIAMO IL PACCHETTO
 
data(meuse)
#PER RICHIAMARE I DATI

head(meuse)
#FA VEDERE LE PRIME SEI RIGHE
 
attach(meuse)
#IL DATABASE E' COLLEGATO AL PERCORSO DI RICERCA R 

plot(cadmium,lead,col="red",pch=19, cex=2)
#pch=point character  cex=character exageration

#ESERCIZIO: rame e zinco, carattere triangolo e colore verde
plot(copper,zinc, col="green",pch=17,cex=2)
 
plot(copper,zinc, col="green",pch=17,cex=2, xlab="rame",ylab="zinco")
#PER CAMBIARE LE ETICHETTE DEL GRAFICO SI USANO I COMANDI xlab e ylab
#IL NOME VA TRA VIRGOLETTE

#multipanel o multiframe
par(mfrow=c(1,2))
#LA FUNZIONE par() CREA UN MULTIPANEL, PIU' PLOT DI 2 O + IMMAGINI/GRAFICI
#row=riga
#DOPO LA FUNZIONE par VANNO INSERITI SUCCESSIVAMENTE I PLOT DEI DUE GRAFICI
plot(cadmium,lead,col="red",pch=19, cex=2)
plot(copper,zinc, col="green",pch=17,cex=2)

#invertiamo i grafici da riga/colonna a colonna/riga
par(mfrow=c(2,1))
plot(cadmium,lead,col="red",pch=19, cex=2)
plot(copper,zinc, col="green",pch=17,cex=2)

#multiframe automatico
install.packages("GGally")
library(GGally)

ggpairs(meuse[,3:6])
#LA FUNZIONE CREA UNA MATRICE DI GRAFICI CON UN DETERMINATO SET DI DATI

#Spatial

head(meuse)

coordinates(meuse)=~x+y
plot(meuse)

spplot(meuse,"zinc")
#spplot() serve per plottare i dati spazialmente

#VETTORE: QUANDO SI RIPETONO NUMERI O CARATTERI

#CRAN MIRROR: SOFTWARE UNICO CHE VIENE PROGETTATO IN TUTTO IL MONDO

##############################################################################################################################
##############################################################################################################################
##############################################################################################################################

###3. R_code_spatial2.r

library(sp)

#dati da usare
data(meuse)

head(meuse)

coordinates(meuse)=~x+y
#COORDINATE DEL DATA SET

spplot(meuse,"zinc")
#spplot dei dati di zinco

#ESERCIZIO:spplot dati di rame
head(meuse)
spplot(meuse,"copper")

bubble(meuse,"zinc")
#LA FUNZIONE bubble() E' UN ALTRO MODO PER PLOTTARE I DATI
#CREA UN GRAFICO A BOLLE DI DATI SPAZIALI

#ESERCIZIO: bubble del rame, colorato di rosso
bubble(meuse,"copper",col="red")

#esempio dati ottenuti da tesi
#foraminiferi (Sofia), carbon capture (Marco)
foram <- c(10, 20, 35, 55, 67, 80)
carbon <- c(5, 15, 30, 70, 85, 99)
plot(foram,carbon,col="green",cex=2,pch=19)

# <- PERMETTE DI DARE UN TITOLO 

#dati dall'esterno:covid-19

setwd("C:/lab") #Windows
#IL SET WORKING DIRECTORY SERVE AD INDICARE LA CARTELLA DOVE SI ANDRA' A LAVORARE
#IN QUESTO CASO VERRA' UTILIZZATA LA CARTELLA lab

covid <- read.table("covid_agg.csv",head=TRUE)
#LA FUNZIONE read.table(NOME DEL FILE, head=TRUE")PERMETTE DI LEGGERE UN FILE IN FORMATO TABELLA
#CREA UN FRAME DI DATI
#A QUESTA TABELLA E' STATO ASSEGNATO IL NOME covid

head(covid)

#######################################################################################################################
#######################################################################################################################
#######################################################################################################################

###4. R_code_point_patterns.r
#CODICE PER ANALISI DEI POINT PATTERNS

install.packages("ggplot2")
# per richiamare il pacchetto library() oppure require()
install.packages("spatstat")

#PACCHETTI UTILIZZATI
library(ggplot2)
library(spatstat)

setwd("C:/lab")

#PER IMPORTARE I DATI IN FORMATO TABELLA
covid <- read.table("covid_agg.csv", head=T)

head(covid)
#per visualizzare le prime righe

plot(covid$country,covid$cases)
#attach() in questo modo si evita il $.
attach(covid)
plot(country,cases)

plot(covid$country,covid$cases,las=0) #etichette parallele (labels)
plot(covid$country,covid$cases,las=1) #etichette orizzontali
plot(covid$country,covid$cases,las=2)#etichette perpendicolari all'asse
plot(covid$country,covid$cases,las=3) #etichette verticali
#las=n SERVE PER CAMBIARE IL MODELLO DI ETICHETTA

#cex:character exageration
plot(covid$country,covid$cases,las=3,cex.lab=0.5, cex.axis=0.5)
#cex.lab DEFINISCE LA DIMENSIONE DEL CARATTERE DELLE ETICHETTE
#cex.axis DEFINISCE LA DIMENSIONE DEL CARATTERE DEGLI ASSI 

#visualizzazione spaziale

#ggplot2
data(mpg)
head(mpg)

#ggplot di esempio
#data, aes (aestethics: VARIABILI CHE COMPORRANNO L'ESTETICA FINALE DEL GRAFICO), tipo di geometria 
ggplot(mpg,aes(x=displ,y=hwy))+geom_point()
#CAMBIO DI GEOMETRIA=PUNTI

ggplot(mpg,aes(x=displ,y=hwy))+geom_line()
#CAMBIO DI GEOMETRIA=LINEE

ggplot(mpg,aes(x=displ,y=hwy))+geom_polygon()
#CAMBIO DI GEOMETRIA=POLIGONI

#ggplot di covid
names(covid)
ggplot(covid,aes(x=lon,y=lat,size=cases))+geom_point()

#ESERCIZIO density: DENSITA' DI PUNTI PRESENTI IN UNA CERTA AREA
library(spatstat)
#creare data set per spatstat
attach(covid)
covids <- ppp(lon, lat, c(-180,180), c(-90,90))
#LA FUNZIONE ppp CREA UNA CLASSE, RAPPRESENTA UN SET DI DATI DEL MODELLO A PUNTI NEL PIANO BIDIMENSIONALE
#LON MAX E MIN (-180,180)
#LAT MAX E MIN (-90,90)
d <- density(covids)
plot(d)

#DAY 2

#PACCHETTI UTILIZZATI
install.packages("rgdal")
library(rgdal)
library(spatstat)

points(covids, pch=19)
plot(d)
points(covid)

#Save the .RData

setwd("C:/lab")

load(".RData")
#LA FUNZIONE load() SERVE PER CARICARE I POINT PATTERN IN RData

ls()
# PERMETTE DI VEDERE LA LISTA DEI VARI FILES DELL'RData

plot(d)

#PER CAMBIARE LA GAMMA DI COLORI VIENE USATA LA FUNZIONE colorRampPalette() 

cl <- colorRampPalette(c('yellow','orange','red')) (100)
#IN QUESTO CASO I MINIMI VALORI VERRANNO RAPPRESENTATI IN GIALLO MENTRE I MASSIMI VERRANNO RAPPRESENTATI IN ROSSO
#(100) INDICA IL NUMERO DELLE GRADAZIONI DI COLORI PRESENTI NELLA PALETTE
plot(d, col=cl)
#PLOT DELLA DENSITA' CON USO DELLA PALETTE DENOMINATA cl

#ESERCIZIO: plot della mappa della densità dal verde al blu.
cl <- colorRampPalette(c('green','yellow','blue')) (150)
plot(d,col=cl)

#PER AGGIUNGERE I PUNTI DEL COVID SI UTILIZZA LA FUNZIONE POINT()
points(covids)


library(rgdal)
#PERMETTE DI CARICARE DEI DATI DALL'ESTERNO

coastlines <- readOGR("ne_10m_coastline.shp")
# PER INSERIRE I CONFINI DEI VARI STATI

plot(coastlines, add=T)
#add AGGIUNGE AL PLOT PRECEDENTE UN NUOVO PLOT, IN QUESTO CASO LE COASTLINES 

#ESERCIZIO: plot della mappa di densità con una nuova colorazione ed aggiunta delle coastlines
plot(d)
cl <- colorRampPalette(c('yellow','orange','red')) (100)
plot(d,col=cl)
points(covids)
plot(coastlines, add=T)

# ESERCIZIO: Caricare il dato .RData workspace point_pattern.RData con la funzione load() e creare un grafico della mappa di densità.
library(spatstat)
library(rgdal) #per poter inserire le coastlines

setwd("~/lab/")
load("point_pattern2.RData")
ls()

cl5 <- colorRampPalette(c('blue', 'purple', 'red')) (200) 
plot(d, col=cl5, main="density")
points(covids)
coastlines <- readOGR("ne_10m_coastline.shp")
plot(coastlines, add=T)

#Interpolazione
head(covid)

View(covid)
#PER VISUALIZZARE L'INTERA TABELLA

marks(covids) <- covid$cases
#ESTRAE IL POINT PATTERN (CASES) E LO ASSOCIA A COVID 

#Smooth()FUNZIONE CHE PERMETTE DI OTTENERE UNA MAPPA CONTINUA DEI VARI PUNTI DI COVID IN BASE AI CASI
s <- Smooth(covids)
#s E' LA MAPPA DEI CASI
plot(s)

#ESERCIZIO:plot(s) with points and coastlines
cl5 <- colorRampPalette(c('cyan', 'purple', 'red')) (200) 
plot(s, col=cl5, main="estimate of cases")
points(covids)
coastlines <- readOGR("ne_10m_coastline.shp")
plot(coastlines, add=T)

dev.off()
#PER CHIUDERE IL GRAFICO

# mappa finale
par(mfrow=c(2,1))

# densità dei punti di covid
cl5 <- colorRampPalette(c('cyan', 'purple', 'red')) (200) 
plot(d, col=cl5, main="density")
points(covids)
coastlines <- readOGR("ne_10m_coastline.shp")
plot(coastlines, add=T)

# interpolazione del numero di casi di covid
cl5 <- colorRampPalette(c('cyan', 'purple', 'red')) (200) 
plot(s, col=cl5, main="estimate of cases")
points(covids)
coastlines <- readOGR("ne_10m_coastline.shp")
plot(coastlines, add=T)

 #LA MAGGIOR PARTE DEI CASI VENGONO PRESI IN SITU

###TESI San Marino

setwd("C:/lab")
load("Tesi.RData")
head(Tesi)

#plot della densità
library(spatstat)

attach(Tesi)
#SI ALLEGA LA TABELLA

summary(Tesi)
#FUNZIONE CHE FA VISUALIZZARE UN RIEPILOGO

#point pattern: x(lon), y(lat), c(xmin, xmax) , c(ymin, ymax)

#x varia da 12.42 a 12. 46
#y varia da 43.91 a 43.94

Tesippp <- ppp(Longitude, Latitude, c(12.41, 12.47), c(43.9,43.95))
#densità
dT <-  density(Tesippp)
plot(dT)
points(Tesippp, col="black")

# DAY 3

setwd("C:/lab/")
#LE VIRGOLETTE SERVONO A CARICARE FILES DALL'ESTERNO 
load("sanmarino.RData")
ls()
#dT:mappa di densità ; Tesi: dataset originale ; Tesippp: point pattern (Longitudine e Latitudine) 

#ASSOCIARE I VALORI CHE VOGLIAMO STIMARE NELLO SPAZIO (interpolazione)

library(spatstat)
#required(): MODALITA' ALTERNATIVA DI RICHIAMARE UN PACCHETTO

plot(dT)
points(Tesippp, col="green")
#LA DENSITA' E' DIRETTAMENTE PROPORZIONATA AI PRATI ARIDI

head(Tesi)

marks(Tesippp) <- Tesi$Species_richness
#marks() ASSOCIA I VALORI DELLA VARIABILE AL POINT PATTERN
#PRENDERE I SINGOLI PUNTI DI CAMPIONAMENTOED ASSOCIARLI AI SINGOLI PUNTI DEL POINT PATTERN
#IN QUESTO CASO SPECIES RICHNESS

#PER VEDERE L'UTILIZZO DI UNA SINGOLA FUNZIONE VA INSERITO SU R IL NOME DELLA FUNZIONE 

#interpolazione
interpol <- Smooth(Tesippp)
#plottare la mappa ed inserire i punti
plot(interpol)
points(Tesippp, col="green")

#I VALORI DI RICCHEZZA SPECIFICA PIU' BASSI SONO NELLA PARTE A NORD E SUD-OVEST DEL DATA SET
#I VALORI PIU' ALTI SONO AD OVEST E SUD-EST

library(rgdal) # per leggere i file vettoriali
sanmarino <- readOGR("San_Marino.shp")

plot(sanmarino)

#PER AGGIUNGERE LA MAPPA interpol VA AGGIUNTO add=TRUE
plot(interpol, add=TRUE)
#PER AGGIUNGERE I PUNTI
points(Tesippp,col="green")

plot(sanmarino, add=TRUE)

#ESERCIZIO:plot multiframe di densità e interpolazione
#par() CREA UN MULTIFRAME
par(mfrow=c(2,1))
#densità
plot(dT,main="Density of points")
points(Tesippp, col="black")
#interpolazione
plot(interpol, main="Estimate of species richness")
points(Tesippp, col="black")

#main= INSERISCE IL TITOLO AL GRAFICO

#ESERCIZIO:plot multiframe di densità e interpolazione con due colonne ed una riga 
par(mfrow=c(1,2))
#densità
plot(dT,main="Density of points")
points(Tesippp, col="black")
#interpolazione
plot(interpol, main="Estimate of species richness")
points(Tesippp, col="black")

###########################################################################################################################
###########################################################################################################################
###########################################################################################################################

###5. R_code_tereril.r
#codice R per le analisi di immagini satellitari

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

###########################################################################################################################
###########################################################################################################################
###########################################################################################################################

###6. R_code_landcover.r

setwd("C:/lab")

library(raster)

#brick()FUNZIONE CHE PERMETTE DI IMPORTARE UN'IMMAGINE SATELLITARE CON TUTTI I DATI

p224r63_2011 <- brick("p224r63_2011_masked.grd")

install.packages("RStoolbox")

library(RStoolbox)

#landsat bands 1b, 2g, 3r, 4nir

#rgb 
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin")

p224r63_2011c <- unsuperClass(p224r63_2011, nClasses=4)
#unsuperClass() CLASSIFICAZIONE SENZA SUPERVISIONE
#IMMAGINE PIU' NUMERO DI CLASSI DESIDERATE

p224r63_2011
# INSERENDO IL NOME DELL'IMMAGINE p224r63_2011c SI OTTENGONO TUTTE LE INFORMAZIONI

plot(p224r63_2011c$map)
#$ SIMBOLO ALTERNATIVO ALLA FUNZIONE ATTACH
#MAP E' LA MAPPA CHE IL PLOT HA GENERATO

#cambiare i colori della mappa 
clclass <- colorRampPalette(c('green', 'red', 'blue', 'black'))(100) 
plot(p224r63_2011c$map, col=clclass)
#in funzione del numero di classi, aumenta l'incertezza dell' algoritmo automatico di classificazione
#riportando potenzialmente classi leggermente differenti

p224r63_2011c <- unsuperClass(p224r63_2011, nClasses=4)
plot(p224r63_2011c$map)

#DIMINUIAMO IL NUMERO DI CLASSI (2)
p224r63_2011c <- unsuperClass(p224r63_2011, nClasses=2)
plot(p224r63_2011c$map)

###########################################################################################################################
###########################################################################################################################
###########################################################################################################################

###7. 


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











 
