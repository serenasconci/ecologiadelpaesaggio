###R_code_exam.r

#Copernicus data: https://land.copernicus.vgt.vito.be/PDF/portal/Application.html#Home

#1.R_code_primocod.r
#2.R_code_spatial.r
#3.R_code_spatial2.r
#4.R_code_point_patterns.r
#5.R_code_tereril.r
#6.R_code_landcover.r
#7.R_code_multitemp.r
#8.R_code_multitemp_NO2.r
#9.R_code_snow.r
#10.R_code_patches.r
#11.R_code_crop.r
#12. Species Distribution Modelling
#13. Exam

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
#FUNZIONE CHE PERMETTE DI VEDERE LE PRIME SEI RIGHE DEL DATA SET

names(meuse)
#FUNZIONE CHE PERMETTE DI VISUALIZZARE IL NOME DELLE VARIABILI

summary(meuse)
#FUNZIONE CHE RESTITUISCE LE INFORMAZIONI PER IL DATA SET

pairs(meuse)
#FUNZIONE CHE PERMETTE DI CREARE UN PLOT DI TUTTE LE VARIABILI IN UN DATASET
pairs(~ cadmium + copper + lead , data = meuse)

#esercizio: aggiungere lo zinco
pairs(~ cadmium + copper + lead + zinc , data = meuse)

pairs(meuse[,3:6])
#IN QUESTO MODO VENGONO RICHIAMATE LE VARIABILI SENZA SCRIVERE I NOMI MA INDICANDO LE POSIZIONI
#LE PARENTESI QUADRE SERVONO PER CREARE UN SUBSET
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
# AL POSTO DELLA FUNZIONE ATTACH SI PUO' ANCHE UTILIZZARE IL SIMBOLO $ CHE LEGA LA VARIABILEAL DATASET
#attach(): FUNZIONA CON TUTTI I PACCHETTI

#funzione plot(meuse$cadmium , meuse$copper)
plot(cadmium , copper)

plot(cadmium , copper , pch=18 , col="blue" , main="primo plot" , xlab="cadmio" , ylab="rame")
#xlab,DA IL TITOLO ALL'ASSE DELLE ASCISSE MENTRE ylabLO DA ALL'ASSE DELLE ORDINATE
plot(cadmium , copper , pch=18 , col="blue" , main="primo plot" , xlab="cadmio" , ylab="rame" , cex.lab=2 , cex=2)

#############################################################################################################################
#############################################################################################################################
#############################################################################################################################

###2. R_code_spatial.r
#funzioni spaziali in Ecologia del paesaggio

library(sp)
#RICHIAMO IL PACCHETTO sp
 
data(meuse)
#PER RICHIAMARE I DATI

head(meuse)
#FA VEDERE LE PRIME SEI RIGHE
 
attach(meuse)
#IL DATABASE E' COLLEGATO AL PERCORSO DI RICERCA R 

plot(cadmium,lead,col="red",pch=19, cex=2)
#pch=point character  cex=character exageration

#esercizio: rame e zinco, carattere triangolo e colore verde
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

#esercizio:spplot dati di rame
head(meuse)
spplot(meuse,"copper")

bubble(meuse,"zinc")
#LA FUNZIONE bubble() E' UN ALTRO MODO PER PLOTTARE I DATI
#CREA UN GRAFICO A BOLLE DI DATI SPAZIALI

#esercizio: bubble del rame, colorato di rosso
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
#PRIME SEI RIGHE DELLA TABELLA

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
#CARICO I DATI
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
#VISUALIZZA I NOMI DELLE VARIABILI
ggplot(covid,aes(x=lon,y=lat,size=cases))+geom_point()

#esercizio density: DENSITA' DI PUNTI PRESENTI IN UNA CERTA AREA
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
#PLOTTAGGIO DEI PUNTI

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

#esercizio: plot della mappa della densità dal verde al blu.
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

#esercizio: plot della mappa di densità con una nuova colorazione ed aggiunta delle coastlines
plot(d)
cl <- colorRampPalette(c('yellow','orange','red')) (100)
plot(d,col=cl)
points(covids)
plot(coastlines, add=T)

#esercizio: Caricare il dato .RData workspace point_pattern.RData con la funzione load() e creare un grafico della mappa di densità.
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
#ESTRAEI DATI ASSOCIATI AL DATASET
#Smooth()FUNZIONE CHE PERMETTE DI OTTENERE UNA MAPPA CONTINUA DEI VARI PUNTI DI COVID IN BASE AI CASI
s <- Smooth(covids)
#s E' LA MAPPA DEI CASI
plot(s)

#esercizio:plot(s) with points and coastlines
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

#esercizio:plot multiframe di densità e interpolazione
#par() CREA UN MULTIFRAME
par(mfrow=c(2,1))
#densità
plot(dT,main="Density of points")
points(Tesippp, col="black")
#interpolazione
plot(interpol, main="Estimate of species richness")
points(Tesippp, col="black")

#main= INSERISCE IL TITOLO AL GRAFICO

#esercizio:plot multiframe di densità e interpolazione con due colonne ed una riga 
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
#LEGENDA STANDARD
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

#esercizio: inserire 5 microclassi, (L'IMMAGINE CON 5 MICROCLASSI SARA' PIU' SGRANATA
cllow <- colorRampPalette(c('black','grey','light grey'))(5) 
plot(p224r63_2011, col=cllow)

#esercizio: gamma di colore in 'blue'
names(p224r63_2011)
#SI OTTENGONO I NOMI DELLE BANDE: "B1_sre" "B2_sre" "B3_sre" "B4_sre" "B5_sre" "B6_bt"  "B7_sre"
clb <- colorRampPalette(c('dark blue','blue','light blue'))(100) 
plot(p224r63_2011$B1_sre, col=clb)
#attach() NON FUNZIONA CON IL PACCHETTO RASTER
#IL SIMBOLO CHE LEGA LA COLONNA(banda) AL DATA SET (immagine satellitare)E' IL $

#esercizio: plottare la banda dell'infrasosso vicino (nir) con colorRampPalette in rosso, arancione e giallo.
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

#esercizio: nir nella componente G(green). In precedenza era stato montato in red.
plotRGB(p224r63_2011, r=3, g=4, b=2, stretch="Lin")
#LA VEGETAZIONE DIVENTA VERDE
#LA ZONA AGRICOLA E' RAPPRESENTATA IN VIOLA

#esercizio: nir nella componente B(blu)
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
#esercizio: plotta l'immagine usando nir nella componente "r" in RGB
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
#multiframe= 1988rgb, 2011rgb e difdiv, TRE RIGHE E UNA COLONNA
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
#PACCHETTO CHE CI PERMETTE DI FARE UN'ANALISI DI LAND COVER
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
#PIU' DIMINUISCO IL NUMERO DELLE CLASSI PIU' L'ERRORE AUMENTA

###########################################################################################################################
###########################################################################################################################
###########################################################################################################################

###7.R_code_multitemp.r

#ANALISI TEMPORALE DELLA VARIAZIONE DELLA LAND COVER

setwd("C:/lab/")

library(raster)

#brick()CARICA I DATI DALL'ESTERNO, NEL CASO DI UN'IMMAGINE SATELLITARE CARICA TUTTE LE BANDE (FUNZIONE DI RASTER) 
defor1 <- brick("defor1_.jpg")
defor2 <- brick("defor2_.jpg")

defor1
#sono presenti tre bande
#names: defor1_.1,defor1_.2, defor1_.3 (nir; rosso; verde)
#defor1_.1 = NIR
#defor1_.2 = ROSSO
#defor1_.3 =VERDE

plotRGB(defor1, r=1, g=2, b=3, stretch="Lin")

#esercizio:plot di defor2
plotRGB(defor2, r=1, g=2,b=3, stretch="Lin")

#CARICARE ENTRAMBE LE IMMAGINI CON LA FUNZIONE par()
par(mfrow=c(2,1))
plotRGB(defor1, r=1, g=2, b=3, stretch="Lin")
plotRGB(defor2, r=1, g=2,b=3, stretch="Lin")

#MEDIANTE LA FUNZIONE unsuperClass() ANDIAMO A CREARE DUE CLASSI (classifcazione non supervisionata)
#E' NECESSARIO IL PACCHETTO RStoolbox
library(RStoolbox)

d1c <- unsuperClass(defor1, nClasses=2)
#IMMAGINE ORIGINALE PIU' IL NUMERO DI CLASSI DESIDERATE
 
plot(d1c$map)
#CAMBIO DI PALETTE
cl <- colorRampPalette(c('black','green'))(100)
plot(d1c$map, col=cl)

#esempio sul significato del $
#mappageologica <- geomap(im_sat, nClasses=...)
#plot(mappageologica$lito)
#plot(mappageologica$lineaments)

#esercizio: classificazione immagine satellitare defor2 con due classi
d2c <- unsuperClass(defor2, nClasses=2)
plot(d2c$map, col=cl)
#CLASSE 1 NON FORESTA
#CLASSE2 FORESTA

dev.off()

#plot delle de mappe ottenute
par(mfrow=c(1,2))
plot(d1c$map, col=cl)
plot(d2c$map, col=cl)
#INVERSIONE NUMERO DI RIGHE E NUMERO DI COLONNE
par(mfrow=c(2,1))
plot(d1c$map, col=cl)
plot(d2c$map, col=cl)

#STIMA DEI VALORI VARIABILI
#MISURARE LA FREQUENZA DELLE DUE CLASSI NELLA PRIMA MAPPA defor1
freq(d1c$map)
#aree aperte=34181
#foresta=307111

totd1 <- 34181 + 307111
#totale=341292

#calcolare la percentuale 
percent1 <- freq(d1c$map) * 100 / totd1
percent1
#89.9 per le foreste
#10.1 per le aree aperte

#frequeza mappa defor2
freq(d2c$map)
#aree aperte=164170
#foresta=178556

totd2 <- 178556 + 164170
#totd2=342726

#percentuale defor2
#aree aperte= 47.9
#foreste= 52.1

#PLOT FINALE CON DATI OTTENUTI
cover <- c("Agriculture", "Forest")
before <- c(10.1, 89.9)
after <- c(47.9,52.1)

#dataframe finale
output <-data.frame(cover, before, after)
#crea una tabella con i dati 

View(output)
#PER VISUALIZZARE LA TABELLA

#DAY2

setwd("C:/lab")

load("defor.RData")

ls()

library(raster)

#MAPPA MULTITEMPORALE
par(mfrow=c(1,2))
cl <- colorRampPalette(c('black','green'))(100) # 
plot(d1c$map, col=cl)
plot(d2c$map, col=cl)

library(ggplot2)
#grafico
#histograms of the % cover before deforestation
ggplot(output, aes(x=cover, y=before, color=cover)) +
geom_bar(stat="identity", fill="white")
#identity: VENGONO PRESI DIRETTAMENTE I VALORI DI COPERTURAsi 

#esercizio: % cover after deforestation
ggplot(output, aes(x=cover, y=after, color=cover)) +
geom_bar(stat="identity", fill="white")

#plot di entrambi gli istogrammi
install.packages("gridExtra")
library(gridExtra) #require()

#esercizio: use grid.arrange to plot the two graphs
#grid.arrange PRENDE I VARI PLOT E LI METTE INSIEME IN UN UNICO GRAFICO, HA LA STESSA FUNZIONE DELLA FUNZIONE par()
grafico1 <- ggplot(output, aes(x=cover, y=before, color=cover)) + 
geom_bar(stat="identity", fill="white")

grafico2 <- ggplot(output, aes(x=cover, y=after, color=cover)) + 
geom_bar(stat="identity", fill="white")

grid.arrange(grafico1, grafico2, nrow=1)
#grid.arrange(plot1, plot2, nrow=1)

#DAY 3

library(ggplot2)

cover <- c("Agriculture","Forest")
before <- c(10.9,89.1)
after <- c(48.2,51.8)

output <- data.frame(cover,before,after)
output

library(gridExtra) # require()

grafico1 <- ggplot(output, aes(x=cover, y=before, color=cover)) +
geom_bar(stat="identity", fill="white")

grafico2 <- ggplot(output, aes(x=cover, y=after, color=cover)) +
geom_bar(stat="identity", fill="white")

# mette insieme i due grafici
grid.arrange(grafico1, grafico2, nrow = 1)

grafico1 <- ggplot(output, aes(x=cover, y=before, color=cover)) + 
geom_bar(stat="identity", fill="white") +
ylim(0, 100)
#ylim() IMPOSTA I LIMITI DELL'ASSE Y
grafico2 <- ggplot(output, aes(x=cover, y=after, color=cover)) + 
geom_bar(stat="identity", fill="white") +
ylim(0, 100)
#ylim 100 I GRAFICI SONO PIU' CONFRONTABILI

# Exercise: use grid.arrange to plot the two graphs 
grid.arrange(grafico1, grafico2, nrow = 1)

##################################################################################################################################
##################################################################################################################################
##################################################################################################################################

###8. R_code_multitemp_NO2.r

#R code for analysing NO2 data from ESA - January to March 2020

library(raster)

setwd("C:/lab")

EN01 <- raster("EN_0001.png")
#raster()FUNZIONE CHE PERMETTE DI CARICARE UN'IMMAGINE CON UNA SOLA BANDA
#FUNZIONE CONTRARIA A brick() CHE CARICA TUTTE LE BANDE
plot(EN01)
#SITUAZIONE DELL'AZOTO A GENNAIO

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
#DUE METODI PER PLOTTARE LE MAPPE
#METODO 1, IL PIU' LENTO
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
#METODO 2, PIU' RAPIDO
#EN<- stack(EN01, EN02, EN03, EN04, EN05, EN06, EN07, EN08, EN09, EN10, EN11, EN12, EN13)
#CON LO STACK LE TREDICI IMMAGINI SONO RACCHIUSE IN UN'UNICA IMMAGINE

#DAY2

library(raster)

load("EN.RData")

setwd("~/lab/esa_no2")
# put all files into the folder 


rlist=list.files(pattern=".png", full.names=T) 
#save raster into list
#TUTTI I FILE SALVATI IN FORMATO .png VERRANNO INSERITI NELLA LISTA

#PER EVITARE DI INSERIRE TUTTE LE IMMAGINI MANUALMENTE E' POSSIBILE UTILIZZARE LA FUNZIONE lapply()
#con lapply
list_rast=lapply(rlist, raster)
#APPLICA UNA FUNZIONE A TUTTI I FILES NELLO STESSO MOMENTO

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
#CREA UN GRAFICO DOVE SI VEDE LA DISTRIBUZIONE
boxplot(EN)
#GRAFICO IN ORIZZONTALE
boxplot(EN,horizontal=T)
boxplot(EN, horizontal=T,outline=F)
#OUTLINE NON VENGONO PRESI IN CONSIDERAZIONE
boxplot(EN, horizontal=T,outline=F,axes=T)
#L'AZOTO RIMANE QUASI LO STESSO MA C'E' UN CAMBIAMENTO DEI MASSIMI VALORI      

##############################################################################################################################
##############################################################################################################################
##############################################################################################################################

###9. R_code_snow.r

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

#esercizio:plot snow cover vit the cl palette
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
#INDICA I LIMITI MIN E MAX
#IN QUESTO MODO UNIFICHIAMO IL LIMITE TRA LE DUE IMMAGINI
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

############################################################################################################################
############################################################################################################################
############################################################################################################################

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

#esercizio: plottare le mappe una accanto all'altra
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

#############################################################################################################################
#############################################################################################################################
#############################################################################################################################

### 11. R_code_crop.r

#crop DEI DATI DELLA CRIOSFERA
#serie di immagini 
#stack()FUNZIONE CHE VA AD APPLICARE LA FUNZIONE DI RIFERIMENTO(ESEMPIO: RASTER IN UN INTERA LISTA DI FILES)

library(raster)
library(ncdf4)
#SERVE PER UTILIZZARE IL DATO CON PATTERN .nc
setwd("C:/lab/snow")

#esercizio: Upload the whole snow set (2000, 2005, 2010, 2015, 2020)

rlist <- list.files(pattern="snow")
rlist 
#CREARE UNA LISTA DI FILES DA POTER INSERIRE ALL'INTERNO DI R
#INVECE DI PATTERN .tif METTIAMO snow PER ESCLUDERE IL FILE DELLA PREDICTION

list_rast <- lapply(rlist, raster)
snow.multitemp <- stack(list_rast)
#TUTTI I FILE SONO STATI IMPORTATI
clb <- colorRampPalette(c('dark blue','blue','light blue'))(100) # 
plot(snow.multitemp,col=clb)

#zoom E' UNA FUNZIONE DI RASTER
#zoom(NOME IMMAGINE, ESTENSIONE)

#IL DOLLARO LEGA IL FILE INTERNO DI snow.multitemp 
plot(snow.multitemp$snow2010r, col=clb)

#NUOVA ESTENSIONE
#L'ITALIA RICADE CIRCA TRA I 40 E I 50 GRADI LAT
#LONGITUDINE TRA I 6 E I 18 GRADI
extension <- c(6, 18, 40, 50)
zoom(snow.multitemp$snow2010r, ext=extension)

#CAMBIO DI ESTENSIONE PER ESSERE PIU' PRECISI
extension <- c(6, 18, 35, 50)
zoom(snow.multitemp$snow2010r, ext=extension)

#ALTRA PICCOLA MODIFICA ALL'ESTENSIONE
extension <- c(6, 20, 35, 50)
zoom(snow.multitemp$snow2010r, ext=extension)
plot(snow.multitemp$snow2010r, col=clb)
 
#zoom (nome immagine ed estensione)
zoom(snow.multitemp$snow2010r, ext=drawExtent())
#clic per indicare il punto che si vuole studiare e poi cliccare di nuovo

#crop
#NON VA DICHIARATO EXTENT
#LA DIFFERENZA CON zoom E' CHE VA MESSO UN ARGOMENTO A FUNZIONE
#CON CROP NO ext=, basta mettere l'immagine e l'estensione che si va ad utilizzare
extension <- c(6, 20, 35, 50)
snow2010r.italy <- crop(snow.multitemp$snow2010r, extension)
plot(snow2010r.italy, col=clb)

#stack() SERIE MULTITEMPORALE CHE ABBIAMO CREATO IN PRECEDENZA CON lapply

#crop di un intero stack
#esercizio: crop the Italy extent on the whole stack of snow layers
#partire dallo stack e creare un crop
snow.multitemp.italy <- crop(snow.multitemp, extension)
plot(snow.multitemp.italy, col=clb)

#VA CREATA UNA LEGENDA UGUALE IN MODO TALE DA POTERLE METTERE A CONFRONTO
#CAMBIO DEL RANGE
#TUTTI I VALORI MINIMI SONO 20 MENTRE IL MASSIMO E' 195
plot(snow.multitemp.italy, col=clb, zlim=c(20,200))
#zlim() SERVE PER DEFINIRE LA VARIAZIONE
#VARIAZIONE DA 20 E 200

boxplot(snow.multitemp.italy, horizontal=T,outline=F)
#boxplot,parte orizzontale, gli outlaiers=falso (VALORI MOLTO ESTERNI)
#MOLTA MENO COPERTURA NEVOSA PERCHE' IL VALORE MAX CHE E' MOLTO ALTO NEL 2000 E MOLTO PIU' BASSO NEL 2020

################################################################################################################################
################################################################################################################################
################################################################################################################################

### 12. Species Distribution Modelling (SDM)

#NON SERVE IL SETTAGGIO DELLA CARTELLA PERCHE' USIAMO I DAI NEL PACCHETTO
install.packages("sdm")
library(sdm)
library(raster)
library(rgdal) #GESTISCE AL MEGLIO I DATI RASTER E VETTORIALI
#IN QUESTO CASO DATI VETTORIALI

#file vettoriali: coordinate x,y 
#PUNTI-LINEE-POLIGONI

#CARICARE IL FILE
file <- system.file("external/species.shp", package="sdm")
#SUCCESSIVAMENTE CARICARE LA PARTE GRAFICA E LE INFORMAZIONI CORRELATE AI VARI PUNTI
species <- shapefile(file)

species
#VALORI DEL FILE
#CI TROVIAMO CIRCA NEL FUSO NUMERO 30 (tra spagna e francia)
#L'ITALIA RICADE NEI FUSI 32 e 33
#occurrence: UNICA INFORMAZIONE, SE LA SPECIE E' PRESENTE O MENO

species$occurrence
# [1] 1 0 1 1 1 0 0 1 1 1 1 1 1 0 1 1 0 1 1 0 0 1 0 1 1 0 1 0 1 0 1 0 1 1 1 1 0
#[38] 1 0 0 0 0 0 0 0 1 0 0 1 0 1 0 0 0 0 0 1 1 1 1 0 0 1 0 1 0 1 1 1 1 0 0 0 0
#[75] 0 1 0 0 1 0 1 0 1 1 1 0 0 1 1 0 0 1 1 1 1 0 0 0 0 0 0 0 1 1 1 0 0 1 1 0 0
#[112] 0 1 0 0 1 1 1 1 1 0 0 0 1 1 0 0 1 1 1 1 1 0 0 0 1 0 0 1 1 0 1 0 1 0 0 1 1
#[149] 0 0 1 0 0 1 1 0 0 0 0 1 1 1 0 0 0 0 1 0 0 1 0 1 0 0 0 0 1 0 1 0 1 0 1 0 0
#[186] 0 0 1 1 0 1 0 1 1 0 1 0 0 0 0

plot(species)

#MODIFICARE LE PRESENZE DALLE ASSENZE

plot(species[species$Occurrence == 1,],col='blue',pch=16)
#PLOT DEL DATA SET, PER TUTTE LE OCCORRENDE DI PRESENZA, QUINDI=1
#LA VIRGOLA INTERROMPE LA MISURAZIONE COLORE BLU E TIPO DI PUNTO 16
points(species[species$Occurrence == 0,],col='red',pch=16)
#POINTS VIENE USATO PER AGGIUNGERE LE INFORMAZIONI ALLA FUNZIONE PRECEDENTE

#variabili ambientali
#esempio: temperatura
path <- system.file("external", package="sdm") 
#all'interno del pacchetto sdm è presente la cartella external

#pattern: asc (file ascii)
lst <- list.files(path=path,pattern='asc$',full.names = T) #
#LE VARIABILI AMBIENTALI CHE SERVONO A PREVEDERE LA PRESENZA DELLA SPECIE
lst
#[1] "C:/Users/Serena/Documents/R/win-library/3.6/sdm/external/elevation.asc"    
#[2] "C:/Users/Serena/Documents/R/win-library/3.6/sdm/external/precipitation.asc"
#[3] "C:/Users/Serena/Documents/R/win-library/3.6/sdm/external/temperature.asc"  
#[4] "C:/Users/Serena/Documents/R/win-library/3.6/sdm/external/vegetation.asc" 

#stack delle variabili
#UNIAMO LA QUOTA, LE PRECIPITAZIONI,LA TEMPERATURA E LA VEGETAZIONE IN UN SOLO BLOCCO
cl <- colorRampPalette(c('blue','orange','red','yellow')) (100)
plot(preds, col=cl)

plot(preds$elevation, col=cl)

#UNIAMO AL PLOT I PUNTI CHE HANNO OCCORRENZE 0 A 1

points(species[species$Occurrence == 1,], pch=16)
#LA SPECIE PREDILIGE UNA BASSA QUOTA

plot(preds$temperature, col=cl)
points(species[species$Occurrence == 1,], pch=16)
#LA SPECIE PREDILIGE UNA TEMPERATURA MEDIO-ALTA

plot(preds$precipitation, col=cl)
points(species[species$Occurrence == 1,], pch=16)
#LA SPECIE PREDILIGE UNA SITUAZIONE INTERMEDIA

plot(preds$vegetation, col=cl)
points(species[species$Occurrence == 1,], pch=16)
#LA SPECIE PREDILIGE L'OMBRA, UN'ELEVATA VEGETAZIONE

#glm 

#model
d <- sdmData(train=species, predictors=preds)

#class                                 : sdmdata 
#=========================================================== 
#number of species                     :  1 
#species names                         :  Occurrence 
#number of features                    :  4 
#feature names                         :  elevation, precipitation, temperature, vegetation 
#type                                  :  Presence-Absence 
#has independet test data?             :  FALSE 
#number of records                     :  200 
#has Coordinates?                      :  TRUE 


m1 <- sdm(Occurrence ~ elevation + precipitation + temperature + vegetation, data=d, methods='glm')
#y=occurrence
p1 <- predict(m1, newdata=preds)

plot(p1, col=cl)
points(species[species$Occurrence == 1,], pch=16)
#MAPPA DI DISTRIBUZIONE DELLA SPECIE, PREDICTION

####################################################################################################################################
####################################################################################################################################
####################################################################################################################################
#EXAM PROJECT











