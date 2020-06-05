#################### R code landcover

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

