#################### R code landcover

setwd("C:/lab")

library(raster)

#brick() importare un'immagine satellitare con tutti i dati

p224r63_2011 <- brick("p224r63_2011_masked.grd")

install.packages("RStoolbox")

library(RStoolbox)

#landsat bands 1b, 2g, 3r, 4nir

#rgb 
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin")

#
p224r63_2011c <- unsuperClass(p224r63_2011, nClasses=4)

# inserendo il nome dell'immagine p224r63_2011c si ottengono tutte le informazioni
#$ simbolo di unione
#map è la mappa che il plot ha generato
plot(p224r63_2011c$map)

#cambiare i colori della mappa 
clclass <- colorRampPalette(c('green', 'red', 'blue', 'black'))(100) 
plot(p224r63_2011c$map, col=clclass)
#diminuiamo il numero di classi a 2
p224r63_2011c <- unsuperClass(p224r63_2011, nClasses=2)
plot(p224r63_2011c$map)

#in funzione del numero di classi, aumenta l'incertezza dell' algoritmo automatico di classificazione

#riportando potenzialmente classi leggermente differenti

p224r63_2011c <- unsuperClass(p224r63_2011, nClasses=4)
plot(p224r63_2011c$map)

p224r63_2011c <- unsuperClass(p224r63_2011, nClasses=2)
plot(p224r63_2011c$map)







