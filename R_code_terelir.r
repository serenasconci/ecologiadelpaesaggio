################Codice R per le analisi di immagini satellitari
#pacchetto raster
install.packages("raster")
library(raster)

setwd("C:/lab")

#<-nome all'immagine, è preferibile in questo caso inserire anche l'anno
#brick() importa un'immagine satellitare con tutte le bande

p224r63_2011 <- brick("p224r63_2011_masked.grd")

plot(p224r63_2011)
