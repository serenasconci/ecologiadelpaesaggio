###PRIMO CODICE R ECOLOGIA DEL PAESAGGIO

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

