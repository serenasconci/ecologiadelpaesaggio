#####PRIMO CODICE R ECOLOGIA DEL PAESAGGIO

#per installare i pacchetti 
install.packages(sp)
#per richiamare i pacchetti library() o require()
library(sp)
#require(sp) 

data(meuse)
#richiama i dati da usare (data set)

head(meuse)
#fa vedere le prime righe del data set

names(meuse)
#mostra il nome delle variabili

summary(meuse)
#restituisce le info per il data set

pairs(meuse)
pairs(~ cadmium + copper + lead , data = meuse)

#ESERCIZIO: aggiungere lo zinco
pairs(~ cadmium + copper + lead + zinc , data = meuse)

pairs(meuse[,3:6])
#richiama le variabili senza scrivere i nomi ma indicando le posizioni
#viene prodotta una matrice di grafici a dispersione
pairs(meuse[,3:6] col="red")
#per decidere il colore si utilizza il comando col mettendo tra virgolette il colore
pairs(meuse[,3:6] col="red" , pch=19)
#per cambiare il tipo di punto nel grafico si utilizza il comando pch
pairs(meuse[,3:6], col="red", pch=19, cex=3,main="Primo pairs")
# cex (character exageration) serve per regolare le dimensioni dei caratteri
#cex < 1 piccole dimensioni, cex > 1 dimensioni maggiori
#il comando main permette di inserire il titolo

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
#funzione plot(meuse$cadmium , meuse$copper)
attach(meuse)

plot(cadmium , copper)

plot(cadmium , copper , pch=18 , col="blue" , main="primo plot" , xlab="cadmio" , ylab="rame")

plot(cadmium , copper , pch=18 , col="blue" , main="primo plot" , xlab="cadmio" , ylab="rame" , cex.lab=2 , cex=2)
