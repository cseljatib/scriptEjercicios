##! Script: "varestado7.r"                                       /
##- Sobre:  Calculo de variables de estado--de orden            /
##+ Detalles: Para la variable altura                          /
##* Ejemplo: Datos de una parcela de muestreo (data=eucaplot2)/
##? Mas detalles: El script se organiza como sigue:          /
##  1. Calculo de variables (de estado) agregadas.          /
##  2. Calculo de variables (de estado) de orden para      /
## la altura.                                             /
##  3. Calculo de altura dominante, empleando una mejor  /
## alternativa de estimacion.                           /
##! ---------------------------------------------------/ 
##                                                    /
##> Profesor: Christian Salas Eljatib                /
##? E-mail: christian.salas AT uchile DOT cl        /
## Web: https://eljatib.com                        /
##!===============================================/

##!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##+## I. Datos a emplear
##!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
library(biometrics)
data(eucaplot2)
head(eucaplot2)
#?eucaplot2
#~~~~~~~

#asinacion de dataframe a objeto "df"
df<-eucaplot2

#==============================
#1) Calculo de variables N y G
#==============================
#1a. Calculo de la densidad (o "N" segun la iufro)
#i. factor de expansion
df$fe <- 10000/500
head(df)
#ii. la densidad a nivel "de rodal" (N)
nha <- sum(df$fe)
nha #en arb/ha

#1b. Calculo del area basal (o "G" segun la iufro)
#i. area basal de cada arbol
df$g <- (pi/40000)*df$dap^2
head(df)
summary(df$g)

#ii. area basal de cada arbol expandida a la hectarea
df$garb.ha <- df$g * df$fe
head(df)

#iii. el area basal a nivel de "rodal" (G)
gha <- sum(df$garb.ha)
gha

#=====================
#2) Calculo de variables de orden para la altura.
# Note que estas son variables de estado de rodal.
##! =====================
##- 2a) Altura media
hmed<- mean(df$atot)
hmed

##- 2b) Altura dominante ("Hdom" o "Hd" segun la iufro)
## cual es la definicion de hdom?
plot.ha<-500/10000 #la superficie de la parcela en ha
plot.ha*100 #numero de arboles necesarios en esta parcela
nref.plot<-plot.ha*100
    
h.dap.orden <- df[ order(-df$dap), "atot"]
h.dap.orden

hdom <- mean(h.dap.orden[1:nref.plot])
hdom

#2c) Altura del arbol de area basal central
median(df$g)
df.ordg.h<-df[ order(-df$g), c("g","atot")]
df.ordg.h
#buscar la altura para el area basal mediana

#en este caso que es n impar, se puede buscar mediante
median.pos<-ceiling(nrow(df)/2)
hg.central<-df.ordg.h[median.pos,c("atot")]
hg.central

#comparando las distintas alturas de rodal
c(hmed,hdom,hg.central)

##*==============================
##2) Calculo altura dominante, con un mejor estimador
##*==============================
##- i. Altura dominante segun un estimador alternativo
# al anterior (o tradicional), ya que sobre-estima.  
# Mejor es emplear un estimador que considere esto.
#ii. revisar funcion domvar() con el algoritmo necesario
#?biometrics::domvar
#iii. calculo de altura dominante, con estimador mas apropiado
hdom.alt<-domvar(data=df,var.int="atot",var.sort="dap",plot.area=500)
hdom.alt

##- Compare este valor con el obtenido en hdom
c(hdom, hdom.alt)

#lo mismo se puede hacer para la estimacion del diam.dominante
ddom.alt<-domvar(data=df,var.int="dap",var.sort="dap",plot.area=500)
ddom.alt

#╔═════════════════╗
#║ Fin del script! ║
#╚═════════════════╝
