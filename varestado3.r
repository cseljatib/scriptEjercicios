##! Script: "varestado3.r"                                       /
##- Sobre:  Calculo de volumen de rodal, diferentes alternativas/
##+ Detalles: Para estimacion de volumen fundamentalmente      /
##* Ejemplo: Datos de una parcela de muestreo (data=eucaplot2)/
##? Mas detalles: El script se organiza como sigue:          /
##  1. Calculo de densidad y area basal.                    /
##  2. Calculo de volumen con un factor de forma.          /
##  3. Calculo de volumen con una funcion de volumen.     /
##! -----------------------------------------------------/
## Se busca ilustrar como cambia la estimacion de    *#
##  volumen individual, y la estimacion de dicha     *#
##  variable a nivel agregado, dependiendo de la     *#
##  alternativa de prediccion empleada.              *#
##                                                      /
##> Profesor: Christian Salas Eljatib                  /
##? E-mail: christian.salas AT uchile DOT cl          /
## Web: https://eljatib.com                          /
##!=================================================/

##!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##+## I. Datos a emplear
##!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
library(datana)
data(eucaplot2)
head(eucaplot2)
?eucaplot2
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
#2) Calculo del volumen del rodal (o "V" segun la iufro)
# mediante un factor de forma
#=====================
# Volumen del arbol mediante factor de forma
ff <- 0.3 #factor de forma
#i. calculo del volumen de cada arbol
df$v <- df$g*df$atot*ff
summary(df$v)
#ii. volumen de cada arbol expandido a la hectarea
df$varb.ha <- df$v * df$fe
#iii. volumen del rodal
vha <- sum(df$varb.ha)
vha


dim(df)
#=====================
#3) Calculo del volumen del rodal mediante un modelo de volumen
#=====================
#i. mediante una funcion de volumen
df$v2 <- 0.0195213+0.0000282*(df$dap^2*df$atot)
#ii. volumen de cada arbol expandido a la hectarea
df$varb2.ha <- df$v2 * df$fe
#iii. volumen del rodal
vha2 <- sum(df$varb2.ha)
vha2

#~~~~~~~~~
#Compare los volumenes de rodal, mediante ambas alternativas
# de estimacion del volumen individual
c(vha,vha2)



#╔═════════════════╗
#║ Fin del script! ║
#╚═════════════════╝
