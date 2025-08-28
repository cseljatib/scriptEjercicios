##! Script: "varestado4.r"                                       /
##- Sobre:  Calculo de variables de estado--nivel agregado      /
##+ Detalles: Para el volumen fundamentalmente                 /
##* Ejemplo: Datos de una parcela de muestreo (data=eucaplot2)/
##? Mas detalles: El script se organiza como sigue:          /
##  1. Calculo de volumen con una funcion de volumen,       / 
##  2. Estimacion simple de biomasa y carbono              /
## a nivel de rodal.                                      /
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

#primero calculemos la densidad
df$fe <- 10000/500
Nha <- sum(df$fe)
Nha
#luego el area basal de cada arbol
df$g <- (pi/40000)*df$dap^2
#el area basal de cada arbol expandida a la hectarea
df$garb.ha <- df$g * df$fe
Gha <- sum(df$garb.ha)
Gha


##volumen
## usaremos un factor de forma
## ff=v/v_r
## v=ff * v_r 
ff <- 0.3 #factor de forma
#luego el volumen de cada arbol
df$v <- df$g*df$atot*ff
#el volumen de cada arbol expandido a la hectarea
df$varb.ha <- df$v * df$fe
Vha <- sum(df$varb.ha)
Vha

##ahora estimaremos el volumen, pero no mediante
# un factor de forma, que es una simplificacion, sino
# que mediante una funcion de volumen
df$v2 <- 0.0195213+0.0000282*(df$dap^2*df$atot)
summary(df$v2)
df$varb2.ha <- df$v2 * df$fe

Vha2 <- sum(df$varb2.ha)
Vha2
#compare este volumen con el anterior calculado
c(Vha,Vha2)

##podemos estimar biomasa
dm <- 550 #kg/m3
df$w <- df$v2*dm
df$warb.ha <- df$w *df$fe
Wha <- sum(df$warb.ha)
Wha
Wha/1000 #en toneladas/ha

#y carbono
carbono.ha <- (Wha/1000)/2
carbono.ha


#╔═════════════════╗
#║ Fin del script! ║
#║ Atte.           ║
#║ El profesor     ║
#╚═════════════════╝
