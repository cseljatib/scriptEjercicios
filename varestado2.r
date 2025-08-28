##! Script: "varestado2.r"                                       /
##- Sobre:  Calculo de variables de estado, nivel agregado      /
##+ Detalles: Estimacion de densidad, area basal y volumen     /
##* Ejemplo: Datos de una parcela de muestreo (data=eucaplot2)/
##? Mas detalles: El script se organiza como sigue:          /
##  1. Calculo de densidad y area basal.            *#
##  2. Calculo de volumen.                          *#
##  3. Calculo de las variables anteriores, pero    *#
##  segregado por niveles de un factor.             *#
##! -----------------------------------------------------/
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
#=====================
#i. Volumen del arbol
# estimacion mediante factor de forma
ff <- 0.3 #factor de forma
#ii. calculo del volumen de cada arbol
df$v <- df$g*df$atot*ff
summary(df$v)
#ii. volumen de cada arbol expandido a la hectarea
df$varb.ha <- df$v * df$fe
vha <- sum(df$varb.ha)
vha

#=====================
#3) Variables de estado segregadas por niveles de un factor
#=====================
# se empleara como factor la sanidad (i.e., estado sanitario del arbol)
unique(df$sanidad)
table(df$sanidad)

#-----
#3a. Densidad por cada clase de sanidad
nha.psan<-tapply(df$fe,df$sanidad,sum)
nha.psan
#comparar la suma de estos valores segregados por sanidad....
sum(nha.psan)
#..., con el de la densidad total
nha

#-----
#3b. Area basal por cada clase de sanidad
gha.psan<-tapply(df$garb.ha,df$sanidad,sum)
gha.psan
#comparar la suma de estos valores segregados por sanidad....
sum(gha.psan)
#..., con el del area basal total del rodal
gha

#-----
#3c. Volumen por cada clase de sanidad
vha.psan<-tapply(df$varb.ha,df$sanidad,sum)
vha.psan
#comparar la suma de estos valores segregados por sanidad....
sum(vha.psan)
#..., con el del volumen total del rodal
vha


#╔═════════════════╗
#║ Fin del script! ║
#║ Atte.           ║
#║ El profesor     ║
#╚═════════════════╝
