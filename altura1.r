##! Script: "altura1.r"                                           /
##- Sobre:  Ajuste modelo de regresion lineal simple (RLS)       /
##+ Detalles:  Emplea estimador de minimos cuadrados.           /
##* Ejemplo: Datos de altura-diametro (data=idahohd2).         /
##? Mas detalles: Entre otras cosas, en este ejercicio se:    / 
## + calculan valores ajustados y residuales.                /
## + representa sigma.hat.e en porcentaje.                  /
## + crea grafico con valores esperados vs diametro para   /
## el modelo.                                             /
##! -----------------------------------------------------/ 
##                                                      /
##> Profesor: Christian Salas Eljatib                  /
## E-mail: christian.salas AT uchile DOT cl           /
## Web: https://eljatib.com                          /
##!=================================================/


##* @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##! I. Datos para ejemplo
##* @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
library(datana)
data(idahohd2)
df <- idahohd2
#?idahohd2 #ejecutelo en la consola
head(df)
dim(df)

##-Estadistica descriptiva
summary(df$dap)
summary(df$atot)
summary(df[,c("dap","atot")])
head(df)
str(df)

##-Cuadro de estadistica descriptiva para dos variables
## usando una funcion especifica
df1<- df[,c("dap","atot")]
head(df1)
descstat(df1)
head(df)


##* @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##! II. Graficos de interes
##* @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##-Distribucion
hist(df$atot)
hist(df$dap)
##-Dispersion
plot(atot~dap, data=df)
plot(atot~dap, data=df, xlab="Diametro (cm)",
ylab="Altura (m)")
plot(atot~dap, data=df)
#compare grafico anterior, con los siguientes
plot(atot~dap, data=df, las=1)
plot(atot~dap, data=df, las=1, col="blue")
plot(atot~dap, data=df, xlab="Diametro (cm)",
     ylab="Altura (m)", las=1, col="blue")

plot(atot~dap, data=df)

##* @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##! III. Ajuste del modelo
##* @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##- Ajuste de modelo de regresion lineal simple
##  h_i=beta_0+beta_1(d_i)+varepsilon_i
mod1<- lm(atot~dap, data=df)
summary(mod1)

##+ ----------------
##- Algunos estadisticos de interes
##- RMSE del modelo
#note que la desv. estandar de los residuales se obtiene directamente
# con lo siguiente
summary(mod1)$sigma
##un error en porcentaje del modelo
100*summary(mod1)$sigma/mean(df$atot)

##+ ----------------
##- Almacenar la varianza de los residuales del modelo
summary(mod1)$sigma
var.hat.e.m1 <-summary(mod1)$sigma^2
var.hat.e.m1

##+ ----------------
##? Valor ajustado segun un diametro
12.39+0.3254994*50 #de 50 cm
12.39+0.3254994*20 #de 20 cm

##+ ----------------
##-Guardando los coeficientes en un objeto
coef(mod1)
coef(mod1)[1]
b0.hat<-coef(mod1)[1]
b1.hat<-coef(mod1)[2]
b0.hat
b1.hat
b0.hat + b1.hat * 50



##* @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##! IV. Valores ajustados y residuales
##* @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
df$aju <- b0.hat+b1.hat*df$d 
head(df)
##- Valor residual
df$e.aju <- df$atot - df$aju 
head(df)

##* @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##! V. Grafico de comportamiento
##* @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##- Generando vector ficticio con la variable predictora
d.fake <- 10:15
d.fake
b0.hat + b1.hat * d.fake
##- Creando una columna en la dataframe con los valores
# ajustados dependiendo de los respectivos valores
# de diametro para el modelo 1


##+ ---------------
##+ Otra forma de obtener lo mismo anterior
df$h.aju1 <- fitted(mod1)
df$e.aju1 <- residuals(mod1)

##- Grafico de comportamiento del modelo
##i. alternativa con funcion pre-programada en R
plot(atot~dap, data=df, xlab="Diametro (cm)", ylab="Altura (m)")
abline(mod1, col="red")

##- Compare el anterior con este grafico
plot(atot~dap, data=df, xlab="Diametro (cm)", ylab="Altura (m)",
     col="gray",las=1)
abline(mod1, col="red",lwd=2)


##? ii. alternativa mas larga, pero quizas mas transparente
50:55 #una secuencia
b0.hat + b1.hat * (50:55)
range(df$d)
d.fake <- 10:110
length(d.fake)
h.ajumod1 <- b0.hat + b1.hat * d.fake
plot(atot~dap, data=df)
lines(d.fake, h.ajumod1, col="red")


#>╔═════════════════╗
#>║ Fin del script! ║
#>║ Atte.           ║
#>║ El profesor     ║
#>╚═════════════════╝
