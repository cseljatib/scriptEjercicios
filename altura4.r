##! Script: "altura4.r"                                           /
##- Sobre:  Comparacion de dos modelos de altura-diametro        /
##+ Detalles:  Emplea estimador de minimos cuadrados.           /
##* Ejemplo: Datos de altura-diametro (data= biomass2).        /
##? Mas detalles: Entre otras cosas, en este ejercicio se:    /
##   + calcula valores predichos, y errores.                 /
##   + calcula los estadisticos de capacidades predictivas: /
##  RMSD, DIFA y DA.                                       /
##! ------------------------------------------------------/ 
##                                                       /
##> Profesor: Christian Salas Eljatib                   /
##? E-mail: christian.salas AT uchile DOT cl           /
## Web: https://eljatib.com                           /
##!==================================================/


##* @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##! I. Datos para ejemplo
##* @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
library(biometrics)
data(biomass2)
df <- biomass2
#?biomass2 #ejecutelo en la consola
head(df)
dim(df)

##-Estadistica descriptiva
library(datana)
descstat(df[,c("dap","atot")])


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



##* @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##! III. Ajuste del modelo 1
##  h=b0+b1*d
##* @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
mod1<- lm(atot~dap, data=df)
summary(mod1)
#guardando los coeficientes en un objeto
coef(mod1)
coef(mod1)[1]
b0.hat<-coef(mod1)[1]
b1.hat<-coef(mod1)[2]

#- Valor ajustado
#* antes se obtuvo este valor mediante
#b0.hat+b1.hat*df$dap
# el cual es "el camino largo", pero tambien se puede 
# lograr mediante la funcion fitted()
df$aju <-    fitted(mod1)
head(df)
#- Valor residual 
df$e.aju <- df$atot - df$aju 
head(df)


##* @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##! IV. Calculo de estadisticos de prediccion
##* @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
n <- nrow(df)
mean.h <- mean(df$atot)
mean.h

##los siguientes es usando la terminologia ocupada en el paper
# de Salas et al 2010 (Remote Sensing of Environment)

#rmsd, raiz cuadrada media de las diferencias (RMSD)
rmsd <- sqrt(sum(df$e.aju^2)/n)
rmsd

#ad, o diferencia agregada (DA)
ad <-  mean(df$e.aju)
ad

##aad, o dif. media absoluta (DIFA)
aad <-  mean(abs(df$e.aju))
aad

#- Calculo de estad. de prediccion en %
100*ad/mean.h
100*rmsd/mean.h
100*aad/mean.h


##* @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##! V. Uso de la funcion valesta() de datana
#- lo mismo que se explico antes, esta implementado en
# la funcion mencionada, como sigue
valesta(y.obs=df$atot,y.pred=df$aju)

##* @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##! VI. Ajuste de otro modelo
## ln(h_i)=beta.0+beta.1*e^(-0.03d_i)+varepsilon_i
##* @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##- Creando las variables Y e X necesarias para el modelo 2
df$ln.h<-log(df$atot)
df$exp.d<-exp(-0.03*df$dap)
plot(ln.h~exp.d, data=df)

descstat(df[,c("dap","exp.d","atot","ln.h")])
mod2<- lm(ln.h~exp.d, data=df)
summary(mod2)
b0.hat2<-coef(mod2)[1]
b1.hat2<-coef(mod2)[2]
b0.hat2
b1.hat2

#- Valor ajustado
#* calculado mediante la funcion fitted(), pero note
# que hay un cambio...... a que se debe?
df$aju2<-exp(fitted(mod2))
head(df)

#- Valor residual 
df$e.aju <- df$atot - df$aju 
head(df)

valesta(y.obs=df$atot,y.pred=df$aju2)

#- Comparacion entre ambos modelos
#? Modelo 1
valesta(y.obs=df$atot,y.pred=df$aju)
#? Modelo 2
valesta(y.obs=df$atot,y.pred=df$aju2)


##- ===================================
##! Tarea sugerida:
## 1. Realice un grafico entre el error de prediccion y el valor
## observado
## 2. Realice un grafico entre el error de prediccion y el valor
## ajustado
## 3. Realice un grafico entre el error de prediccion y el diametro
## 4. En base a los calculos realizados, ¿Cual modelo seleccionaria?
## y ¿Por que?.
##- ===================================


#>╔═════════════════╗
#>║ Fin del script! ║
#>║ Atte.           ║
#>║ El profesor     ║
#>╚═════════════════╝
