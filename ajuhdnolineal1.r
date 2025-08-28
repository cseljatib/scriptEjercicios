##! Script: "ajuhdnolineal1.r"                                    /
##- Sobre:  Ajuste modelo no-lineal de altura-diametro           /
##+ Detalles:  Emplea estimador de minimos cuadrados no-lineales/
##* Ejemplo: Datos de altura-diametro (data= idahohd2).        /
##? Mas detalles: Modelo de poder                             /
##   + calcula valores predichos.                            /
##   + realiza grafico de comportamiento.                   /
##! -------------------------------------------------------/ 
##                                                        /
##> Profesor: Christian Salas Eljatib                    /
##? E-mail: christian.salas AT uchile DOT cl            /
## Web: https://eljatib.com                            /
##!===================================================/


##!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##+## I. Datos para ejemplo
##!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
library(datana)
data(idahohd2)
df <- idahohd2
#?idahohd2 #ejecutelo en la consola
head(df)
dim(df)
str(df)

##-Estadistica descriptiva
summary(df$atot)
##estadistica descriptiva para dos variables
summary(df[,c("atot","dap")])

##-Cuadro de estadistica descriptiva para dos variables
descstat(df[,c("atot","dap")])

##!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##+# II. Graficos de interes
##!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##- (1) Distribucion
boxplot(df$atot)
hist(df$atot)

boxplot(df$dap)
hist(df$dap)

##- (2) Dispersion
plot(atot ~ dap, data=df)

##!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##+## III. Ajuste del modelo
##!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#Ajuste del modelo de poder
poder.nls <- nls(atot~  b0*dap^(b1), 
              data = df,
              start = list(b0 = 7, b1=0.5),trace=T)

summary(poder.nls)
coef(poder.nls)

##- Algunos estadisticos de interes
##- RMSE del modelo
(summary(poder.nls))$sigma 
sigma.e<-(summary(poder.nls))$sigma 
sigma.e

##+ Int.Conf. coef.estimados
coef(poder.nls)
confint(poder.nls, level=0.95) 

##!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##+## IV. Grafico de comportamiento
##!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#
##-Guardando los coeficientes en un objeto
coef(poder.nls)
coef(poder.nls)[1]
b0.hat<-coef(poder.nls)[1]
b1.hat<-coef(poder.nls)[2]

b0.hat
b1.hat

##-Valores esperados segun el modelo
range(df$dap)
d.fake <- seq(10,122,by=0.1)
yhat<-predict(poder.nls, newdata=data.frame(dap=d.fake))

##- Grafico de dispersion con valor esperado del modelo
plot(atot~dap, data=df)
lines(d.fake, yhat, col="red",lwd=2)
##!==fin del grafico de comportamiento


##+ ===============================
##*Tarea
##- Calcule el RMSD, DIFA, y DA del modelo.
##+ ===============================


#-╔═════════════════╗
#-║ Fin del script! ║
#-║ Atte.           ║
#-║ El profesor     ║
#-╚═════════════════╝
