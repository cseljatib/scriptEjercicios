##! Script: "ajuhdnolineal2.r"                                    /
##- Sobre:  Ajuste modelos no-lineal de altura-diametro          /
##+ Detalles:  Emplea estimador de minimos cuadrados no-lineales/
##* Ejemplo: Datos de altura-diametro (data= idahohd2).        /
##? Mas detalles: Modelo de Michaelis-Menten y de Stage       /
##   + calcula valores predichos.                            /
##   + calcula estadisticos de prediccion RMSD, AD, AAD.    /
##   + realiza grafico de comportamiento.                  /
##! ------------------------------------------------------/ 
##                                                       /
##> Profesor: Christian Salas Eljatib                   /
##? E-mail: christian.salas AT uchile DOT cl           /
## Web: https://eljatib.com                           /
##!==================================================/


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
##-Distribucion
boxplot(df$atot)
hist(df$atot)

boxplot(df$dap)
hist(df$dap)

##-Dispersion
plot(atot ~ dap, data=df)

##!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##+## III. Ajuste del modelo
##!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##+ (1) Ajuste del modelo de Michaelis-Menten
mm.nls <- nls(atot~(b0*dap)/(b1+dap), 
  data = df,
start = list(b0 = 170, b1=0.5),trace=T)

summary(mm.nls)
coef(mm.nls)

##- Algunos estadisticos de interes
##- RMSE del modelo
(summary(mm.nls))$sigma 
sigma.e.m1<-(summary(mm.nls))$sigma 
sigma.e.m1

##+ Int.Conf. coef.estimados
coef(mm.nls)

##+ Valores ajustados por el modelo
df$h.mm<-fitted(mm.nls)

##+ (2) Ajuste del modelo de Stage
sta.nls <- nls(atot~
                b0*exp(-b1/(dap^b2)), 
              data = df,
              start = list(b0 = 20, b1=8,b2=.8),trace=T)

summary(sta.nls)
coef(sta.nls)

##- Algunos estadisticos de interes
##- RMSE del modelo
sigma.e.m2<-(summary(sta.nls))$sigma 
sigma.e.m2

##+ Int.Conf. coef.estimados
coef(sta.nls)

##+ Valores ajustados por el modelo
df$h.sta<-fitted(sta.nls)

##!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##+## IV. Grafico de comportamiento
##!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#
##-Guardando los coeficientes en un objeto
##+ (1) Modelo Michaelis-Menten
b0.hat.mm<-coef(mm.nls)[1]
b1.hat.mm<-coef(mm.nls)[2]
b0.hat.mm
b1.hat.mm

##+ (2) Modelo Stage
b0.hat.sta<-coef(sta.nls)[1]
b1.hat.sta<-coef(sta.nls)[2]
b2.hat.sta<-coef(sta.nls)[3]
c(b0.hat.sta,b1.hat.sta,b2.hat.sta)


##-Valores esperados segun cada modelo
range(df$dap)
d.fake <- seq(10,122,by=0.1)
yhat.mm<-predict(mm.nls, newdata=data.frame(dap=d.fake))
yhat.sta<-predict(sta.nls, newdata=data.frame(dap=d.fake))

##- Grafico de dispersion con valor esperado de cada modelo
plot(atot~dap, data=df,las=1)
lines(d.fake, yhat.mm, col="red",lwd=2)
lines(d.fake, yhat.sta, col="blue",lwd=2,lty=2)

legend("bottomright",c("Michaelis-Menten","Stage"), title="Modelo",
       col = c("red","blue"), lty=c(1,2), lwd=c(2,2))

##!==fin del grafico de comportamiento


##- Calcule el RMSD, DIFA, y DA de cada modelo.
valesta(y.obs = df$atot, y.pred = df$h.mm)

valesta(y.obs = df$atot, y.pred = df$h.sta)

##+ ===============================
##*Tarea
##- Confeccione grafico residual vs. valor ajustado de cada modelo.
##- Cual modelo prefiere?
##+ ===============================


#>╔═════════════════╗
#>║ Fin del script! ║
#>║ Atte.           ║
#>║ El profesor     ║
#>╚═════════════════╝
