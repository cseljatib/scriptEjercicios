##! Script: "ajuhdnolineal3.r"                                    /
##- Sobre:  Ajuste modelos no-lineal de altura-diametro          /
##+ Detalles:  Emplea expresiones ya definidas para los modelos /
##* Ejemplo: Datos de altura-diametro (data= idahohd2).        /
##? Mas detalles: Modelos de poder, Michaelis-Menten y Stage  /
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
##- ahora ocupando una expresion ya definida en el paquete biometrics
library(biometrics)
mm.nls<-nls(atot~mmenten.fx(x=dap,paramod = c(b0,b1)),
  data = df,start = list(b0 = 170, b1=0.5),trace=T)
    

summary(mm.nls)
coef(mm.nls)

##+ compare lo anterior con la forma anterior que se reviso para
## ajuste no-lineal

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
##- ahora ocupando una expresion ya definida en el paquete biometrics
sta.nls<-nls(atot~stage.fx(x=dap,paramod = c(b0,b1,b2)),
            data = df,start = list(b0 = 20, b1=8,b2=.8),trace=T)

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

##+ (3) Ajuste del modelo de Poder
##- ahora ocupando una expresion ya definida en el paquete biometrics
pod.nls<-nls(atot~power.fx(x=dap,paramod = c(b0,b1)),
            data = df,start = list(b0 = 10, b1=.5),trace=T)

summary(pod.nls)
coef(pod.nls)

##- Algunos estadisticos de interes
##- RMSE del modelo
sigma.e.m2<-(summary(pod.nls))$sigma 
sigma.e.m2

##+ Int.Conf. coef.estimados
coef(pod.nls)

##+ Valores ajustados por el modelo
df$h.pod<-fitted(pod.nls)


##!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##+## IV. Grafico de comportamiento
##!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#
##-Valores esperados segun cada modelo
range(df$dap)
d.fake <- seq(10,122,by=0.1)
yhat.mm<-predict(mm.nls, newdata=data.frame(dap=d.fake))
yhat.sta<-predict(sta.nls, newdata=data.frame(dap=d.fake))
yhat.pod<-predict(pod.nls, newdata=data.frame(dap=d.fake))

##- Grafico de dispersion con valor esperado de cada modelo
plot(atot~dap, data=df,las=1)
lines(d.fake, yhat.mm, col="red",lwd=2)
lines(d.fake, yhat.sta, col="blue",lwd=2,lty=2)
lines(d.fake, yhat.pod, col="green",lwd=2,lty=1)

legend("bottomright",c("Michaelis-Menten","Stage","Poder"), title="Modelo",
       col = c("red","blue","green"), lty=c(1,2,1), lwd=c(2,2,2))

##!==fin del grafico de comportamiento

##+ Capacidades predictivas
##- Calcule el RMSD, DIFA, y DA de los tres modelos.
valesta(y.obs = df$atot, y.pred = df$h.mm)

valesta(y.obs = df$atot, y.pred = df$h.sta)

valesta(y.obs = df$atot, y.pred = df$h.pod)

##+ ===============================
##*Tarea
##- Confeccione grafico residual vs. valor ajustado de cada modelo.
##- Realice un analisis de estadistica inferencial de cada modelo
##   ajustado.
##- Cual modelo prefiere?
##+ ===============================


#>╔═════════════════╗
#>║ Fin del script! ║
#>║ Atte.           ║
#>║ El profesor     ║
#>╚═════════════════╝
