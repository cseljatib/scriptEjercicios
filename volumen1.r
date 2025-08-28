##! Script: "volumen1.r"                                         /
##- Sobre:  Ajuste y comparacion de tres modelos de volumen     /
##+ Detalles:  Emplea estimador de minimos cuadrados.          /
##* Ejemplo: Datos de volumen Pinus pinaster (data=pinaster2)./
##                                                           /
##! --------------------------------------------------------/ 
##                                                         /
##> Profesor: Christian Salas Eljatib                     /
##? E-mail: christian.salas AT uchile DOT cl              /
## Web: https://eljatib.com                             /
##!====================================================/


##* @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##! I. Datos para ejemplo
##* @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
library(biometrics)
data("pinaster2")
#?pinaster2 #ejecutelo en la consola

df<-pinaster2

head(df)

df$d2 <- df$dap^2

##+ Definiendo el tipo de volumen a ocupar
df$v <- df$vtcc #volumen con corteza

library(datana)
descstat(df[,c("dap","atot","v")])

##* @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##! II. Graficos de interes
##* @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##-Distribucion
hist(df$dap)
hist(df$atot)
hist(df$v)


plot(v~dap, data=df)

##- Grafico dispersion con distribucion marginal
xyhist(x=df$dap,y=df$v)

##+ Creando una nueva variable
df$d2h<-(df$d2*df$atot)

plot(v~dap, data=df)
plot(v~d2, data=df)
plot(v~d2h, data=df)

df$ln.v <- log(df$v)
df$ln.d2h <- log(df$d2h)

plot(ln.v~ln.d2h, data=df)

##* @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##! III. Ajuste de modelos
##* @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##- Modelo 1
##? v_i=beta_0+beta_1(d_i^2)+varepsilon_i
m1<-lm(v~d2, data=df)
summary(m1)

##- Modelo 2
##? v_i=beta_0+beta_1(d_i^2*h)+varepsilon_i
m2 <- lm(v~d2h, data=df)
summary(m2)

##- Modelo 3
##? ln(v_i)=beta_0+beta_1ln(d_i)+beta_2ln(h_i)+varepsilon_i
df$ln.d<-log(df$dap)
df$ln.h<-log(df$atot)
m3 <- lm(ln.v~ln.d+ln.h, data=df)
summary(m3)

##* @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##! IV. Valores ajustados
##* @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##- Para el modelo 1
coef(m1)
b0.hat.m1<-coef(m1)[1]
b1.hat.m1<-coef(m1)[2]
b0.hat.m1
b1.hat.m1
##volumen predicho para un dap de 16.5 cm
b0.hat.m1+b1.hat.m1*16.5^2

## creando objeto
dap.ast <- 16.5
b0.hat.m1+b1.hat.m1*dap.ast^2

##- Para el modelo 2
coef(m2)
b0.hat.m2<-coef(m2)[1]
b1.hat.m2<-coef(m2)[2]
b0.hat.m2
b1.hat.m2

##volumen predicho para un dap de 16.5 cm y una altura de 11.7 m
b0.hat.m2+b1.hat.m2*(16.5^2*11.7)

## creando objeto
h.ast <- 11.7
b0.hat.m2+b1.hat.m2*(dap.ast^2*h.ast)

##- Para el modelo 3
coef(m3)
b0.hat.m3<-coef(m3)[1]
b1.hat.m3<-coef(m3)[2]
b2.hat.m3<-coef(m3)[3]
##una forma mas larga, pero eficiente
y.hat<- (b0.hat.m3+b1.hat.m3*(log(dap.ast))+b2.hat.m3*(log(h.ast)))
exp(y.hat)


#residuales
df$e1 <- residuals(m1)
df$e2 <- residuals(m2)
df$e3 <- residuals(m3)

#valores ajustados
df$v.aju1 <- fitted(m1)
df$v.aju2 <- fitted(m2)
df$v.aju3 <- fitted(m3)

#supuesto de normalidad
hist(df$e1)
hist(df$e2)
hist(df$e3)

#supuesto de homocedasticidad
plot(e1 ~ v, data=df)
abline(h=0,col="red", lwd=2)
plot(e1 ~ dap, data=df)
abline(h=0,col="red", lwd=2)
plot(e2 ~ dap, data=df)
abline(h=0,col="red", lwd=2)
plot(e3 ~ dap, data=df)
abline(h=0,col="red", lwd=2)


##* @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##! V. Valores predichos
##* @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##+ Valores predichos en la unidades de la variable de interes
df$v.pred1 <- df$v.aju1
df$v.pred2 <- df$v.aju2
df$v.pred3 <- exp(fitted(m3))

par(mfrow=c(1,3))
plot(v.pred1 ~ v, data=df)
abline(0,1,col="red", lwd=2)

plot(v.pred2 ~ v, data=df)
abline(0,1,col="red", lwd=2)

plot(v.pred3 ~ v, data=df)
abline(0,1,col="red", lwd=2)
dev.off()

##- ===================================
##? Tarea sugerida:
##calcular estadisticos de prediccion de cada modelo

#graficos de dispersion
par(mfrow=c(2,2))
plot(m1)
dev.off()

###comparar lo anterior con la sintaxis
#plot(m1)

par(mfrow=c(2,2))
plot(m2)
dev.off()

par(mfrow=c(2,2))
plot(m3)
dev.off()


#+╔═════════════════╗
#+║ Fin del script! ║
#+║ Atte.           ║
#+║ El profesor     ║
#+╚═════════════════╝

