##! Script: "volumen2.r"                                          /
##- Sobre:  Ajuste y comparacion de dos modelos de volumen       /
##+ Detalles:  Uno multiple con cinco coeficientes, y el otro   /
## con variable respuesta transformada.                        /
##* Ejemplo: Datos de volumen Pinus pinaster (data=pinaster2)./
##                                                           /
##! --------------------------------------------------------/ 
##                                                         /
##> Profesor: Christian Salas Eljatib                     /
##? E-mail: christian.salas AT uchile DOT cl             /
## Web: https://eljatib.com                             /
##!====================================================/

##* @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##! I. Datos para ejemplo
##* @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
library(biometrics)
data("pinaster2")
#?pinaster2 #ejecutelo en la consola

data("pinaster2")
df<-pinaster2

##creando nuevas columnas (variables), para simplificar el codigo mas adelante
df$d<-df$dap
df$h<-df$atot
df$v<-df$vtcc
df$d2<-df$d^2
df$d2h2<-df$d2*df$h^2
df$h2d<-df$h^2*df$d
df$h2<-df$h^2

##* @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##! III. Ajuste de modelos
##* @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##- Modelo 1
##> mod1: v=b0+b1(d^2)+b2(d^2*h^2)+b3(d^2*h)+b4(h^2)
m1<-lm(v~d2+d2h2+h2d+h2,data=df)
summary(m1)

##- Prediccion a partir de modelo 1
dap<-30
htot<-22
b0.hat<-coef(m1)[1]
b1.hat<-coef(m1)[2]
b2.hat<-coef(m1)[3]
b3.hat<-coef(m1)[4]
b4.hat<-coef(m1)[5]
b0.hat+b1.hat*dap^2+b2.hat*(dap^2*htot^2)+b3.hat*(htot^2*dap)+b4.hat*(htot^2)

##- Modelo 2
##> mod2: (d^2)/v=b0+b1(1/h)
df$d2.v<-df$d2/df$v
df$inv.h<-1/df$h
m2<-lm(d2.v~inv.h,data=df)
summary(m2)
b0.hat.m2<-coef(m2)[1]
b1.hat.m2<-coef(m2)[2]
#valor predicho por modelo 2
df$dl.v.aju<-b0.hat.m2+b1.hat.m2*(1/df$h)
df$v.aju<- (df$d^2)*(1/df$dl.v.aju)
#calculo de error en prediccion del volumen
df$e.aju <- df$v - df$v.aju
#calculo del RMSD
sum(df$e.aju^2)/(nrow(df))
sqrt(sum(df$e.aju^2)/(nrow(df)))
rmsd<-sqrt(sum(df$e.aju^2)/(nrow(df)))
100*rmsd/mean(df$v)

#+╔═════════════════╗
#+║ Fin del script! ║
#+║ Atte.           ║
#+║ El profesor     ║
#+╚═════════════════╝
