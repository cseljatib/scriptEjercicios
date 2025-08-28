##! Script: "biomasa2.r"                                         /
##- Sobre:  Ajuste modelos de biomasa                           /
##* Ejemplo: Datos de biomasa (data=biomass2).                 /
##? Mas detalles: Entre otras cosas, en este ejercicio :      /
## - Se ejemplifica alternativa "simple" para ajustar modelos/
##  que no involucra crear columnas nuevas en la base       /
##  de datos.                                              /
## - Lo mismo para las predicciones                       / 
##! -----------------------------------------------------/ 
##                                                      /
##> Profesor: Christian Salas Eljatib                  /
## E-mail: christian.salas AT uchile DOT cl           /
## Web: https://eljatib.com                          /
##!=================================================/

library(biometrics)
library(datana)
#?biomass2

df <- biometrics::biomass2
head(df)


mean(df$dap)

##Estadistica descriptiva para variables de interes
descstat(df[,c("dap","atot","wtot","wfuste","wramas","whojas")])

str(df)
tapply(df$atot,df$spp,mean)

tapply(df$atot,df$spp,length)

hist(df$dap)
hist(df$wtot)

plot(wtot~dap,data=df)

##Para analizar una nueva variable, antes hemos visto 
# que se puede crear una nueva variable: una transformacion del diametro
df$d2 <- df$dap^2

plot(wtot~d2, data=df)

## pero si lo anterior, se compara con 
plot(wtot~I(dap^2),data=df)

##Modelo base
##Y_i=beta_0+beta_1*(d_i^2)+varepsilon+i,
## varepsilon ~ G(0,varianza_varepsilon)

## ajuste modelo de biomasa total
#(i) antes vimos que se puede realizar mediante
m.tot <- lm(wtot ~ d2, data=df)
summary(m.tot)

#(ii) pero ahora utilizar la siguiente sintaxis
m1<-lm(wtot~I(dap^2),data=df)
summary(m1)
summary(m1)$sigma

#Como lograr la prediccion directamente con este nuevo
#  modelo?
dap <- 30:35
predict(m1, data.frame(dap))

sigma.e.m1<-summary(m1)$sigma
100*sigma.e.m1/mean(df$wtot)


plot(wtot~dap,data=df)
plot(wfuste~dap,data=df)

#modelo biomasa fustal
m2<-lm(wfuste~I(dap^2),data=df)
summary(m2)

sigma.e.m2<-summary(m2)$sigma
100*sigma.e.m2/mean(df$wfuste)

plot(wtot~dap,data=df)
plot(wfuste~dap,data=df)
plot(wramas~dap,data=df)

#modelo biomasa ramas
m3<-lm(wramas~I(dap^2),data=df)
summary(m3)

sigma.e.m3<-summary(m3)$sigma
100*sigma.e.m3/mean(df$wramas)

dap<-10:60
dap

w.tot<-predict(m1,data.frame(dap))
w.fus<-predict(m2,data.frame(dap))
w.ram<-predict(m3,data.frame(dap))

plot(w.tot~dap,type="l",las=1)
lines(dap,w.fus,col="blue")
lines(dap,w.ram,col="red")

legend("topleft", c("Total","Fuste","Ramas"),
       lty=c(1,1,1), title = "Biomasa",
       col=c("black","blue","red"))

##@@@@@@@@@@@@@@@@@@@@@@@@@
###en el caso de ajustar con 
# la alternativa "larga", de
# generar columnas, el procedimiento
#es como sigue
df$d2<-df$dap^2
m1b<-lm(wtot~d2,data=df)
m2b<-lm(wfuste~d2,data=df)
m3b<-lm(wramas~d2,data=df)
#compare los parametros estimados
# con los objetos m1, m2 y m3, respectivamente

#El grafico de comportamiento 
# esperado bajo el modelo en este
# caso se realiza como
d2<-dap^2

wb.tot<-predict(m1,data.frame(d2))
wb.fus<-predict(m2,data.frame(d2))
wb.ram<-predict(m3,data.frame(d2))

plot(wb.tot~dap,type="l",bty="l")
lines(dap,wb.fus,col="blue")
lines(dap,wb.ram,col="red")

#@@@@@@@@@@@@@@@@@
#eso es todo estimad@s alumn@s
#disfRuten!
#saludos
#C
#@@@@@@@@@@@@@@@@@

