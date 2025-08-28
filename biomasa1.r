##! Script: "biomasa1.r"                                         /
##- Sobre:  Ajuste modelo de biomasa                            /
##* Ejemplo: Datos de biomasa (data=biomass2).                 /
##? Mas detalles: Entre otras cosas, en este ejercicio :      /
## - Se ejemplifica alternativa "larga" para ajustar modelos /
##  que involucran transformacion de columnas de la base    /
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

df <- biometrics::biomass2

head(df)

tapply(df$atot, df$spp, length)

boxplot(df$dap)
##Estadistica descriptiva para variables de interes
descstat(df[,c("dap","atot","wtot","wfuste","wramas","whojas")],full = TRUE)

#creando una nueva variable: una transformacion del diametro
df$d2 <- df$dap^2

plot(wtot~dap, data=df)
plot(wtot~d2, data=df)

##Modelo base
##Y_i=beta_0+beta_1*(d_i^2)+varepsilon+i,
## varepsilon ~ G(0,varianza_varepsilon)

d.list <- 10:60
d.list

## ajuste modelo de biomasa total
m.tot <- lm(wtot ~ d2, data=df)
summary(m.tot)
b0.hat.mtot <- coef(m.tot)[1]
b1.hat.mtot <- coef(m.tot)[2]

w.tot.hat <- b0.hat.mtot + b1.hat.mtot*d.list^2

## ajuste modelo de biomasa del fuste
m.fu <- lm(wfuste ~ d2, data=df)
summary(m.fu)
b0.hat.mfu <- coef(m.fu)[1]
b1.hat.mfu <- coef(m.fu)[2]


w.fu.hat <- b0.hat.mfu + b1.hat.mfu*d.list^2

##ajuste modelo de ramas
m.ra <- lm(wramas ~ d2, data=df)
summary(m.ra)
b0.hat.mra<- coef(m.ra)[1]
b1.hat.mra<- coef(m.ra)[2]
w.ra.hat <- b0.hat.mra+b1.hat.mra*d.list^2
#plot(d.list,w.ra.hat, type="l", las=1, col="blue")


head(data.frame(d.list,w.tot.hat,w.fu.hat,w.ra.hat))

####combinar en un solo grafico
plot(d.list,w.tot.hat, type = "l", las=1, xlab="Diametro (cm)", ylab="Biomasa (kg)")
lines(d.list,w.fu.hat, col="blue")
lines(d.list,w.ra.hat, col="red")

legend("topleft", c("Total","Fuste","Ramas"),
       lty=c(1,1,1), title = "Biomasa",
       col=c("black","blue","red"))

##ejercios Extras
###1. analisis descriptivo de los componentes de biomas, pero en terminos porcentuales
## por ejemplo, el del fuste seria
df$fuste.p<-100*df$wfuste/df$wtot
summary(df$fuste.p)


##- ===================================
##! Tarea sugerida:
##1. Calcular lo anterior para cada componente
##2. Repetir lo anterior, pero por especie
##3. Ajuste los modelos del script, pero por especie.
##- ===================================

#>╔═════════════════╗
#>║ Fin del script! ║
#>║ Atte.           ║
#>║ El profe        ║
#>╚═════════════════╝

