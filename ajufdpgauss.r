##! Script: "ajufdpgauss.r"                                          /
##* Sobre:  Ajuste funcion de densidad de probabilidades.           /
##+ Detalles: Deriva estadisticos desde tabla de rodal y estimacion/
##  mediante el metodo de los momentos.                           /
##- Ejemplo: Ajuste de funcion de Gauss para datos derivados     /
##  de una tabla de rodal.                                      /
##-------------------------------------------------------------/ 
##                                                            /
##> Profesor: Christian Salas Eljatib                        /
## E-mail: christian.salas AT uchile DOT cl                 /
## Web: https://eljatib.com                                /
##========================================================/

##* @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##- I. Datos a emplear
##* @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
library(biometrics)
head(standtabCoihue2)
#Activar siguiente linea para ver metadata
#?standtabCoihue2
df <- standtabCoihue2

nha<-df[nrow(df),"nha"]
gha<-df[nrow(df),"gha"]

tr2<-df[-nrow(df),]

dbh.l <- as.numeric(as.character(tr2$cd)) 
m.y<-sum(dbh.l*tr2$nha)/nha
m.y

cd <- dbh.l
nha.cd.real<- tr2$nha

A<-sum(dbh.l^2*tr2$nha)
B<- (sum(dbh.l*tr2$nha))^2
C<- B/nha
vary<-(A-C)/(nha-1)
sd.y <- sqrt(vary)
sd.y

## Ahora apliquemos el modelo a los datos observados, 
##  usando la funcion ajustada
w.amp <- 5
lim.inf <- dbh.l-(w.amp/2)
lim.sup <- dbh.l+(w.amp/2)
#probabilidad acumulada para el modelo de Gauss
prob.sup<-pnorm(lim.sup, mean = m.y, sd = sd.y)
prob.inf<-pnorm(lim.inf, mean = m.y, sd = sd.y)
prob.cd<-prob.sup-prob.inf

sum(prob.cd)

##lo que resta de probabilidades debe ser asignado
delta.prob<-1-sum(prob.cd)
delta.prob

#una posibilidad es asignar diferencial uniformemente
delta.prob/length(dbh.l)
#otra es asignar diferencial proporcionalmente
pondera.cd<-prob.cd/sum(prob.cd)
sum(pondera.cd)

data.frame(dbh.l,lim.inf,lim.sup,prob.inf,prob.sup,prob.cd,pondera.cd)
add.dife.prob<-pondera.cd*delta.prob
prob.cd<-prob.cd+add.dife.prob

data.frame(dbh.l,lim.inf,lim.sup,prob.inf,prob.sup,prob.cd)
sum(prob.cd)

#Para visualizar el ajuste del modelo, se puede proceder como sigue
frec.rel<-tr2$nha/nha
plot(dbh.l,frec.rel,col="black",type = "o",las=1,bty="l",
       ylab="Frecuencia relativa",xlab="Diametro (cm)")
lines(dbh.l,prob.cd,col="red",type = "o")
  legend("topright",c("Observada","f.d.p. Gauss"),
         col=c("black","red"),
         lty = c(1,1), pch=c(1,1))

#@@@@@@@@@@@@@@@@@
#eso es todo estimad@s alumn@s
#disfRuten!
#saludos
#C
#@@@@@@@@@@@@@@@@@
