##! Script: "ajufdpwei.r"                                            /
##* Sobre:  Ajuste funcion de densidad de probabilidades            /
##+ Detalles: Emplea estimador numerico de maxima verosimilitud,   /
##  mediante optimizacion.                                       /
##- Ejemplo: Ajuste de funcion de Weibull para datos de          /
##  diametro de arboles en un bosque.                          /
##------------------------------------------------------------/ 
##                                                           /
##> Profesor: Christian Salas Eljatib                       /
## E-mail: christian.salas AT uchile DOT cl                /
## Web: https://eljatib.com                               /
##=======================================================/

##* @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##- I. Empleando la funcion de Poisson
##* @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##- (a) Matematicamente se escribe como
##....., escribiendo la expresion
beta<-6;alpha=3.6;y.aca<-5
(alpha/beta)* 
  ((y.aca/beta)^(alpha-1))*
  exp(-(y.aca/beta)^alpha)

##- (b) Ocupemos una funcion ya escrita en R, que
## calcula la misma expresion matematica que escribimos antes.
#La funcion se llama "dweibull()", revise ayuda sobre ella con
#?dweibull
dweibull(5,shape=3.6,scale=6)
dweibull(3,shape=1.6,scale=4)
##empleando nuestros objetos
dweibull(y.aca,shape=alpha,scale=beta)

##- (c) Ocupemos la expresion de la probabilidad acumulada
## de funcion de Weibull
1-exp(-(5/6)^3.6)##expresion matematica
pweibull(5,shape=3.6,scale=6)
pweibull(y.aca,shape=alpha,scale=beta)
#y otros
pweibull(5,shape=1.6,scale=4)
pweibull(10,shape=1.6,scale=4)

##* @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##- II. Datos a emplear
##* @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
library(datana)
head(llancahue2)
#Activar siguiente linea para ver metadata
#?llancahue
df <- llancahue2
dim(df)
str(df)

descstat(df[,"dap"])
num.arbs<-nrow(df)

##- Densidad del rodal
sup.plot<-130*70  #en m2
sup.plot
sup.plot.ha<-sup.plot/10000 #en hectareas
sup.plot.ha
fexp<-10000/sup.plot
nha<-nrow(df)*fexp
nha

#-- Declarando la variable aleatoria de interes
df$y <- df$dap #la variable aleatoria
summary(df$y)

hist(df$y)

histbxp(df$y)

##* @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##- III. Ajustando el modelo de Fdp
##* @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##+ Ajuste mediante maxima verosimilitud

##Maximizar la funcion de maxima verosimilitud de la fdp
##Aca se define (por Ud) una funcion que la calcule
loglike.wei <-function(parametros=parametros,
                        data=data){
  -sum(dweibull(data, shape=parametros[1],scale=parametros[2],log = T))
}
##la que tiene un signo negativo antes de la funcion
loglike.wei(c(1,20),df$y)
loglike.wei(c(4,30),df$y)

##valores iniciales para los parametros a ser estimados
candidatos<-c(.2,30)
candidatos

##?optim
optim.loglik.wei<-optim(c(candidatos[1],candidatos[2]),loglike.wei,data=df$y)
optim.loglik.wei
names(optim.loglik.wei)
optim.loglik.wei$par
optim.loglik.wei$value

#guardando resultados
param.wei.mle<-optim.loglik.wei$par
param.wei.mle
alpha.mle<-param.wei.mle[1]
beta.mle<-param.wei.mle[2]

##* @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##- IV. Aplicando el modelo ajustado
##- @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
## (a) generando una tabla de rodal
range(df$y)
w.amp <- 5
dap.l<-seq(5,80, by=w.amp)
lim.inf <- dap.l-(w.amp/2);lim.inf
lim.sup <- dap.l+(w.amp/2);lim.sup
cbind(lim.inf,lim.sup)
prob.sup<-pweibull(lim.sup, shape = alpha.mle, scale = beta.mle)
prob.inf<-pweibull(lim.inf, shape = alpha.mle, scale = beta.mle)
prob.cd<-prob.sup-prob.inf
sum(prob.cd)

##lo que resta de probabilidades debe ser asignado
delta.prob<-1-sum(prob.cd)
delta.prob
#una posibilidad es asignar diferencial uniformemente
length(dap.l)
delta.prob/length(dap.l)
#otra es asignar diferencial proporcionalmente
pondera.cd.ori<-pondera.cd<-prob.cd/sum(prob.cd)
sum(pondera.cd)

prob.cd.nogood<-prob.cd

data.frame(dap.l,lim.inf,lim.sup,prob.inf,prob.cd.nogood,pondera.cd)
data.frame(dap.l,lim.inf,lim.sup,prob.inf,prob.sup,prob.cd,pondera.cd)
addcd.dife.prob<-pondera.cd*delta.prob
prob.cd<-prob.cd+addcd.dife.prob

df.h0<-data.frame(dap.l,lim.inf,lim.sup,prob.inf,prob.sup,prob.cd,pondera.cd)
df.h<-data.frame(dap.l,lim.inf,lim.sup,prob.inf,prob.sup,prob.cd.nogood,pondera.cd.ori,addcd.dife.prob,prob.cd)
sum(prob.cd)
head(df.h)
dim(df.h)
tail(df.h)

##------------------------
##Para visualizar el ajuste del modelo, se puede proceder como sigue
##(1) primero construir tabla de rodal, para mismos intervalos anteriores
min.lim.sup<-df.h$lim.sup[1];min.lim.sup
max.lim.sup<-df.h$lim.sup[nrow(df.h)];max.lim.sup
breaks.yo<-c(0,seq(min.lim.sup,max.lim.sup,by=w.amp))
breaks.yo
cls.y <- breaks.yo; cls.y
y.class <- findInterval(df$y, cls.y)
y.class <- factor(y.class*w.amp); y.class
y.class[y.class=="0"]<-w.amp
nha.cd<-table(y.class)*fexp
cds<-sort(as.numeric(names(nha.cd)))
nha.cd<-as.numeric(nha.cd)
sum(nha.cd)
#tabla de rodal
trod<-data.frame(cds,nha.cd)
trod
sum(trod$nha.cd)

#(2) graficar ambas aproximaciones
length(dap.l)
length(cds)
frec.rel<-as.numeric(nha.cd/nha)
sum(prob.cd)
#para verificar
data.frame(cds,nha.cd,dap.l,prob.cd,frec.rel)
nha.cd.esp<-prob.cd*nha
#genera tabla de rodal, observada y esperada bajo el modelo fdp
trod.espe<-data.frame(cds,frec.rel,prob.cd,nha.cd,nha.cd.esp)
trod.espe

plot(cds,frec.rel,col="black",type = "o",las=1,bty="l",
     ylab="Frecuencia relativa",xlab="Diametro (cm)")
lines(dap.l,prob.cd,col="red",type = "o")
legend("topright",c("Observada","Fdp de Weibull"),
       col=c("black","red"),
       lty = c(1,1), pch=c(1,1))

message("Si ves este mensaje, estamos OK!!")

#╔══════════════════════╗
#║ Estimad@ estudiante: ║
#║ DisfRute el ejemplo! ║
#║ El profesor     ╔════╝
#╚═════════════════╝
