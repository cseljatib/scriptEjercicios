##! Script: "ahusa3.r"                                            /
##- Sobre:  Usando un modelo de ahusamiento para simular         /
## trozado                                                      /
##+ Detalles:  Estructura de datos y uso de modelo             /
##* Ejemplo: Datos de ahusamiento (data=tapereurca2).         /
##? Mas detalles: Entre otras cosas, en este ejercicio se:   /
## Se emplea el modelo para predecir diametros fustales,    /
## asi como tambien alturas fustales y calculo de          /
## volumen.                                               /
##! -----------------------------------------------------/ 
##                                                      /
##> Profesor: Christian Salas Eljatib                  /
## E-mail: christian.salas AT uchile DOT cl           /
## Web: https://eljatib.com                          /
##!=================================================/


#======================================
#coeficientes del modelo de Kozak (1988)
alpha0 <- 1.02453;alpha1 <-0.88809 ; alpha2 <- 1.00035
beta1 <- 0.95086;beta2 <- -0.18090 ; beta3 <- 0.61407; beta4 <- -0.35105;
beta5 <- 0.05686 
p <- 0.25
kozak88 <- function(dap, h, hl) {
  dib <- alpha0 * dap^alpha1 * alpha2^dap
  X <- (1.0 - sqrt(hl / h)) / (1.0 - sqrt(p))
  Z=hl/h
  a=beta1*Z*Z;
  b = beta2 * log(Z + 0.001);
  c = beta3 * sqrt(Z) +  beta4 * exp(Z);
  d = beta5 * (dap / h);
  retval<-(dib*X^(a+b+c+d))
  retval[h < hl] <- 0
  retval }

dap.arb <- 45
h.arb <- 27

#graficando el perfil fustal segun el modelo
hl.fake <- seq(0,h.arb,by=0.01)
dl.hat <- kozak88(dap=dap.arb,h=h.arb,hl=hl.fake)
plot(dl.hat~hl.fake, col="blue", type="l",las=1, xlim=c(min(hl.fake),max(hl.fake)*1.1),
     ylab="Diametro fustal (cm)", xlab="Altura fustal (m)",bty="l")
abline(v=0,lty=2);abline(h=0,lty=2)


#altura comercial
hiu.fx <- function(hl, dap, h, diu) {
  dl <- kozak88(dap=dap.arb,h=h.arb, hl) 
  dlf <- dl - diu
  dlf
}

#DIU objetivo
diu.objetivo <- diufijo <- 10

#buscando la raiz de la funcion
out.hiu.hat <- uniroot(hiu.fx,
                       c(0, h.arb),
                       dap = dap.arb,
                       h = h.arb,
                       diu = diu.objetivo)
out.hiu.hat
hiu.hat<-out.hiu.hat$root
hiu.hat
#esta es la altura en el fuste
# donde se encuentra el diametro fustal de 10 cm.

#verifiquemos si es cierto
diu.check <- kozak88(dap=dap.arb, h=h.arb, hl=hiu.hat)
diu.check


###Definicion de productos comerciales
#primero calculo del diam. de tocon
hst <- 0.3
diu.tocon <- kozak88(dap=dap.arb,h=h.arb, hl=hst)
diu.tocon <- round(diu.tocon,3)

trozo.breaks<-c(5,10,23,27,33,diu.tocon)
trozo.produ<-c("lenha","pulpa","ase2","ase1","debo")


##buscando las alturas comerciales
hiu.bks <- rep(0, length(trozo.breaks))
for(i in 1:length(trozo.breaks)) {
  dap <- dap.arb; h <- h.arb;
  if(trozo.breaks[i] <= kozak88(dap, h, 0.0)) {
    hiu <- uniroot(hiu.fx,
                   c(0, h),
                   dap = dap,
                   h = h,
                   diu = trozo.breaks[i])
    hiu.bks[i] <- hiu$root
  } else {
    hiu.bks[i] <- 0.0
  }
}

trozo.breaks
#las alturas fustales a las cuales el fuste
hiu.bks
cbind(trozo.breaks,hiu.bks)


#el largo de cada seccion
largo.trozos1 <- diff(-hiu.bks)
largo.trozos1

diam.min <- (trozo.breaks[1:(length(trozo.breaks)-1)])
diam.min
diam.max <- (trozo.breaks[2:(length(trozo.breaks)-1)])
diam.max
diam.max <- c(diam.max,diu.tocon)
diam.max
largo.trozos <-largo.trozos1
largo.trozos

##una funcion para Smalian
volsecc.fx <- function(d1, d2, l) {
    volsecc <- (pi/40000)*(1/2)*(d1^2 + d2^2) * l 
    volsecc
  }
#la cual revisamos mediante
volsecc.fx(d1=c(10,5),d2=c(5,2),l=c(2,1))

#calculo de volumen de cada trozo, segun Smalian
vol.m3<- volsecc.fx(d1=diam.min,d2=diam.max,l=largo.trozos)

df.trozos<-data.frame(diam.min,diam.max,largo.trozos, vol.m3)
df.trozos

#?╔═════════════════╗
#?║ Fin del script! ║
#?║ Atte.           ║
#?║ El profesor     ║
#?╚═════════════════╝
