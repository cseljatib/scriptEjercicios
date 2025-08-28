##! Script: "ahusa1.r"                                         /
##- Sobre:  Modelos de ahusamiento                            /
##+ Detalles:  Utilidad de un modelo de ahusamiento.         /
##* Ejemplo: predecir diametros fustales                    /
##? Mas detalles: se grafica el ahusamiento segun un modelo/
## dado                                                   /
##! -----------------------------------------------------/ 
##                                                      /
##> Profesor: Christian Salas Eljatib                  /
## E-mail: christian.salas AT uchile DOT cl           /
## Web: https://eljatib.com                          /
##!=================================================/

###uso de un modelo de ahusamiento
b0 <- -0.0482
b1 <- 0.72126
b2 <- -0.02417

h <- 30 #altura total, en m
d <- 35 #dap, en cm
#supongamos una altura de tocon de 0.3m
hst<-0.3
largo.trozo1<-3.6
hl<-hl.s <- largo.trozo1+hst #altura fustal
hl
x1 <- log((h-hl)/(h-1.3))
x1
x2 <- x1^2
x2

#supongamos que este es el modelo de ahusamiento
ln.y <- b0 + b1*x1 + b2*x2
ln.y

y <- exp(ln.y)
y

dl.pred <- y*d
dl.pred #diam. fustal cuando la altura fustal es hl


###calcular volumen de una troza
#porcion superior  a los 3.9m
dsup<-dl.pred #que ya calculamos
dsup

##estimar el diametro del tocon
hl <- hl.i<-hst #altura fustal
x1 <- log((h-hl)/(h-1.3))
x1
x2 <- x1^2
x2
ln.y <- b0 + b1*x1 + b2*x2
y <- exp(ln.y)
dl.pred <- y*d
dl.pred #diam. fustal cuando la altura fustal es hl
dinf <- dl.pred
dinf

dsup

#y el largo de la seccion es
largo.sec<- hl.s - hl.i
largo.sec

#volumen de la troza, usando formula de Smalian
vol = ((pi/40000)/2) * (dinf^2 + dsup^2) * largo.sec
vol

#####
#grafico de ahusamiento
hl <- seq(0.3,h,by=0.01)
hl

x1 <- log((h-hl)/(h-1.3))
x2 <- x1^2

ln.y <- b0 + b1*x1 + b2*x2

y <- exp(ln.y)

dl <- y*d
dl

plot(dl ~ hl, type="l", las=1)
#+╔═════════════════╗
#+║ Fin del script! ║
#+║ Atte.           ║
#+║ El profesor     ║
#+╚═════════════════╝
