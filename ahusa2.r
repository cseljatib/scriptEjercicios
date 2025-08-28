##! Script: "ahusa2.r"                                           /
##- Sobre:  Ajuste modelo de ahusamiento                        /
##+ Detalles:  Estructura de datos y uso de modelo             /
##* Ejemplo: Datos de ahusamiento (data=tapereurca2).         /
##? Mas detalles: Entre otras cosas, en este ejercicio se:   /
## Se ejemplifica dos alternativas para ajustar modelos     /
##  que involucran transformacion de columnas de la base   /
##  de datos.                                             /
##! -----------------------------------------------------/ 
##                                                      /
##> Profesor: Christian Salas Eljatib                  /
##? E-mail: christian.salas AT uchile DOT cl          /
## Web: https://eljatib.com                          /
##!=================================================/

##* @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##! I. Datos para ejemplo
##* @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#Datos de ahusamiento Eucalyptus regnans
library(biometrics)
data(tapereuca2)
df <- tapereuca2
#?tapereuca2 #ejecutelo en la consola
head(df)
dim(df)


##@@@Fin a seccion de lectura de los datos
head(df)
dim(df)
#revisando la estructura de los datos
df[1:15,]

#ahora veamos los datos del segundo arbol
df[13:29,]
tail(df)
str(df)

#el numero de observaciones
dim(df)
nrow(df)

#cuantos arboles hay en la base de datos?
df$narb <- as.factor(df$narb)
unique(df$narb)
length(unique(df$narb)) #numero de arboles

#graficando los datos
plot(dlsc ~ hl, data = df)

#graficando los datos por arbol
library(lattice)
xyplot(dlsc ~ hl|narb, data = df, xlab="Altura fustal (m)", ylab="Diametro fustal (cm sc.)")
xyplot(dlsc ~ hl|narb, data = df, as.table=T)
xyplot(dlsc ~ hl|narb, data = df, type="o", as.table=T)

xyplot(dlsc ~ hl, groups=narb, data = df, type="o")

#crear variables a ser usadas en el modelo a ajustar
#diametro fustal relativo=(dl/d)
df$dl.d <- df$dlsc/df$dap
#altural fustal relativa transformada=(hl-1.3)/(h-1.3)
df$hl.tr <- (df$htot-df$hl)/(df$htot-1.3)

plot(dl.d ~ hl.tr, data = df)

##Ajuste del modelo
## ln(dl/d)=b0+b1(X)+b2(X^2)
df$var.y <- log(df$dl.d)
df$var.x <- log(df$hl.tr)
df$var.x2 <- df$var.x^2

##hasta ahora hemos visto ajustar este modelo como
m1.old <- lm(var.y ~ var.x + var.x2, data=df)
m1.old
summary(m1.old)

#pero tambien lo podemos hacer mediante
m1 <- lm(var.y ~ var.x + I(var.x^2), data=df)
m1
summary(m1)

coef(m1.old)
coef(m1)

#podemos hacer lo mismo, pero aplicado 
# ahora para la variable respuesta
m1b <- lm(I(log(dlsc/dap)) ~ var.x + I(var.x^2), data=df)
coef(m1.old)
coef(m1)
coef(m1b)

summary(m1b)

##- ===================================
##! Tarea sugerida:
##1. compare los parametros estimados de los objetos m1 y m1.old
##2. ocupe el modelo ajustado para responder a la siguiente pregunta:
##cual es el dlsc, que estima el modelo
## para un arbol de dap=25, htot=23, y hl=6?
##- ===================================




#+╔═════════════════╗
#+║ Fin del script! ║
#+║ Atte.           ║
#+║ El profesor     ║
#+╚═════════════════╝
