##! Script: "varestado5.r"                                        /
##- Sobre:  Estimacion (realista) de volumen a nivel de rodal    /
##+ Detalles:  empleando un modelo de altura para los arboles   /
## que no cuentan con la medicion de dicha variable            /
##* Ejemplo: Datos de una parcela de muestreo (data=eucaplotr)/
##? Mas detalles: El script se organiza como sigue:          /
##  1. Calculo de densidad y area basal.                    /
##  2. Identificar arboles muestra.                        /
##  3. Ajuste de modelo de altura-diametro.               /
##  4. Aplicar el modelo dendrometrico establecido.      /
##> Note que en los ejemplos anteriores revisados, se   /
## contaba con todas las alturas de los arboles de la  /
## parcela. Ahora en cambio, dicha variable no esta   /
# disponible para todos los individuos.              /
##! ------------------------------------------------/ 
##                                                 /
##> Profesor: Christian Salas Eljatib             /
##? E-mail: christian.salas AT uchile DOT cl     /
## Web: https://eljatib.com                     /
##!============================================/

##!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##+## I. Datos para ejemplo
##!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
library(biometrics)

#?eucaplotr
#~~~
#Datos a emplear
df<-eucaplotr
head(df)
names(df)[c(1,5)] <- c("dap","atot")

library(datana)
#note que ahora
descstat(df[,c("dap","atot")])
#no todos los arboles tienen medicion de altura

df

#==============================
#1) Calculo de variables N y G
#==============================
#1a. Calculo de la densidad (o "N" segun la iufro)
#i. factor de expansion
df$fe <- 10000/500
head(df)
#ii. la densidad a nivel "de rodal" (N)
nha <- sum(df$fe)
nha #en arb/ha

#1b. Calculo del area basal (o "G" segun la iufro)
#i. area basal de cada arbol
df$g <- (pi/40000)*df$dap^2
head(df)
summary(df$g)

#ii. area basal de cada arbol expandida a la hectarea
df$garb.ha <- df$g * df$fe
head(df)

#iii. el area basal a nivel de "rodal" (G)
gha <- sum(df$garb.ha)
gha

#=====================================
#2) Identificando los arboles muestra
#=====================================
#note que hay arboles con datos vacios en la altura total
summary(df$atot)
dim(df)
nrow(df)
#i. creando una columna identificador de si el arbol es muestra o no de altura
df$muestra.h<-0

#ii. porcion de datos con arboles no-muestra de altura
df.noaju <- df[is.na(df$atot),   ]
dim(df.noaju)
nrow(df.noaju)
#iii. porcion de datos con arboles muestra de altura
df.aju <- df[!is.na(df$atot),   ]
dim(df.aju)
nrow(df.aju)
#en estos ultimos, volveremos a definir la columna muestra.h, para que
# sea 1, y asi nos ayude a identificar los arboles muestra y no-muestra
df.aju$muestra.h<-1


#iv. volvemos a crear la dataframe con ambos subset de datos
df<-rbind(df.aju,df.noaju)
dim(df)

table(df$muestra.h)

#=====================================
#3) Ajuste de un modelo de altura-diametro
#=====================================
nrow(df.aju)
#i. graficando
plot(atot~dap, data=df.aju)
#compare el grafico anterior con el siguiente, ve alguna diferencia?
plot(atot~dap, data=df.aju, xlim=c(0,40),ylim=c(0,40),las=1)

#ii. ajustando el modelo
#h_i=beta_0+beta_1*d_i+varepsilon_i
mod.rls <- lm(atot~dap, data=df.aju)
b0.hat<-coef(mod.rls)[1];b1.hat<-coef(mod.rls)[2];
b0.hat
b1.hat

#iii. prediciendo la altura
df$h.pred <- b0.hat + b1.hat*df$dap
head(df[,c("dap","atot","muestra.h","h.pred")])


#=====================================
#4) Aplicando el modelo dendrometrico establecido
#=====================================
#solo emplearemos la prediccion de la altura para los arboles
# que no cuentan con altura medida, es decir, los no-muestra.

# Es decir, para implementar lo anterior debemos hacerlo de forma
# diferenciada

#ii. se crea la columna h.final que contendra la altura que se empleara
# en los calculos posteriores a ser realizados (e.g., estimacion de volumen)
df$h.final <- df$h.pred

#iii.volvemos a filtrar
df.1 <- subset(df, muestra.h==1)
df.0 <- subset(df, muestra.h==0)
#iii.a) revisemos si el modelo dendrometrico esta correctamente
# aplicado para el set de datos de arboles muestra
df.1[,c("dap","atot","muestra.h","h.pred","h.final")]

df.1$h.final <- df.1$atot #mantenemos lo observado, y no lo estimado
df.1[,c("dap","atot","muestra.h","h.pred","h.final")]

#iii.v) revisemos si el modelo dendrometrico esta correctamente
# aplicado para el set de datos de arboles no-muestra
df.0[,c("dap","atot","muestra.h","h.pred","h.final")]

##iv. Finalmente, unimos todo en la dataframe final

df<-rbind(df.1,df.0)
df[,c("dap","atot","muestra.h","h.pred","h.final")]
#aca se observa que solo se emplean las alturas predichas
# para aquellos arboles a los cuales no se les midio
# la altura en terreno


##volumen
## usaremos un factor de forma
## ff=v/v_r
## v=ff * v_r 
ff <- 0.3 #factor de forma
#luego el volumen de cada arbol
df$v <- df$g*df$h.final*ff
#el volumen de cada arbol expandido a la hectarea
df$varb.ha <- df$v * df$fe
vha <- sum(df$varb.ha)
vha

##ahora estimaremos el volumen, pero no mediante
# un factor de forma, que es una simplificacion, sino
# que mediante una funcion de volumen
df$v2 <- 0.0195213+0.0000282*(df$dap^2*df$h.final) 
df$varb2.ha <- df$v2 * df$fe

vha2 <- sum(df$varb2.ha)
vha2
#compare este volumen con el anterior calculado
c(vha,vha2)

##+ ===============================
##*Tarea, preguntas, y asi reforzar lo estudiado:
## - en cuanto varia el volumen agregado (V), si se emplea un modelo
## de altura-diametro de la siguiente forma h=b0+b1(1/d)?
##+ ===============================

#?╔═════════════════╗
#?║ Fin del script! ║
#?╚═════════════════╝
