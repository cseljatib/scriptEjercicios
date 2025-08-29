#!==================================================*#
#- Script: "estructura3.r"
#* In short: 'trodalExistMuestreo'                *#
#* Sobre:  Genera una tabla de rodal y existencias  *#
#*  a partir de datos a nivel de arbol en varias    *#
#*  parcelas de muestreo. El script se organiza como*#
#*  sigue:                                          *#
#*  1. Calculo de densidad y area basal por parcela.*#
#*  2. Segrega la variable densidad media por clases*# 
#*  de diametro (i.e., tabla de rodal).             *#            
#*  3. Grafico la distribucion diametrica media.    *#
#*  4. Segrega la variable area basal media por     *#
#*  clases de diametro.                             *#
#*  5. Ajusta un modelo de altura-diametro          *#
#*  6. Predice altura, con modelo anterior, a los   *# 
#*  arboles no-muestra.                             *#
#*  7. Calculo de alt. media y volumen por parcela. *#
#*  8. Segrega la variable altura y volumen por     *#
#*  clases de diametro, para el muestreo.           *#
#*  9. Genera la tabla de rodal y existencias media.*#
#*                                                  *#
#? Profesor: Christian Salas Eljatib                *#
#* E-mail: cseljatib AT gmail DOT com               *#
#* Web: https://eljatib.com                         *#
#!==================================================*#
#~~~~~~~
#Datos a emplear
library(biometrics)
head(radiatapl2)
#?radiatapl2
#~~~~~~~

#asignacion de dataframe a objeto "df"
df<-radiatapl2

#~~~~
#revisando los datos
str(df)
head(df)
names(df)

#~~~~
#cuantas arboles hay?
n.arb <- nrow(df)

#~~~~
#cuantas parcelas hay?
unique(df$parce)
table(df$parce)
#en que se diferencian las dos previas lineas?

n.parcelas<-length(unique(df$parce))
n.parcelas
sup.parce<-150

#==============================
#1) Calculo de N y G por parcela
#==============================
#1a) densidad por parcela
#i. factor de expansion
df$fe <- 10000/sup.parce
head(df)

#ii. la densidad a nivel "de rodal/ha" (N), por parcela
nha.ppar<-tapply(df$fe,df$parce,sum)
nha.ppar

#1b) area basal por parcela
#i. area basal de cada arbol
df$g <- (pi/40000)*df$dap^2
#ii. area basal de cada arbol expandida a la hectarea
df$garb.ha <- df$g * df$fe
#iii. area basal por parcela
gha.ppar<-tapply(df$garb.ha,df$parce,sum)
gha.ppar

#==============================
#1c) Creando una dataframe con los resultados
# de Variables de estado de rodal por parcela
#==============================
#tenemos los siguientes resultados, y los 
# asignaremos a objetos con nombres estandares
N<-nha.ppar;G<-gha.ppar;
Dg<-sqrt((G/N)*(40000/pi))
varest.ppar<-data.frame(N,G,Dg)
varest.ppar

tab.out<-varest.ppar
row.names(tab.out)
Parcela <- row.names(tab.out)
Parcela
tab.out<-cbind(Parcela,varest.ppar)
tab.out
descstat(tab.out[,c("N","G","Dg")],3)

#guardando las medias de las variables de estado de rodal
nha.med <- mean(tab.out$N);nha.med;
gha.med <- mean(tab.out$G);gha.med;
dg.med <- mean(tab.out$Dg);dg.med

#==============================
#2) Tabla de rodal del muestreo (i.e., tabla
# de rodal media)
#==============================
#i. sobre amplitud de clases de diametros a emplear
range(df$dap)
diff(range(df$dap))/2
w.amp <- 2
#ii. asignacion de clase diametrica a cada arbol
df$clase.d<-(as.integer((df$dap+((w.amp/2)-0.1))/w.amp))*w.amp
unique(df$clase.d)
sort(unique(df$clase.d))
length(unique(df$clase.d))

#iii. factor expansion del muestreo, "fe.muestreo" 
n.parcelas
df$fe.muestreo <- df$fe/n.parcelas
df$garb.muestreo <- df$g * df$fe.muestreo
head(df)


#iv. tabla de rodal del muestreo
trodal<-tapply(df$fe.muestreo, df$clase.d, sum)
trodal
#comparar el resultado de 
sum(trodal)
#....con
nha.med
descstat(tab.out[,2:3],3)

nha.cd<-trodal
#==============================
#3) Grafico de distribucion diametrica media
#==============================
barplot(trodal,las=1,col="green",
        xlab="Clase diametrica (cm)", ylab="Densidad (arb/ha)")
abline(h=0)

#==============================
#4) Area basal media del rodal por clase diametrica
#==============================
gha.cd<-tapply(df$garb.muestreo, df$clase.d, sum)
gha.cd
#comparar el resultado de 
sum(gha.cd)
#....con
gha.med
descstat(tab.out[,2:3],3)


#==============================
#5) Ajuste modelo altura-diametro
#==============================
#datos...
descstat(df[,c("dap","atot")])
#5a) Ajuste de modelo de altura
#solo arboles con registro de altura
df1 <- na.omit(df)
head(df1)
#grafico de dispersion arboles muestra
plot(atot~dap,data=df1)
plot(atot~dap,data=df1,xlab="Diametro (cm)",ylab="Altura (m)",
     las=1,col="blue")
#el efecto de la escala en los graficos, compare lo siguiente
plot(atot~dap,data=df1,xlim=c(0,30),     ylim=c(0,25))

n.ajumodh<-nrow(df1); n.ajumodh

##recuerde, dos formas de ajustar un modelo estadistico
#h_i=beta_0 + beta_1 ln(1/(1+(d/10)))+varepsilon_i
#i) alternativa que crea columnas del modelo en los datos
df1$transf.d<-log(1/(1+(df1$dap/10)))
m1.largo<- lm(atot~transf.d,data=df1)
summary(m1.largo)
sigma.hat.1<-summary(m1.largo)$sigma
e.media.gral<-100*sigma.hat.1/mean(df1$atot)
e.media.gral

#ii) alternativa que no necesita que se creen
# las columnas del modelo en los datos
m1 <- lm(atot ~ I(log(1/(1+(dap/10)))), data=df1)
summary(m1)

#==============================
#6) Predecir altura a arboles no muestra
#==============================
#predecir las alturas con el modelo
##modelo dendrometrico: conservar las alturas medidas, y solo
# predecir a las observaciones no medidas.
#como logramos lo anterior?
df$h.aju <- predict(m1, newdata = df)
summary(df$h.aju)
df$h.final <- df$h.aju

#porcion de arboles donde no se midio la altura
df0 <- df[is.na(df$atot),]
dim(df0)
head(df0)
tail(df0)
#porcion de arboles donde si se midio la altura
df1 <- df[!is.na(df$atot),]
dim(df1)
head(df1)
df1$h.final <- df1$atot
#finalmente tenemos el tree list o listado de arboles con todas las variables
# necesarias
trl <- rbind(df0,df1)
dim(trl)

###termino de prediccion segun modelo dendrometrico
# establecido para la altura

summary(trl$h.final)

#==============================
#7. Calculo de alt. media y volumen por parcela.
#==============================
#7a). altura media (Hm) por parcela
hmed.ppar<-tapply(trl$h.final,trl$parce,mean)
hmed.ppar

#7b) volumen por parcela
#i. estimacion de volumen (alternativa simple, factor de forma)
ff <- 0.3 #factor de forma
#ii. calculo del volumen de cada arbol
trl$v <- trl$g*trl$h.final*ff
#iii. volumen de cada arbol expandido a la hectarea
trl$varb.ha <- trl$v * trl$fe
#iv. volumen basal por parcela
vha.ppar<-tapply(trl$varb.ha,trl$parce,sum)
vha.ppar

#variables de estado de rodal por parcela
Hm<-hmed.ppar;V<-vha.ppar
tab.out2<-cbind(tab.out,Hm,V)
tab.out2

descstat(tab.out2[,2:ncol(tab.out2)])

#guardando la media de la altura para el muestreo
h.med <- mean(tab.out2$Hm)

#guardando la media del volumen para el muestreo
vha.med <- mean(tab.out2$V)
tab.out2

#==============================
#8) Tabla de rodal y existencia media
#==============================
#8a) altura media por clase diametrica
hmed.cd<-tapply(trl$h.final, trl$clase.d, mean)
hmed.cd

#8b) Volumen medio del rodal por clase diametrica
trl$varb.muestreo <- trl$v * trl$fe.muestreo
vha.cd<-tapply(trl$varb.muestreo, trl$clase.d, sum)
vha.cd
sum(vha.cd)
#comparar con
vha.med

nha.cd
cd<-as.numeric(rownames(trodal)) 
nha.cd
tab.rodale <- data.frame(cd,nha.cd,gha.cd,hmed.cd,vha.cd)
tab.rodale

#ordenar una dataframe
tab.rodale <- tab.rodale[order(tab.rodale$cd),]
#tab.rodale
#alt. media del rodal
hmed.tr <- sum(tab.rodale$nha.cd*tab.rodale$hmed.cd)/nha.med
hmed.tr
ult.fila <- data.frame(cd="Total",nha.cd=nha.med,gha.cd=gha.med,hmed.cd=hmed.tr,vha.cd=vha.med)
tremedia.total <- rbind(tab.rodale,ult.fila)
tremedia.total
                           
##Preguntas:
# - si se varia el modelo dendrometrico, y se emplee el modelo 
# ajustado de altura, para todos los arboles del muestreo. Como varia
# la estimacion del volumen por parcela, y el medio?
# sus resultados a nivel del muestreo con los obtenidos hasta ahora.
# - ajuste un modelo lineal simple de altura (h=b0+b1d), y compare
# sus resultados a nivel del muestreo con los obtenidos hasta ahora.
# - Calcule la altura dominante para cada parcela.

#╔═════════════════╗
#║ Fin del script! ║
#║ Atte.           ║
#║ El profesor     ║
#╚═════════════════╝
