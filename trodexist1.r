##! Script: "trodexist1.r"                                          /
##+ Sobre:  Construccion de tabla de rodal y existencia            /
##- Detalles: El script se organiza como sigue:                   /
##  1. Calculo de densidad, area basal y volumen.   *#
##  2. Segrega la variable densidad por clases de   *#
##  diametro (i.e., tabla de rodal).                *#
##  3. Genera grafico de distribucion diametrica.   *#
##  4. Segrega la variable area basal, altura media,*#
##  y volumen por clases de diametro (i.e., tabla de*#
##  rodal y existencias).                           *#
##---------------------------------------------------------/ 
##                                                        /
##> Profesor: Christian Salas Eljatib                    /
## E-mail: christian.salas AT uchile DOT cl             /
## Web: https://eljatib.com                            /
##!===================================================/

##* @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
library(biometrics)
data(eucaplot2)
head(eucaplot2)
?eucaplot2
#~~~~~~~

#asinacion de dataframe a objeto "df"
df<-eucaplot2

#==============================
#1) Calculo de densidad, area basal y volumen
#==============================
#1a) Densidad del rodal (arb/ha)
#i. factor de expansion
df$fe <- 10000/500
head(df)
#ii. la densidad N, en base a la parcela
nha <- sum(df$fe)
nha #en arb/ha

#1b) Area basal del rodal (m2/ha)
#i. area basal de cada arbol
df$g <- (pi/40000)*df$dap^2
#ii. area basal de cada arbol expandida a la hectarea
df$garb.ha <- df$g * df$fe
gha <- sum(df$garb.ha)
gha

#1c) Volumen del rodal (m3/ha)
#i. estimacion de volumen (alternativa simple, factor de forma)
ff <- 0.4 #factor de forma
#ii. calculo del volumen de cada arbol
df$v <- df$g*df$atot*ff
#iii. volumen de cada arbol expandido a la hectarea
df$varb.ha <- df$v * df$fe
vha <- sum(df$varb.ha)
vha

#las variables de rodal son entonces
nha
gha
vha

#==============================
#2) Tabla de rodal
#==============================
#i. sobre amplitud de clases de diametros a emplear
range(df$dap)
diff(range(df$dap))/2
w.amp <- 2
#ii. asignacion de clase diametrica a cada arbol
df$clase.d<-(as.integer((df$dap+((w.amp/2)-0.1))/w.amp))*w.amp
unique(df$clase.d)
sort(unique(df$clase.d))
df

#iii. Creando la tabla rodal
nha.cd<-trodal<-tapply(df$fe, df$clase.d, sum)
trodal
#comparar el resultado de 
sum(trodal)
#....con
nha

#==============================
#3) Grafico de distribucion diametrica
#==============================
barplot(trodal,las=1,col="blue",
        xlab="Clase diametrica (cm)", ylab="Densidad (arb/ha)")
abline(h=0)

#==============================
#4) Area basal, altura media, y volumen
#por clase diametrica
#==============================
#4a) Area basal por clase diametrica
gha.cd<-g.por.clase.diam<-tapply(df$garb.ha,df$clase.d,sum)
gha.cd
#comparar el resultado de 
sum(gha.cd)
#....con
gha

#4b) Volumen por clase diametrica
vha.cd<-v.por.clase.diam<-tapply(df$varb.ha,df$clase.d,sum)
vha.cd
#comparar el resultado de 
sum(vha.cd)
#....con
vha

##Distribucion del volumen por clase diametrica
barplot(vha.cd,las=1,col="yellow",
        ylab="Volumen (m3/ha)", xlab="Clase diametrica (cm)")
abline(h=0)

#4c) Altura media por clase diametrica
hmed.cd<-tapply(df$atot,df$clase.d,mean)
hmed.cd
#calculo de altura media del "rodal"
hmed.tr <- sum(hmed.cd*trodal)/nha
hmed.tr

#==============================
#Bonus: Prepara tabla de rodal y existencia que
# incluya una fila con el total
#==============================
#tabla de rodal y existencias
trodal
cd<-as.numeric(rownames(trodal)) 
tab1<-data.frame(cd,nha.cd,gha.cd,hmed.cd,vha.cd)
tab1
tab2<-data.frame("Total",nha,gha,hmed.tr,vha)
names(tab2) <- names(tab1)
##la tabla de rodal y existencias, con fila de total
tre.total <- rbind(tab1,tab2)
tre.total

#~~~~~~~~
##Guarda esta ultima tabla de rodal y existencia, que incluye 
## una fila con el total, a un archivo fisico. 
## (Activar la siguiente linea)
#write.csv(tre.total,file="tabrodalEx.csv",row.names=FALSE)
#~~~~~~~~

#╔═════════════════╗
#║ Fin del script! ║
#╚═════════════════╝
