##! Script: "trodal.r"                                              /
##+ Sobre:  Construccion de tabla de rodal y grafico distribucion  /
##  diametrica.                                                   /
##- Detalles: El script se organiza como sigue: calculo de       /
##  densidad; segrega la variable anterior por clases de        /   
##  diametro, o clases diametricas (tabla de rodal); y crea    /  
##  grafico de distribucion diametrica.                       /
##? Ejemplo: Emplea datos provenientes de una parcela de     /
##  muestreo (dataframe= eucaplot2).                        /
##---------------------------------------------------------/ 
##                                                        /
##> Profesor: Christian Salas Eljatib                    /
## E-mail: christian.salas AT uchile DOT cl             /
## Web: https://eljatib.com                            /
##!===================================================/

##* @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##- I. Datos
##* @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
library(biometrics)
head(eucaplot2)
#Activar siguiente linea para ver metadata
#?eucaplot2
df <- eucaplot2
df
dim(df)


#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
### II. Calculo de la densidad (o "N" segun la IUFRO)
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#i. factor de expansion
df$fe <- 10000/500
head(df)
#ii. la densidad a nivel "de rodal" (N)
nha <- sum(df$fe)
nha #en arb/ha

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
### III. Construccion tabla de rodal
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
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
trodal<-tapply(df$fe, df$clase.d, sum)
trodal
#comparar el resultado de 
sum(trodal)
#....con
nha

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
### IV. Grafico de distribucion diametrica
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
barplot(trodal,las=1, xlab="Clase diametrica (cm)",
        ylab="Densidad (arb/ha)")
abline(h=0)



#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
### V. Bonus!!
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#==============================
#Prepara tabla de rodal que
# incluya una fila con el total
#==============================
nha.cd<-trodal
clase.d<-as.numeric(rownames(trodal)) 
tab1<-data.frame(clase.d,nha.cd)
tab1
tab2<-data.frame("Total",nha)
names(tab2) <- names(tab1)
trodal.total <- rbind(tab1,tab2)
trodal.total

#~~~~~~~~
##Guarda esta ultima tabla de rodal, que incluye 
## una fila con el total, a un archivo fisico. 
## (Activar la siguiente linea)
#write.csv(trodal.total,file="tabrodal.csv",row.names=FALSE)
#~~~~~~~~


#╔══════════════════════╗
#║ Fin del script:      ║
#║ DisfRute el ejemplo! ║
#║ El profesor     ╔════╝
#╚═════════════════╝
