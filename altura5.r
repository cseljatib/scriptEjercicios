##! Script: "altura5.r"                                            /
##- Sobre:  Otra forma de ajustar modelos lineales con variables  /
## transformadas                                                 /
##+ Detalles:  Emplea estimador de minimos cuadrados.           /
##* Ejemplo: Datos de altura-diametro (data= biomass2).        /
##? Mas detalles: Compara la forma "larga" y una nueva, que   /
## es mas corta, para ajustar modelos con variables          /
## trasformadas, ademas                                     /
##   + calcula valores predichos.                          /
##   + calcula estadisticos RMSD, DIFA y DA.              /
##! -----------------------------------------------------/ 
##                                                      /
##> Profesor: Christian Salas Eljatib                  /
##? E-mail: christian.salas AT uchile DOT cl          /
## Web: https://eljatib.com                          /
##!=================================================/


##* @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##! I. Datos para ejemplo
##* @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
library(biometrics)
data(biomass2)
df <- biomass2
#?biomass2 #ejecutelo en la consola
head(df)
table(df$spp)
##- Seleccionemos una sola especie
df<-subset(df, spp=="White birch")
dim(df)

##-Estadistica descriptiva
library(datana)
descstat(df[,c("dap","atot")])

##* @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##! II. Graficos de interes
##* @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##-Distribucion
hist(df$atot)
hist(df$dap)

##-Dispersion
plot(atot~dap, data=df)
plot(atot~dap, data=df, xlab="Diametro (cm)",
ylab="Altura (m)")
plot(atot~dap, data=df)


##* @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##! III. Ajuste de modelos, ya ajustados
##+ ========================================
##? Modelo 1 (inverso del diametro)
##  h_i=beta_0+beta_1(1/d_i)+varepsilon_i
##+ ========================================
##-(i) Alternativa "larga"
## Necesita crear en los datos la variable X necesaria
## para el modelo
df$inv.d <- 1/df$dap
mod1.old<- lm(atot~inv.d, data=df)
summary(mod1.old)

##-(ii) Alternativa "corta"
## No cecesita crear en los datos la variable X necesaria
## para el modelo
mod1<- lm(atot~I(1/dap), data=df)
summary(mod1)

##- Compare ambos modelos ajustados
coef(mod1.old)
coef(mod1)

##? son ambos objetos el mismo modelo estadistico?
##? Cual de las dos alternativas es mas simple?

##+ ========================================
##? Modelo 2 
##  ln(h_i)=beta.0+beta.1*e^(-0.03d_i)+varepsilon_i
##+ ========================================
##-(i) Alternativa "larga"
## Necesita crear en los datos las variables X e Y necesarias
## para el modelo 2.
df$ln.h<-log(df$atot)
df$exp.d<-exp(-0.03*df$dap)

mod2.old<- lm(ln.h~exp.d, data=df)
summary(mod2.old)

##-(ii) Alternativa "corta"
## No cecesita crear en los datos la variable X necesaria
## para el modelo
mod2<- lm(I(log(atot))~I(exp(-0.03*dap)), data=df)
summary(mod2)

##- Compare ambos modelos ajustados
coef(mod2.old)
coef(mod2)

##? son ambos objetos el mismo modelo estadistico?
##? Cual de las dos alternativas es mas simple?


##* @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##! IV. Calculando valores ajustadas de altura para cada modelo
##* @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##-(i) Alternativa "larga"
## Ya se reviso antes, implica guardar los coeficientes
## estimados y escribir la expresion matematica respectiva
##+ para el modelo 1
df$aju1.old<-coef(mod1.old)[1]+coef(mod1.old)[2]*(1/df$dap)

##+ para el modelo 2
df$aju2.old<-exp(
    coef(mod2.old)[1]+ (coef(mod2.old)[2]*(exp(-0.03*df$dap)))
                 )

head(df[,c("dap","atot","aju1.old","aju2.old")])

##-(ii) Alternativa "corta"
## Empleando los modelos ajustados que no tienen las variables
## transformadas en la dataframe.

##+ para el modelo 1
df$aju1<-fitted(mod1)

##+ para el modelo 2
df$aju2<-exp(fitted(mod2))


head(df[,c("dap","atot","aju1","aju1.old","aju2","aju2.old")])

##? son las predicciones diferentes para cada objeto de
## un mismo modelo estadistico?, o son iguales?
##? Cual de las dos alternativas es mas simple para obtener las
## predicciones o valores ajustados?



##* @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##! V. Calculo estadisticos de prediccion
##* @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

valesta(y.obs=df$atot,y.pred=df$aju1)
valesta(y.obs=df$atot,y.pred=df$aju2)


##* @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##! Tarea sugerida:
## 1. Prepare un cuadro en una hoja a mano, y escriba los
## parametros estimados de cada modelo  (cada fila un modelo).
## 2. Prepare otro cuadro en una hoja a mano, y escriba los
## estadisticos de prediccion de cada modelo  (cada fila un modelo).
## 3. Compare ambos modelos, basado en los estadisticos de
##    prediccion calculados.
##* @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@


##* @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##! Bonus!!:
## Una vez que realice lo anterior, podria
## comparar su segundo cuadro con lo siguiente, que emplea
## la funcion valestamod() del paquete datana
df.ajumod1<-data.frame(altura.obs=df$atot,altura.pred=df$aju1,modelo="Modelo 1")
df.ajumod2<-data.frame(altura.obs=df$atot,altura.pred=df$aju2,modelo="Modelo 2")
df.ajusmodels<-rbind(df.ajumod1,df.ajumod2)
##- Aplicando la funcion para generar una tabla comparativa
valestamod(data=df.ajusmodels,y.obs="altura.obs",y.pred="altura.pred", model="modelo")

##* @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

#>╔═════════════════╗
#>║ Fin del script! ║
#>║ Atte.           ║
#>║ El profesor     ║
#>╚═════════════════╝
