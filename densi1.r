##! Script: "densi1.r"                                              /
##* Sobre:  Curva de tamanho-densidad por IDR                      /
##+ Detalles: Indice de densidad de rodal -- graficos             /
##- Ejemplo: Usa coeficientes de modelo de Reineke para un tipo  /
## de bosques nativos de Chile.                                 /
##-------------------------------------------------------------/ 
##                                                            /
##> Profesor: Christian Salas Eljatib                        /
##? E-mail: christian.salas AT uchile DOT cl                /
## Web: https://eljatib.com                                /
##========================================================/

##* @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##- I. Modelo previamente ajustado
##* @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##- proviene del estudio de Salas-Eljatib & Weiskittel (2018)
## publicado en la revista Ecology and Evolution.
b0.hat <- 12.257; b1.hat <- -1.4742 
q.c <- 25 # diam. clave
qmd <- 5:40 #vector con la variable qmd
n.aju <- exp(b0.hat+ b1.hat *log(qmd)) #valores predichos por el modelo
#grafique este modelo mediante
plot(n.aju ~ qmd, type="l")
idr.test <- seq(600,2200,by=400);
idr.test
#generando las curvas de densidad para cada indice de densidad de rodal
n.aju.1 <- exp( log(idr.test[1]) + b1.hat *(log(qmd)- log(q.c)))
n.aju.2 <- exp( log(idr.test[2]) + b1.hat *(log(qmd)- log(q.c)))
n.aju.3 <- exp( log(idr.test[3]) + b1.hat *(log(qmd)- log(q.c)))
n.aju.4 <- exp( log(idr.test[4]) + b1.hat *(log(qmd)- log(q.c)))
n.aju.5 <- exp( log(idr.test[5]) + b1.hat *(log(qmd)- log(q.c)))
#tenemos 5 vectores, cada un con una curva de IDR
# ahora las uniremos en una dataframe, pero ademas, agregando el qmd
out <- data.frame( qmd, n.aju.1, n.aju.2, n.aju.3, n.aju.4, n.aju.5)
    
out[c(1,2,15:25),]    

matplot(out[,1], out[,2:6], type="l", xlab="Diam. arbol de area basal media (cm)",
          ylab="Densidad (arb/ha)",
          ylim=c(1000,25000))
legend("topright", legend = idr.test, col = 1:5, lty = 1:5,
         title="IDR")
abline(v=q.c,col="brown",lty=2)

#╔═════════════════╗
#║ Fin del script! ║
#║ Atte.           ║
#║ El profesor     ║
#╚═════════════════╝
