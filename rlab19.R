#****************************************************************************
#Laboratorio 19: Diagnosticos de Multicolineralidad: VIF y Numero condicion y  Regression Ridge
# Usa la libreria MASS y nuestra funcion acunaridge
# Edgar Acuna,
# Mayo 2014
#*************************************************
library(MASS)
# Ejemplo 1: Conjunto de datos millaje
millaje<-read.table(file="http://academic.uprm.edu/millaje.txt",header=T)
# Hallando la matriz de correlacion de las predictoras
mcor<-cor(millaje[,2:5])
#Hallando los VIFs 
vif<-diag(solve(mcor))
cat("los VIF's son:\n")
vif
#recalculando los VIF's sin la variable hp
diag(solve(cor(millaje[,2:4])))
#Hallando el numero condicion
ev<-eigen(cor(millaje[,2:5]))
evals<-ev$values
evals
cond<-sqrt(evals[1]/ev$values[4])
cond
#Hallando la regresion ridge
rr1<-acunaridge(mpg~.,data=millaje,lambda=seq(0,0.1,.01))
rr1
#Haciendo la Traza Ridge
matridge<-cbind(rr1$lambda,t(rr1$coef[-1,]))
win.graph()
plot(matridge[,1],matridge[,2],ylim=c(-2,0.5),type="l",xlab="lamb",ylab="coefs",col=2)
title("La traza Ridge para Millaje")
lines(matridge[,1],matridge[,3],col=3)
lines(matridge[,1],matridge[,4],col=4)
lines(matridge[,1],matridge[,5],col=6)

#********************************************************
#Ejemplo 2:Conjunto de datos pollution 
pollution<-read.table(file="http://math.uprm.edu/~edgar/pollution.dat",header=F)
# Hallando la matriz de correlacion de las predictoras
mcor2<-cor(pollution[,1:15])
#Hallando los VIFs 
vif2<-diag(solve(mcor2))
cat("los VIF's son:\n")
vif2
#recalculando los VIF's sin las variable 12 y 13
diag(solve(cor(pollution[,c(1:11,14:15)])))
#Hallando el numero condicion
ev2<-eigen(cor(pollution[,1:15]))
evals2<-ev2$values
evals2
cond2<-sqrt(evals2[1]/evals2[15])
cond2
# Hallando la regression ridge 
rr2<-acunaridge(V16~.,data=pollution,lambda=seq(0,.5,.05))
rr2
#Haciendo la Traza Ridge
matridge2<-cbind(rr2$lambda,t(rr2$coef[-1,]))
win.graph()
plot(matridge2[,1],matridge2[,2],ylim=c(-20,15),type="l",xlab="lamb",ylab="coefs",col=2)
title("La Traza Ridge para Pollution")
for(i in 3:16)
{
lines(matridge2[,1],matridge2[,i],col=i)
}
#************************************************************************************
