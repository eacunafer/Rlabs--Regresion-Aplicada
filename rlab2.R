# ********************************************
# Segundo laboratorio de R
# Inferencia en regresion lineal simple
# Edgar Acuna
# Febrero 20 del 2018
# *********************************************
# Leyendo el conjunto de datos muertes
muertes<-read.table("http://academic.uprm.edu/eacuna/mortalidad.txt",header=T)
# Calculo de la linea de regresion usando el comando lsfit
l1<-lsfit(muertes$porc.inmuniz,muertes$tasa.mort)
# Calculo de la linea de regresion usando el comando lm
l2<-lm(tasa.mort~porc.inmuniz,data=muertes)
l2
# resumiendo los resultados de la linea de regresion
summary(l2)
# Imprimiendo los coeficientes estimados
summary(l2)$coef
beta=summary(l2)$coef[2,1]
eebeta=summary(l2)$coef[2,2]
# Hallando el intervalo de confianza del 95% para la pendiente Beta
bint<-c(beta-qt(.975,18)*eebeta,beta+qt(.975,18)*eebeta)
bint
# analisis de varianza para regresion
anova(l2)
#Hallando el intervalo de confianza para la media y
# el intervalo de prediccion de Y para varios valores de X. 
porc.inmuniz<-c(79,80,89,107)
porc.inmuniz<-as.data.frame(porc.inmuniz)
# calculo del intervalo de confianza para la media de Y
predict(l2,porc.inmuniz,se.fit=T,interval=c("confidence"),level=.99)
# calculo del intervalo de prediccion para y
predict(l2,porc.inmuniz,se.fit=T,interval=c("prediction"),level=.95)
# Haciendo bandas de confianza y bandas de prediccion
porc.inmuniz=seq(0,100,.5)
porc.inmuniz=as.data.frame(porc.inmuniz)
limic=predict(l2,porc.inmuniz,interval=c("confidence"),level=.95)
limip=predict(l2,porc.inmuniz,interval=c("prediction"),level=.95)
limites=cbind(porc.inmuniz,limic[,2:3],limip[,2:3])
plot(limites[,1],limites[,2],xlab="porcentaje de inmunizacion",ylab="tasa de mortalidad",ylim=c(-100,300),type="l",col=2)
title("Bandas de confianza y prediccion")
points(limites[,1],limites[,3],type="l",col=2)
points(limites[,1],limites[,4],type="l",col=4)
points(limites[,1],limites[,5],type="l",col=4)
points(muertes$porc.inmuniz,muertes$tasa.mort)
bandas <- expression("intervalo media", "intervalo prediccion")
legend(-3, 1, bandas, lty=1, col=c(2,4), cex=.7)
abline(l2)
#Usando ggplot2
library(ggplot2)
attach(muertes)
temp_var <- predict(l2, interval="prediction")
new_df <- cbind(muertes, temp_var)
ggplot(new_df, aes(porc.inmuniz, tasa.mort))+
    geom_point() +
    geom_line(aes(y=lwr), color = "red", linetype = "dashed")+
    geom_line(aes(y=upr), color = "red", linetype = "dashed")+
    geom_smooth(method=lm, se=TRUE)+
ggtitle("Bandas de Confianza y Bandas de Prediccion")






