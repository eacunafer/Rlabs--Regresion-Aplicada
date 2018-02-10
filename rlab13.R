#****************************************************************************
# Laboratorio 13.  Transformacion de Box y Cox para remediar la falta de 
#                  normalidad.
#  Edgar Acuna,
#  Marzo 2012
#  
#*********************************************************************
#Leyendo el archivo de datos de millaje directamente de la internet
#millaje<-read.table(file="http://academic.uprm.edu/eacuna/millaje.txt",header=T)
millaje<-read.table(file="c:/millaje.txt",header=T)
# Hallando el modelo de regresion ajustado
l1<-lm(mpg~.,data=millaje)
summary(l1)
#Cargando la libreria MASS que contiene la funcion boxcox
library(MASS)
#Haciendo la transformacion
bc<-boxcox(l1,lambda=seq(-.6,.6,length=20),plotit=T)
#Transformando la respuesta
millaje1<-millaje
millaje1$mpg<-((millaje$mpg)^-0.22-1)/-0.22
l2<-lm(mpg~.,data=millaje1)
summary(l2)
# Viendo el efecto de la transformacion Box-Cox
rstint<-rstandard(l1)
rstint1<-rstandard(l2)
win.graph()
 par(mfrow=c(2,3))
 hist(rstint)
 boxplot(rstint)
title("Antes de la transformacion")
 qqnorm(rstint)
 qqline(rstint)
hist(rstint1)
 boxplot(rstint1)
title("Despues de la tranformacion")
 qqnorm(rstint1)
 qqline(rstint1)
#****************************************************************************
