#*************************************************************************
# Laboratorio 12.  Transformacion de Box y Tidwell en un modelo de lineal 
#                 multiple.
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
betas<-l1$coeff
#Excluyendo el intercepto del vector de betas
betas1<-betas[-1]
#hallando las nuevas variables zetas
z1<-millaje$sp*log(millaje$sp)
z2<-millaje$wt*log(millaje$wt)
z3<-millaje$vol*log(millaje$vol)
z4<-millaje$hp*log(millaje$hp)
millaje1<-cbind(millaje,z1,z2,z3,z4)
l2<-lm(mpg~.,data=millaje1)
betas2<-l2$coeff
gammas<-betas2[c(6:9)]
#Hallando los alfas
alfas<-(gammas/betas1)+1
alfas
#Creando las nuevas variables
sp1<-millaje1$sp^alfas[1]
wt1<-millaje1$wt^alfas[2]
vol1<-millaje1$vol^alfas[3]
hp1<-millaje1$hp^alfas[4]
#regresion con todas las variables transformadas
l3<-lm(millaje1$mpg~sp1+wt1+vol1+hp1)
summary(l3)
#
#Haciendo nuevamente la transformacion desde el inicio pero sin usar vol
millaje2<-millaje[,-4]
l11<-lm(mpg~.,data=millaje2)
summary(l11)
betas11<-l11$coeff
#Excluyendo el intercepto del vector de betas1
betas12<-betas11[-1]
#hallando las nuevas variables zetas
z11<-millaje2$sp*log(millaje2$sp)
z21<-millaje2$wt*log(millaje2$wt)
z31<-millaje2$hp*log(millaje2$hp)
millaje2<-cbind(millaje2,z11,z21,z31)
l21<-lm(mpg~.,data=millaje2)
betas22<-l21$coeff
gammas1<-betas22[c(5:7)]
#Hallando los alfas1
alfas1<-(gammas1/betas12)+1
alfas1
#Creando las nuevas variables
sp11<-millaje2$sp^alfas1[1]
wt11<-millaje2$wt^alfas1[2]
hp11<-millaje2$hp^alfas1[3]
l5<-lm(millaje2$mpg~sp11+wt11+hp11)
summary(l5)
#Haciendo la regresion con solo las dos variables significativas
l6<-lm(millaje2$mpg~wt11+hp11)
summary(l6)
#****************************************************************************
