
#****************************************************************************
# Laboratorio 11.  Transformacion para estabilizar la varianza
# Edgar Acuna, 
# Abril 2012
#  
#****************************************************************************
#Leyendo el archivo de datos de millaje directamente de la internet
millaje<-read.table(file="http://academic.uprm.edu/eacuna/millaje.txt",header=T)
#millaje<-read.table(file="c:/millaje.txt",header=T)
l1<-lm(mpg~.,data=millaje)
# Agrupando la variable de respuesta mpg en intervalos
nclases<-nclass.scott(millaje$mpg)
clases<-cut(millaje$mpg,nclases)
dmilla<-cbind(millaje$mpg,clases)
#Hallando la potencia de la transformacion
medias<-rep(0,nclases)
vars<-rep(0,nclases)
for(j in 1:nclases)
{medias[j]<-mean(dmilla[dmilla[,2]==j,])
vars[j]<-var(as.vector(dmilla[dmilla[,2]==j,]))
}
lmeans<-log(medias)
lvars<-log(vars)
lsfit(lmeans,lvars)
# El lsfit indica que la varianza es proporcional a la media al cuadrado
# una transformacion logaritmica en la variable de respuesta es recomendada 
mpglog<-log(millaje$mpg)
millaje1<-cbind(millaje,mpglog)
l2<-lm(mpglog~sp+wt+vol+hp,data=millaje1)
summary(l1)
summary(l2)
# Considerando que la varianza es proporcional a la media al cubo
# una transformacion  h(y)=y^-0.5 es realizada 
mpg05<-millaje$mpg^-0.5
millaje2<-cbind(millaje,mpg05)
l3<-lm(mpg05~sp+wt+vol+hp,data=millaje2)
summary(l3)
win.graph()
par(mfrow=c(1,3))
plot(l1$fitted,rstandard(l1),main="sin Transformacion",ylim=c(-3,3))
abline(h=0)
plot(l2$fitted,rstandard(l2),main="var~e(y)^2,w=log(y)",ylim=c(-3,3))
abline(h=0)
plot(l3$fitted,rstandard(l3),main="Var~E(y)^3, w=y^-1/2",ylim=c(-3,3))
abline(h=0)
#***************************************************************************


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


#***********************************************************************
# Laboratorio 14.  Minimos cuadrados ponderados
#  Edgar Acuna, 
#  Marzo 2012
#  
#*********************************************************************
#Leyendo el archivo de datos de millaje directamente de la internet
millaje<-read.table(file="http://math.uprm.edu/~edgar/millaje.txt",header=T)
#millaje<-read.table(file="c:/millaje.txt",header=T)
#
#Extrayendo la primera y ultima observacion y consideramdo solo
#las columnas de variable de respuesta(1) y de wt(3).
#
millaje1<-millaje[-c(1,82),c(1,3)]
win.graph()
plot(millaje1[,2],millaje1[,1])
#Agrupando los valores de x
tabla<-table(millaje1[,2])
valsx<-dimnames(tabla)
valsx<-as.double(unlist(valsx))
nvalsx<-length(valsx)
#Calculando la varianza de cada grupo
varsg<-rep(0,nvalsx)
for(i in 1:nvalsx)
{grupox<-millaje1[millaje1[,2]==valsx[i],]
varsg[i]<-var(grupox[,1])
}
varsg
#Asignando los pesos a cada observacion (metodo I)
win.graph()
plot(varsg,valsx)
ndatos<-dim(millaje1)[1]
pesos<-rep(0,ndatos)
for(i in 1:ndatos)
{for(j in 1:nvalsx)
{if(millaje1[i,2]==valsx[j])
{pesos[i]<-1/varsg[j]}
}
}
pesos
#resultados de regresion sin ponderar
lw1<-lm(mpg~.,data=millaje1,weights=pesos)
summary(lw1)
#Asignando los pesos a cada observacion (metodo II)
valsx2<-valsx^2
s2<-lm(varsg~valsx+valsx2)
s2
valsx<-millaje1[,2]
valsx2<-millaje1[,2]^2
nuevo<-data.frame(valsx,valsx2)
pesos1<-predict.lm(s2,nuevo)
pesos1<-1/pesos1
pesos1
#resultados de regresion ponderada
lw2<-lm(mpg~.,data=millaje1,weights=pesos1)
summary(lw2)
#************************************************
#Aplicando regresion ponderada con la variable sp
#************************************************
nclases<-nclass.scott(millaje[,2])
clases<-cut(millaje[,2],nclases)
millaje2<-cbind(millaje[,1],clases)
#Calculando la varianza de cada grupo
varsg2<-rep(0,nclases)
for(i in 1:nclases)
{grupox<-millaje2[millaje2[,2]==i,]
varsg2[i]<-var(grupox[,1])
}
varsg2
#Asignando los pesos a cada observacion 
win.graph()
plot(varsg2,1:nclases)
nd<-dim(millaje)[1]
pesos2<-rep(0,nd)
for(i in 1:nd)
{for(j in 1:nclases)
{if(millaje2[i,2]==j)
{pesos2[i]<-1/varsg2[j]}
}
}
pesos2
#Resultados usando regresion ponderada
lw3<-lm(mpg~sp,data=millaje[,c(1,2)],weights=pesos2)
summary(lw3)
#Resultados sin usar regresion ponderada
lw4<-lm(mpg~sp,data=millaje)
summary(lw4)
#****************************************************************************


