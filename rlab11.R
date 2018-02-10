
#***************************************************************
# Laboratorio 11.  Transformacion para estabilizar la varianza
# Edgar Acuna, 
# Abril 2012
#  
#***************************************************************
#Leyendo el archivo de datos de millaje directamente de la internet
#millaje<-read.table(file="http://academic.uprm.edu/eacuna/millaje.txt",header=T)
millaje<-read.table(file="c:/millaje.txt",header=T)
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
#***************************************************************
