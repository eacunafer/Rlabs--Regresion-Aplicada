#***********************************************************************
# Laboratorio 10.  Plots de residuales
# Edgar Acuna, 
# Marzo 2018
#*********************************************************************
#Leyendo el archivo de datos de millaje directamente de la internet
millaje<-read.table(file="http://academic.uprm.edu/eacuna/millaje.txt",header=T)
#millaje<-read.table(file="c:/millaje.txt",header=T)
millaje
#Hallando la matriz de correlaciones
cor(millaje)
# Construyendo la  matriz X, cuya primera columna es de unos 
mx<-millaje[,c(2:5)]
col1<-rep(1,82)
mx<-cbind(col1,mx)
mx<-as.matrix(mx)
#Construyendo la matriz sombrero H
H<-mx%*%solve(t(mx)%*%mx)%*%t(mx)
#Hallando los residuales
n<-dim(mx)[1]
In=diag(0,n,n)
ehat<-(In-H)%*%millaje[,1]
#hallando los plots de residuales
win.graph()
par(mfrow=c(2,2))
plot(mx[,2],ehat,xlab="sp")
plot(mx[,3],ehat,xlab="wt")
plot(mx[,4],ehat,xlab="vol")
plot(mx[,5],ehat,xlab="hp")
#Hallando los residuales sin incluir la primera predictora(sp)
mx1<-mx[,-2]
H1<-mx1%*%solve(t(mx1)%*%mx1)%*%t(mx1)
ehat1<-(In-H1)%*%millaje[,1]
#Hallando los residuales de la primera predictora vs las otras
H1<-mx1%*%solve(t(mx1)%*%mx1)%*%t(mx1)
ehat1r<-(In-H1)%*%mx[,2]
#Hallando los residuales de incluir solo la variable volumen
mvol<-mx[,c(1,4)]
hvol<- mvol%*%solve(t(mvol)%*%mvol)%*%t(mvol)
ehvol<-(In-hvol)%*%millaje[,1]
#Hallando los residuales de peso (wt) versus volumen
ehwt.vol<-(In-hvol)%*%mx[,3]
#Haciendo el plot de regresion parcial de wt versus vol
win.graph()
plot(ehwt.vol,ehvol)
#Hallando los residuales de peso (hp) versus volumen
ehp.vol<-(In-hvol)%*%mx[,5]
#Haciendo el plot de regresion parcial de wt versus vol
win.graph()
plot(ehp.vol,ehvol)
# Cotejando la suposicion de normalidad
l1<-lm(mpg~wt,data=millaje)
summary(l1)
rstint<-rstandard(l1)
win.graph()
 par(mfrow=c(1,3))
 hist(rstint)
 boxplot(rstint)
 qqnorm(rstint)
 qqline(rstint)
#aplicando pruebas noparametricas
shapiro.test(rstint)
ks.test(rstint,"pnorm",0,1)
#Cotejando si la varianza es constante
win.graph()
plot(l1$fitted,rstint)
# Extra: Graficando los puntos y la curva ajustada encima
millaje$z<-1/millaje$wt
l2<-lm(mpg~z,data=millaje)
summary(l2)
win.graph()
#plot(millaje$z,millaje$mpg)
puntosx<-seq(15,60,length=100)
puntosy<-l2$coef[1]+l2$coef[2]/puntosx
plot(millaje$wt,millaje$mpg)
lines(puntosx,puntosy)
