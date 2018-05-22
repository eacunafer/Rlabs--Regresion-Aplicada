#***********************************************************************
# Laboratorio 17:  Seleccion de variables 
# a) Usando el metodo forward. Hace uso de la libreria leaps y de la 
# funcion regsubsets de dicha libreria
#  b) Seleccion de los mejores subconjuntos usando leaps y step con 
# el criterio AIC. Usa la funcion selforw
# Edgar Acuna,
# Mayo 2018
#*********************************************************************
#Leyendo el archivo de datos de grasa directamente de la internet
grasa<-read.table(file="http://academic.uprm.edu/eacuna/grasa.txt",header=T)
#grasa<-read.table(file="c:/grasa.txt",header=F)
# El numero maximo de variables a entrar sera igual al numero de 
# predictoras del conjunto original
maxvar<-dim(grasa)[2]
#
#llamando a la libreria leaps
library(leaps)
######################################################################
#Aplicando el metodo forward
######################################################################
#freg<-regsubsets(grasa~., data=grasa,method="forward",nvmax=maxvar)
#Mostrando la salida de todos los pasos con la estadisticas respectiva
selforw(grasa[,2:14],grasa[,1],.15)
#Hallando el mejor subconjunto usando stepwise y el criterio AIC
l1<-lm(grasa~1,data=grasa)
lall<-lm(grasa~.,data=grasa)
step(l1,scope=list(lower=l1,upper=lall),direction="forward")
######################################################################
#Aplicando el metodo Backward
######################################################################
backelim(grasa[,2:14],grasa[,1],.15)
step(lall,direction="backward")
###########################################################################
# Aplicando el metodo de los mejores subconjuntos
###########################################################################
#matrix  de predictoras
grasa.x<-grasa[,2:14]
#vector de respuesta
grasa.y<-grasa[,1]
#nombres de las variables predictoras
nombres<-colnames(grasa.x)
leaps(grasa.x,grasa.y,method="r2",nbest=2,names=nombres)
leaps(grasa.x,grasa.y,method="adjr2",nbest=2,names=nombres)
#Mejor modelo usando Cp de mallows
bcp<-leaps(grasa.x,grasa.y,method="Cp",nbest=1,names=nombres)
bcp$Cp
p<-2:maxvar
plot(p,bcp$Cp,type="l")
title("Grafica del Cp de Mallows segun el tamano del modelo")
lines(2:maxvar,2:maxvar)
######################################################################################################
backelim=function(x,y,alpha){
# Hace forward elimination using el paquete leaps 
# x es la matriz o data frame de variables independientes
# y es el vector de respuestas
require(leaps)
m<-ncol(x)                           # numero de variables independientes
n<-nrow(x)                           # tamano de la muestra
vm<-1:m
x=as.matrix(x)                    # en caso de que x sea data frame
pvmin<-rep(0,m)
regsubsets(x,y,method="backward")->out.x
pv.orig<-1-pf((out.x$rss[vm]-out.x$rss[vm+1])*(n-vm-1)/out.x$rss[vm+1],1,n-vm-1)
# sequential min of p-values down from full model
for (i in m:1){pvmin[i]<-min(pv.orig[m:i])}
sigma2=out.x$sserr/(n -1+ out.x$intercept - out.x$last)
cat("Eliminacion hacia atras",fill=T)
cat("",fill=T)
out=data.frame(p=c(vm,m+1),nvar=c(vm-1,m),rem.var=c(NA,colnames(x)[out.x$vorder-1]),
         pvmin=round(c(NA,pvmin),4),
          s=round(sqrt(out.x$rss/(n-c(vm,m+1))),3),
        r2=round(1-(out.x$rss/out.x$nullrss),3),
        r2adj=round(1-(out.x$rss/out.x$nullrss)*(n-1)/(n-c(vm,m+1)),3),
        Cp=round((out.x$rss/sigma2)-n+2*c(vm,m+1),3)
)
a=length(pvmin[pvmin>alpha])
cat("p=numero de coeficientes en el modelo",fill=T)
cat("nvar=p-1=numero de variables predictoras",fill=T)
cat("rem.var=la variable a ser removida, el modelo actual no incluye",fill=T)
cat("        esta variable",fill=T)
cat("pvmin=pvalue de la F parcial correspondiente a la variable menos importante en cada paso",fill=T)
cat("",fill=T)
out1=cbind(out[rev(((m+2)-a):(m+1)),1:4],out[rev(((m+1)-a):m),5:8])
print(out1)
}
#############################################################################
forwabic=function(x,y,alpha){
# Hace seleccion  forward usando el paquete leaps
# x es uma matriz o data frame de variables independentes
# y es el vector de respuestas
#alpha es el nivel de significacion para la f-parcial
# 
require(leaps)
m=ncol(x)            # numero de variables independientes
n=nrow(x)            # tamano de muestra
vm=1:m
x=as.matrix(x)      # si x es una data frame
pvmax<-rep(0,m)
out.x=regsubsets(x,y,nbest=2, method="forward")
pv.orig<-1-pf((out.x$rss[vm]-out.x$rss[vm+1])*(n-vm-1)/out.x$rss[vm+1],1,n-vm-1)
for (i in 1:m){pvmax[i]<-max(pv.orig[1:i])}  # sequential max of pvalues
cat("Seleccion Forward",fill=T)
cat("",fill=T)
out<-data.frame(p=c(vm,m+1),nvar=c(vm-1,m),add.var=c(NA,colnames(x)[out.x$vorder-1]),
         pvmax=round(c(NA,pvmax),4),
  aic=round(n * log(out.x$rss/n) +  2*c(vm,m+1),3),
   bic=round(n * log(out.x$rss/n) +  c(vm,m+1)*log(n),3),
   gcv=round(n*out.x$rss/((n-c(vm,m+1))^2),4)
)
a=length(pvmax[pvmax<alpha])
cat("p=numero de coeficientes en el modelo, p=1 es por el  intercepto",fill=T)
cat("nvar=p-1=numero de variables  predictoras",fill=T)
cat("add.var=la variable que ha sido anadida al modelo actual",fill=T)
cat("pvmax=p-value de F-parcial correspondiente a la variable mas importante en cada paso",fill=T)
cat("",fill=T)
print(out[2:(a+1),])
}
#################################################################
selforw=function(x,y,alpha){
# Hace seleccion  forward usando el paquete leaps
# x es uma matriz o data frame de variables independentes
# y es el vector de respuestas
#alpha es el nivel de significacion para la f-parcial
# 
require(leaps)
m=ncol(x)            # numero de variables independientes
n=nrow(x)            # tamano de muestra
vm=1:m
x=as.matrix(x)      # si x es una data frame
pvmax<-rep(0,m)
out.x=regsubsets(x,y,method="forward")
pv.orig<-1-pf((out.x$rss[vm]-out.x$rss[vm+1])*(n-vm-1)/out.x$rss[vm+1],1,n-vm-1)
for (i in 1:m){pvmax[i]<-max(pv.orig[1:i])}  # sequential max of pvalues
cat("Seleccion Forward",fill=T)
cat("",fill=T)
sigma2=out.x$sserr/(n -1+ out.x$intercept - out.x$last)
out<-data.frame(p=c(vm,m+1),nvar=c(vm-1,m),add.var=c(NA,colnames(x)[out.x$vorder-1]),
         pvmax=round(c(NA,pvmax),4),
        s=round(sqrt(out.x$rss/(n-c(vm,m+1))),3),
        r2=round(1-(out.x$rss/out.x$nullrss),3),
        r2adj=round(1-(out.x$rss/out.x$nullrss)*(n-1)/(n-c(vm,m+1)),3),
        Cp=round((out.x$rss/sigma2)-n+2*c(vm,m+1),3)
)
a=length(pvmax[pvmax<alpha])
cat("p=numero de coeficientes en el modelo, p=1 es por el  intercepto",fill=T)
cat("nvar=p-1=numero de variables  predictoras",fill=T)
cat("add.var=la variable que ha sido anadida al modelo actual",fill=T)
cat("pvmax=p-value de F-parcial correspondiente a la variable mas importante en cada paso",fill=T)
cat("",fill=T)
print(out[2:(a+1),])
}
#****************************************************************************
