#***************************************************************************
# Laboratorio  7: Inferencia en Regresion lineal multiple
# Edgar Acuna, 
# Marzo 2018
#****************************************************************************
#Leyendo el archivo de datos de millaje directamente de la internet
millaje<-read.table(file="http://academic.uprm.edu/eacuna/millaje.txt",header=T)
#millaje<-read.table(file="c:/millaje.txt",header=T)
l1<-lm(mpg~.,data=millaje)
#Hallando los p-values para las pruebas de hipotesis
summary(l1)
anova(l1)
#Modelo reducido para probar B(vol)=B(hP)=0
l2<-lm(mpg~sp+wt, data=millaje)
anova(l2)
p=dim(millaje)[2]-1
n=dim(millaje)[1]
k=2
#Suma de cuadrados de regresion del modelo completo
a=sum(anova(l1)$Sum[-(p+1)])
#Suma de cuadrados de regresion del modelo reducid
b=sum(anova(l2)$Sum[-(k+1)])
#Cuadrado Medio del error del modelo completo
c=anova(l1)$Mean[p+1] 
#Calculo del F parcial
fp<-((a-b)/2)/c
fp
#Hallando el percentil de la F con alpha=.05
qf(.95,k,n-p-1)
#hallando el intervalo de confianza del 95% para el valor medio
sp<-100
wt<-20
vol<-90
hp<-50
nuevo<-as.data.frame(cbind(sp,wt,vol,hp))
nuevo
predict.lm(l1,nuevo,se.fit=T,interval=c("confidence"),level=.95)
#Hallando el ntervalo de prediccion del 99% para los mismos datos
predict.lm(l1,nuevo,se.fit=T,interval=c("prediction"),level=.99)
#*********************************************************************************

