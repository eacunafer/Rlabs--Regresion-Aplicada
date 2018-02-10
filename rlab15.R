#***********************************************************************
# Laboratorio 15. Regresion con variables predictoras cualitativas
#  Edgar Acuna, 
#  Marzo 2018
#*********************************************************************
#Leyendo el archivo de datos de millaje directamente de la internet
pesobebe<-read.table(file="http://academic.uprm.edu/eacuna/bajopeso.txt",header=T)
#pesobebe<-read.table(file="c:/pesobebe.txt",header=T)
pbebe<-pesobebe[,10]
pmama<-pesobebe[,3]
fuma<-pesobebe[,5]
#regresion sonsiderando que la  relacion entre pesomama y pesobebe no
# es afectada por la variable fumar
l1<-lm(pbebe~pmama)
summary(l1)
#regresion considerando que fumar puede afectar la relacion pesomama-pesobebe
l2<-lm(pbebe~pmama+fuma) 
summary(l2)
#haciendo una regresion separada para cada grupo
pbebe0<-pbebe[fuma==0]
pmama0<-pmama[fuma==0]
win.graph()
plot(pmama0,pbebe0)
l3<-lm(pbebe0~pmama0)
summary(l3)
pbebe1<-pbebe[fuma==1]
pmama1<-pmama[fuma==1]	
win.graph()
plot(pmama1,pbebe1)
l4<-lm(pbebe1~pmama1)
summary(l4)
#Probando la hipotesis de pendientes iguales
gl0<-summary(l3)$df[2]
gl1<-summary(l4)$df[2]
var0<-summary(l3)$sigma^2
var1<-summary(l4)$sigma^2
#Probando igualdad de varianzas
razon=var0/var1
if(razon<qf(.95,gl0,gl1))
{cat("se acepta igualdad")}
#Calculo de la varianza ponderada
varp<-(gl0*var0+gl1*var1)/(gl0+gl1)
varp
#**Calculo de las sumas de cuadrados sxx para cada grupo
ssx0<-sum(pmama0^2)-length(pmama0)*mean(pmama0)^2
ssx1<-sum(pmama1^2)-length(pmama1)*mean(pmama1)^2
#*Calculo de la prueba t
tcal<-(l3$coeff[2]-l4$coeff[2])/sqrt(varp*(1/ssx0+1/ssx1))
tcal
#Decision
if(abs(tcal)>qt(0.925,gl0+gl1))
{cat("Se rechaza hipotesis de igualdad\n")}
cat("Se acepta que las pendientes son iguales\n")
l5=lm(pbebe~pmama+fuma+fuma*pmama,data=pesobebe)
summary(l5)
#****************************************************************************
