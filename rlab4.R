# **************************************************************************************
#Laboratorio 4: Analisis de residuales en regresion lineal simple
#Marzo 2018
#Edgar Acuna
# **************************************************************************************
# llamando al conjunto de datos mortalidad infantil
 muertes<-read.table("http://academic.uprm.edu/eacuna/mortalidad.txt",header=T)
# Ajustando el modelo de regresion
 l2<-lm(tasa.mort~porc.inmuniz,data=muertes)
# Listado de las componentes del objeto l2
 attributes(l2)
 # imprimiendo los residuales usuales
 l2$res
# imprimiendo un resumen de la regresion
 l3<-summary(l2)
# imprimiendo un listado de las componentes del objeto l3
 attributes(l3)
# Extrayendo la desviacion estandar estimada
 l3$sigma
# Calculando los residuales estandarizados
 l2$res/l3$sigma
# Calculando los residuales estudentizados (internamente)
 studresi<-rstandard(l2)
# Cotejando la normalidad de los residuales estudentizados
win.graph()
 
par(mfrow=c(1,3),oma=c(1,1,1,1))
 hist(studresi)
 boxplot(studresi,main="boxplot de residuales")
 qqnorm(studresi)
 qqline(studresi)
title("Cotejando normalidad de residuales",outer=TRUE)

win.graph()
par(mfrow=c(2,2),oma=c(1,1,1,1))
qqnorm(studresi, main="Plot de Normalidad")

qqline(studresi)
plot(studresi, main="Plot de residuales")
abline(h=0,col=2)
plot(l2$fitted,studresi)
title("residuales versus valores ajustados",cex=.5)
abline(h=0,col=2)

plot(muertes$porc.inmuniz,studresi, main="residuales versus predictora",cex=.5)
abline(h=0,col=2)
title("Analisis de residuales",outer=TRUE)
# Construyendo la funcion para calcular el estadistico Durbin-Watson
dw<-function(e)
{
# eliminando el primer dato del vector original
e1<-e[-1] 
# eliminando el ultimo dato del vector original
e2<-e[-length(e)]
# creando las diferencias
diff<-e1-e2
dw<-sum(diff^2)/sum(e^2)
dw
}
#Leyendo el archivo de datos de mortalidad directamente de la internet
muertes<-read.table(file="http://math.uprm.edu/~edgar/mortalidad.txt",header=T)
attributes(muertes)
#Ajustando el modelo lineal
l1<-lm(tasa.mort~porc.inmuniz,data=muertes)
#Calculando el estadistico de Durbin Watson
dw1<-dw(l1$res)
cat("\n ","el estadistico Durbin Watson de la regresion lineal es=",dw1,"\n")

