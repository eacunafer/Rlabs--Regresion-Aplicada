#****************************************************************************
# Laboratorio 16: Regresion logistica 
#  Edgar Acuna, 
#  Marzo 2013
#  
#****************************************************************************
#Leyendo el archivo de datos de millaje directamente de la internet
pesobebe<-read.table(file="http://academic.uprm.edu/eacuna/bajopeso.txt",header=T)
#pesobebe<-read.table(file="c:/bajopeso.txt",header=T)
# la primera columna de pesobebe contiene la variable bajopeso que se obtiene
#codificando la ultima variable peso. Si peso<2500 entonces bajopeso es 0 de lo
#contario vale 1
#Haciendo la regresion de bajopeso versus pesomama
l1=lm(bajopeso~pesomama,data=pesobebe)
#Ploteando bajopeso versus pesomama y la linea de regresion
win.graph()
plot(pesobebe$pesomama,pesobebe$bajopeso)
title("bajopeso versus pesomama")
abline(l1)
#Ploteando los residuales de la regresion
win.graph()
par(mfrow=c(1,2))
plot(l1$fitted,rstandard(l1))
abline(h=0)
plot(pesobebe$pesomama,rstandard(l1))
abline(h=0)
#Ploteando la curva logistica F(x)=1/(1+exp(-x)) en el rango (-10,10)
puntos<-seq(-10,10,length=100)
win.graph()
par(mfrow=c(1,1))
plot(puntos,1/(1+exp(-puntos)),type="l")
title("plot de la curva logistica en (-10,10)")
#Notar que hay algo de similtud con el plot anterior
#Agrupando la  variable pesomama en 5 intervalos para sacar estimaciones de 
#las probabilidades de bajopeso
ngrupos<-5 # el metodo de scott no conviene aqui
grupos<-cut(pesobebe[,3],ngrupos)
datos1<-cbind(pesobebe[,1],pesobebe[,3],grupos)
proby<-rep(0,ngrupos)
for(i in 1:ngrupos)
{datos2<-datos1[datos1[,3]==i,]
proby[i]<-sum(datos2[,1])/length(datos2[,1])
}
cat("las probabilidades estimadas en cada grupos son:",proby,"\n")
#Desafortunadamente los grupos no son homogeneos y no se prestan para
#aplicar regresion logistica en datos agrupados
#De ahora en adelante vamos a excluir la columna peso de pesobebe
pesobebe<-pesobebe[,-10]
# Haciendo la regresion logistica simple
logis1<-glm(bajopeso~pesomama,data=pesobebe,family=binomial)
summary(logis1)
#Haciendo la regresion logistica multiple
logis2<-glm(bajopeso~.,data=pesobebe,family=binomial)
summary(logis2)
#Haciendo otra vez la regresion logistica incluyendo solo las variables mas #significativas
logis3<-glm(bajopeso~pesomama+raza+fuma+hipertensio,data=pesobebe,family=binomial)
summary(logis3)
#Observando el valor de la devianza y del AIC el tercer modelo seria el mejor modelo
#sin embargo en lo que sigue vamos a considerar los resultados del segundo modelo
#********************************************************
#Prediciendo las clases con el segundo modelo
#Haciendo la clasificacion por el metodo mas simple comparando con p=0.5
#********************************************************
phat<-fitted.values(logis2)
nobs<-dim(pesobebe)[1]
clases<-rep(0,nobs)
for(i in 1:nobs)
{if(phat[i]>=0.5){clases[i]<-1}
}
errores<-sum(clases!=pesobebe[,1])
rate<-errores/nobs
cat("la tasa de mala clasificacion es=",rate,"\n")
#*************************************************
# Haciendo la clasificacion con el metodo mas complicado calculando la sensitividad y #especificidad
#********************************
p<-seq(.1,.9,length=9)
sensit<-rep(0,9)
especif<-rep(0,9)
for(j in 1:9)
{clases1<-rep(0,nobs)
for(i in 1:nobs)
{if(phat[i]>=p[j]){clases1[i]<-1}
}
pesobebe1<-cbind(pesobebe[,1],clases1)
sibajo<-pesobebe1[pesobebe1[,1]==1,]
nobajo<-pesobebe1[pesobebe1[,1]==0,]
sensit[j]<-mean(sibajo[,1]==sibajo[,2])
especif[j]<-mean(nobajo[,1]==nobajo[,2])
}
tabla<-cbind(p,sensit,especif)
cat("Sensitividad y especifidad para varios valores de p\n")
print(tabla)
#Haciendo el plot para hallar el p optimo
win.graph()
plot(p,sensit,type="l")
lines(p,especif)
text(p,sensit,labels=p)
title("Ploteando la sentividad y especificidad para varios p") 
# p=0.30 parece ser el optimo
#Ploteaando la curva ROC
win.graph()
plot(1-especif,sensit,type="l")
text(1-especif,sensit,labels=p)
title("La curva ROC")
#Notar que para p=.30 la curva esta mas cerca a la esquina superior izquierda 
#Clasificacion final
clasesf<-rep(0,nobs)
for(i in 1:nobs)
{if(phat[i]>=0.3){clasesf[i]<-1}
}
erroresf<-sum(clasesf!=pesobebe[,1])
ratef<-erroresf/nobs
cat("la tasa de mala clasificacion optima es=",ratef,"\n")
#****************************************************************************


