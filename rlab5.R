#***********************************************************
# Laboratorio  5. 
#Introduccion a Regresion lineal multiple
# Marzo 2018, Edgar Acuna
#************************************************************
#Leyendo el archivo de datos de millaje directamente de la internet
millaje<-read.table(file="http://academic.uprm.edu/eacuna/millaje.txt",header=T)
#millaje<-read.table(file="c:/millaje.txt",header=T)
attributes(millaje)
#Haciendo el plot matricial
win.graph()
pairs(millaje)
#haciendo el plot de mpg versus hp
win.graph()
plot(millaje$hp,millaje$mp)
#Extrayendo las variables del conjunto millaje
mpg<-millaje$mpg
hp<-millaje$hp
wt<-millaje$wt
sp<-millaje$sp
vol<-millaje$vol
#Ajustando el modelo lineal mpg vs hp
 l1<-lm(mpg~hp)
summary(l1)
#Ajustando  el modelo cuadratico de mpg versus hp
hp2<-hp^2
l2<-lm(mpg~hp+hp2)
summary(l2)
#Ajustando  el modelo hiperbolico de mpg versus hp
hp1<-1/hp
l3<-lm(mpg~hp1)
summary(l3)
#Ajustando  el modelo lineal de mpg versus las predictoras  hp y wt
l4<-lm(mpg~hp+wt)
summary(l4)
#Ajustando  el modelo lineal de mpg versus las predictoras  hp1 y wt
l5<-lm(mpg~hp1+wt)
summary(l5)
#Ajustando  el modelo lineal de mpg versus todas las predictoras
l6<-lm(mpg~vol+hp+sp+wt)
summary(l6)

