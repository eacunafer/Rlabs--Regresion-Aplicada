
#**************************************************************************
# Laboratorio 6. Calculo matricial en Regresion lineal multiple
# Edgar Acuna, 
# Marzo 2018
#***************************************************************************
#Leyendo el archivo de datos de millaje directamente de la internet
millaje<-read.table(file="http://academic.uprm.edu/eacuna/millaje.txt",header=T)
#millaje<-read.table(file="c:/millaje.txt",header=T)
mdatos<-as.matrix(millaje)
dim(mdatos)
# Construyendo la  matriz X, cuya primera columna es de unos 
mx<-millaje[,c(2:5)]
col1<-rep(1,82)
mx<-cbind(col1,mx)
mx<-as.matrix(mx)
#calculo del número de observaciones y variables predictoras
n<-dim(mx)[1]
p<-dim(mx)[2]-1
# Calculando la transpuesta de mx
t(mx)
# Calculando la transpuesta de mx y mutiplicandola por mx
t(mx)%*%mx
#Calculo de la matriz sombrero
  h<-mx%*%solve(t(mx)%*%mx)%*%t(mx)
#Calculo de los valores ajustados yhat=hat*y
yhat<-h%*%mdatos[,1]
#Calculo de los residuales
resid<-mdatos[,1]-yhat
#Calculo de la suma de cuadrados de los errores
sse<-sum(resid^2)
#calculo del cuadrado medio del error=mse
df<-n-p-1
 mse<-sse/df
 #Calculo de la desviacion estandar estimada
s<-mse^.5
#****************************************************************************

