#********************************
#Laboratorio 8. Falta de ajuste en R
#Edgar Acuna,
# Marzo 2018
#***************************
#Leyendo el conjunto de datos
millaje=read.table("http://academic.uprm.edu/eacuna/millaje.txt",header=T)
millajelf=millaje[,c(1,5)]
table(millajelf$hp)
# Hay m=40 valores distintos de la predictora
# anadiendo una columna var3 que identifica a que grupo pertenece cada
# observación
var3=factor(millajelf$hp,labels=1:40)
millajelf=cbind(millajelf,var3)
millajelf[1:10,]
#haciendo la regresion lineal simple
l1=lm(mpg~hp,data=millajelf)
l1
anova(l1)
#Haciendo el analisis de varianza de claificacion simple de mpg
# entre los 40 grupos diferentes
l2=lm(mpg~factor(var3),data=millajelf)
anova(l2)
#Haciendo el anova para comparar los dos modelos. Se extrae la suma de cuadrados del
# error Puro y la suma de cuadrados de falta de Ajuste.
anova(l1,l2)
#Consideremos ahora todas las variable predictoras
millajep=millaje[,2:5]
#Encontrando el numero de combinaciones distintas
dim(unique(millajep))
# Hay m=70 valores distintos de la predictora
# anadiendo una columna var4 que identifica a que grupo pertenece cada
# observación
#
millajelf=gruposlf(millaje[,2:5])
# Haciendo la regresion lineal multiple
l3=lm(mpg~sp+wt+vol+hp,data=millaje)
anova(l3)
#Haciendo el analisis de varianza de clasificacion simple de mpg
# entre los 70 grupos diferentes
mpg=millaje[,1]
millajelf=cbind(mpg,millajelf)
millajelf[1:3,]
l4=lm(mpg~factor(varg),data=as.data.frame(millajelf))
anova(l4)
#Haciendo el anova para comparar los dos modelos . Se extrae la suma de cuadrados del
# error Puro y la suma de cuadrados de falta de Ajuste.
#anova(l2,l4)
#***********************************************************
#****************************************************************
gruposlf=function (data) 
{# Esta funcion encuentra el numero de grupos distintos entre las filas
# de la matriz X y le anadae una columna varg que identifica al grupo
#
data=as.matrix(data)
p=dim(data)[2]
datau=unique(data)
rowsu=dim(datau)[1]
cat("\nnumber of distinct instances",rowsu)
cat("\n")
rowsrep=1:rowsu
n=dim(data)[1]
varg=rep(0,n)
data1=cbind(data,varg)
for(j in 1:rowsu)
{tempo=row.matches(datau[j,],data)
data1[tempo,p+1]=j
}
return(data1)
}
#*******************************************
row.matches=function(y, X) {
#********************************************
#This functions finds out the rows in matrix X
#that are equal to the vector y
#y: a vector
#X: a matrix
#**************************************************
        i <- seq(nrow(X)) 
        j <- 0 
        while(length(i) && (j <- j + 1) <= ncol(X)) 
                i <- i[X[i, j] == y[j]] 
        i 
}
