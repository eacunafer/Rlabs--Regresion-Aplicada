#**************************************************************
#Laboratorio 18:Funciones para calcular Press y Validacion cruzada para #Regresion 
#Edgar Acuna
#Mayo 2014
#***************************************************************
PRESS=function (x) 
{#x es un objecto que sale de aplicar lm
    sum(resid(x)^2/(1 - lm.influence(x)$hat)^2)
}
cv10reg=function(data, folds=10,repet)
{#data: el conjunto de datos
#folds: numero de partes
#numero de repeticiones del experimento
#Suposicion: la primera columna contiene la variable de respuesta
n=dim(data)[1]
p=dim(data)[2]
nombres=colnames(data)
f1=as.formula(paste(nombres[1],".",sep="~"))
#print(f1)
EVC<-rep(0,repet)
for(i in 1:repet)
{
resid <- matrix(0, 1, folds)
azar <- data[rank(runif(n)),  ]
parti <- floor(n/folds)
for(j in 1:folds) {
cc <- ((j - 1) * parti + 1):(j * parti)
if(j == folds) {
cc <- ((j - 1) * parti + 1):n
}
datap <- azar[cc,  ]
# La muestra de prueba
datat <- azar[ - cc,  ]
#La muestra de entrenamiento
tempo = lm(f1, data = datat)
tempo1 <- predict(tempo, datap)
resid[j] <- sum((tempo1 - datap[, 1])^2)
}
EVC[i] <- sum(resid)/n
}
cat("Los estimados del error promedio de prediccion en cada prediccion son:\n")
print(EVC)
cat("La estimacion del error promedio de prediccion segun el numero de repeticiones dado sera\n")
EVC1<-mean(EVC)
EVC1
}
#***************************************************************************

