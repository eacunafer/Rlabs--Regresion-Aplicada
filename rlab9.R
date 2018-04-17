#***********************************************************************
# Laboratorio 9.  
#Diagnosticos de casos influenciales en Regresion 
#lineal multiple.
#Ejecutar primero la funcion acuinflu que aparece al final
#  Edgar Acuna, Marzo 2018
#  
#*********************************************************************
#Leyendo el archivo de datos de millaje directamente de la internet
millaje<-read.table(file="http://academic.uprm.edu/eacuna/millaje.txt",header=T)
#millaje<-read.table(file="c:/millaje.txt",header=T)
# Hallando el modelo de regresion ajustado
l1<-lm(mpg~.,data=millaje)
#Hallando los residuales estudentizados internamente
rstint<-rstandard(l1)
rstint
#Hallando los residuales estudentizados externamente
rstext<-rstudent(l1)
rstext
#Determinado las observaciones que son outliers
which(abs(as.vector(rstint))>2)
#Ploteando los residuales y mostrando los outliers segun rsint
out1=rstint[abs(rstint)>2]
win.graph()
par(mfrow=c(1,1))
plot(rstint)
text(names(out1),out1,names(out1),adj=c(1,1))
#mostrando los outliers segun rstext
which(abs(as.vector(rstext))>2)
# Construyendo la  matriz X, cuya primera columna es de unos 
mx<-millaje[,c(2:5)]
col1<-rep(1,82)
mx<-cbind(col1,mx)
mx<-as.matrix(mx)
p<-dim(mx)[2]
ndatos<-dim(mx)[1]
# Hallando los valores leverages(diagonal de la matriz Hat) de cada observacion
leverages<-hat(mx)
#Determinando las observaciones que tienen un leverage alto
indlev=which(abs(leverages)>3*p/ndatos)
print(indlev)
#Ploteando los valores leverages
lev1=leverages[indlev]
#win.graph()
plot(leverages)
text(indlev,lev1,indlev,adj=c(1,1))
#calculando la distancia Cook de cada observacion
cookd<-cooks.distance(l1)
#win.graph()
plot(cookd)
#Determinando las observaciones influenciales segun distancia Cook
which(cookd>qf(0.50,p,ndatos-p))
#calculando los dffits
dfits<-dffits(l1)
#Determinando las observaciones influenciales segun dffits
which(abs(as.vector(dfits))>2*(p/ndatos)^.5)
dif1=dfits[abs(dfits)>2*(p/ndatos)^.5]
#win.graph()
plot(dfits)
text(names(dif1),dif1,names(dif1),adj=c(1,1))
#Calculado los dbfetas
dfb<-dfbetas(l1)
#Determinando las observaciones influenciales para cada uno de los coeficientes
for( i in 1:p)
{tempo<-which(abs(as.vector(dfb[,i]))>2*(ndatos)^(-.5))
cat("valores que influencian el coeficiente",i,"\n")
print(tempo)
}
#Calculando los covratios de cada observacion
cvrat<-covratio(l1)
#Determinando las observaciones influenciales segun los covratios
indcvr=which(abs(1-as.vector(cvrat))>3*p/ndatos)
print(indcvr)
cvr1=cvrat[indcvr]
win.graph()
plot(cvrat)
text(indcvr,cvr1,indcvr,adj=c(-1,-1),cex=.7)
#Determinando todas las observaciones influenciales con por lo menos uno
#de los criterios
#Nota: la funcion acuinflu es una version corregida de la funcion
#influence.measures disponible en R. 
tempo<-acuinflu(l1)
summary(tempo)
#****************************************************************************

acuinflu<-function (lm.obj) 
{#Esta funcion modifica la fucnion influence.measures disponible en R.
# Edgar Acuna, Marzo 2003
    is.influential <- function(infmat, n) {
        k <- ncol(infmat) - 4
        if (n <= k) 
            stop("Too few cases, n < k")
        absmat <- abs(infmat)
        result <- cbind(absmat[, 1:k] > 2/sqrt(n), absmat[, k + 1] > 
            3 * sqrt(k/n), abs(1 - infmat[, k + 2]) > (3 * 
            k)/n, infmat[, k + 3]>qf(0.5, k, n - k), 
            infmat[, k + 4] > (3 * k)/n)
        dimnames(result) <- dimnames(infmat)
        result
    }
    infl <- lm.influence(lm.obj)
    p <- lm.obj$rank
    e <- weighted.residuals(lm.obj)
    s <- sqrt(sum(e^2, na.rm = TRUE)/df.residual(lm.obj))
    xxi <- chol2inv(lm.obj$qr$qr, lm.obj$qr$rank)
    si <- infl$sigma
    h <- infl$hat
    dfbetas <- infl$coefficients/outer(infl$sigma, sqrt(diag(xxi)))
    vn <- variable.names(lm.obj)
    vn[vn == "(Intercept)"] <- "1_"
    colnames(dfbetas) <- paste("dfb", abbreviate(vn), sep = ".")
    dffits <- e * sqrt(h)/(si * (1 - h))
    cov.ratio <- (si/s)^(2 * p)/(1 - h)
    cooks.d <- ((e/(s * (1 - h)))^2 * h)/p
    dn <- dimnames(lm.obj$qr$qr)
    infmat <- cbind(dfbetas, dffit = dffits, cov.r = cov.ratio, 
        cook.d = cooks.d, hat = h)
    is.inf <- is.influential(infmat, sum(h > 0))
    ans <- list(infmat = infmat, is.inf = is.inf, call = lm.obj$call)
    class(ans) <- "infl"
    ans
}

