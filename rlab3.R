#***********************************************************
# Laboratorio  3: Analisis de  Correlacion
# Febrero 2008,
# Edgar Acuna
#************************************************************
#Leyendo el archivo de datos corrs directamente desde la internet
cor1=read.table(file="http://academic.uprm.edu/eacuna/corrs.dat",header=T)

co1=cor(cor1$x1,cor1$y1)
cat("\n","el coeficiente de correlacion entre X1 y Y1 es=",co1,"\n")

co2=cor(cor1$x2,cor1$y2)
cat("\n","el coeficiente de correlacion entre X2 y Y2 es=",co2,"\n")

co3=cor(cor1$x3,cor1$y3)
cat("\n","el coeficiente de correlacion entre X3 y Y3 es=",co3,"\n")

co4=cor(cor1$x4,cor1$y4)
cat("\n","el coeficiente de correlacion entre X4 y Y4 es=",co4,"\n")

win.graph()
par(mfrow=c(2,2),oma=c(1,1,1,1))
plot(cor1$x1,cor1$y1)
text(10,30,"r=.988")

plot(cor1$x2,cor1$y2)
text(10,20,"r=-.992")
plot(cor1$x3,cor1$y3)
text(12,32,"r=.191")

plot(cor1$x4,cor1$y4)
text(12,25,"r=.112")
title("Ejemplos de correlaciones",outer=TRUE)

#Leyendo el archvo de datos corrout
 cor2<-read.table(file="http://academic.uprm.edu/eacuna/corrout.dat",header=T)
attributes(cor2)
 c1<-cor(cor2$x1,cor2$y1)
cat("\n ","el coeficiente de correlacion entre X1 y Y1 es=",c1,"\n")
 c2<-cor(cor2$x2,cor2$y2)
cat("\n ","el coeficiente de correlacion entre X2 y Y2 es=",c2,"\n")
c3<-cor(cor2$x3,cor2$y3)
cat("\n ","el coeficiente de correlacion entre X3 y Y3 es=",c3,"\n")
 c4<-cor(cor2$x4,cor2$y4)
cat("\n ","el coeficiente de correlacion entre X4 y Y4 es=",c4,"\n")
 win.graph()
par(mfrow=c(2,2),oma=c(1,1,1,1))
 plot(cor2$x4,cor2$y4)
 text(10,30,"r=0.264",cex=.9)
 plot(cor2$x3,cor2$y3)
 text(20,25,"r=0.436",cex=.9)
 plot(cor2$x1,cor2$y1)
 text(20,25,"r=0.984",cex=.9)
 plot(cor2$x2,cor2$y2)
 text(50,100,"r=0.995",cex=.9)
title("Efecto de outliers en la correlacion",outer=TRUE)

