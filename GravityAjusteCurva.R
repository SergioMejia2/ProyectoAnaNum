library(PolynomF)
library(plotly)
library(rgl)
library(rsm)

data = read.csv("https://raw.githubusercontent.com/SergioMejia2/AnaNum1910/master/Proyecto/datacsv.csv")
sort1.data <- data[order(data$Latitud, data$Altitud, data$Gravedad), ]

gravedad = sort1.data$Gravedad
altitud = sort1.data$Altitud
latitud = sort1.data$Latitud

tam = length(gravedad)

z <- lm(gravedad~(poly(latitud,3,raw=TRUE)+poly(altitud,3,raw=TRUE)))#splinefun(1:tam,latitud)
z$coefficients
summary(z)
coef(z)
#pdf(file = "gravity.pdf")
png(file="img.png",width=800)
image(z, altitud ~ latitud,d=100,col=rev(rainbow(100, s = 1, v = 1, start = 0, end = 0.85)))
dev.off()
png(file="cont.png",width=800)
contour(z, altitud ~ latitud, labcex=0.8)
dev.off()
persp(z, altitud ~ latitud,col = rev(rainbow(100, s = 1, v = 1, start = 0, end = 0.7))
      ,zlab="gravedad", contours = list(z="top", col="blue"))
dev.off()
png(file="img3.png",width=800)
persp(z, altitud ~ latitud,theta=30,phi=30,col = rev(rainbow(100, s = 1, v = 1, start = 0, end = 0.7))
      ,zlab="gravedad", contours = list(z="top", col="blue"))
dev.off()
png(file="img4.png",width=800)
persp(z, altitud ~ latitud,theta=90,phi=0,col = rev(rainbow(100, s = 1, v = 1, start = 0, end = 0.7))
      ,zlab="gravedad" )#contours = list(z="top", col="blue"))
dev.off()
png(file="img5.png",width=800)
persp(z, altitud ~ latitud,theta=0,phi=0,col = rev(rainbow(100, s = 1, v = 1, start = 0, end = 0.7))
      ,zlab="gravedad", contours = list(z="top", col="blue"))
dev.off()

datosAjustados = data.frame(Lat = latitud, Alt = altitud, Fitted = fitted(z))

GravedadHelmert <- function(x){
  result = 0
  
  result = 9.7803267715*(1 + 0.001931851353*sin(x)^2/sqrt(1-0.0066943800229*sin(x)^2))
  
  return (result)
}

gravedadTeorica <- function(x){
  gravedadT = c()
  
  i = 1
  
  while(i <= length(x)){
    gravedadT[i]=GravedadHelmert(x[i])
    i = i+1
  }
  
  return(gravedadT)
  
}

error <- function(exp, teo){
  
  errores = c()
  
  i=1
  while(i<=length(exp)){
    
    errores[i] = abs((exp[i]-teo[i])/(teo[i]))*100
    
    i = i+1
  }
  
  return(errores)
  
}

plot3d(datosAjustados$Lat,datosAjustados$Alt,datosAjustados$Fitted)

grav = 0
grav = gravedadTeorica(latitud)

err = 0
err = error(gravedad,grav)
err2 = error(datosAjustados$Fitted,grav)
#pdf(file="qqnorms.pdf")
png(file="err1.png",width=800)
qqnorm(err)
qqline(err)
dev.off()
png(file="err2.png",width=800)
qqnorm(err2)
qqline(err)
dev.off()