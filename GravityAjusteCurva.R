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

z <- lm(gravedad~(poly(latitud,3)+poly(altitud,3)))#splinefun(1:tam,latitud)
z$coefficients

image(z, altitud ~ latitud,d=100,col=rainbow(100))
contour(z, altitud ~ latitud)
persp(z, altitud ~ latitud,theta=30,phi=30,col=rainbow(40),zlab="gravedad",
      contours = list(z="top", col="blue"))

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

grav = 0
grav = gravedadTeorica(latitud)

err = 0
err = error(gravedad,grav)
err2 = error(datosAjustados$Fitted,grav)

qqnorm(err)
qqline(err)

qqnorm(err2)
qqline(err)