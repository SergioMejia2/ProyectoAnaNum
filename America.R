set.seed(417)
rm(list=ls())
library(plotly)
data<-read.csv("https://raw.githubusercontent.com/SergioMejia2/ProyectoAnaNum/master/AmericaData.csv")

gravedad =  data$Gravedad
latitud = data$Latitud
longitud = data$Longitud
altitud = data$Altitud
cities = data$Ciudad
d = 2*(6.371*(10^6))

city_info = 0
for(i in 1:length(cities))
{
  city_info[i] = paste(cities[i],"\nCoordinates:\nL: ",latitud[i],"\nLo: ",longitud[i],"\nAlt: ",altitud[i])
}
city_info[length(city_info)+1] = "Earth"


altitud = altitud+d
altitud = altitud*1.1

for(i in 1:length(longitud))
{
  if(longitud[i] < 0)
  {
    longitud[i] = longitud[i] + 360
  }
}

latitud = -latitud*(pi/180)
longitud = longitud*(pi/180)

print(length(latitud))
print(length(longitud))

print(altitud)
print(longitud)
print(latitud)

xm = altitud*sin(longitud)*cos(latitud)
ym = altitud*sin(longitud)*sin(latitud)
zm = altitud*cos(longitud)

xm[length(xm)+1] = 0
ym[length(ym)+1] = 0
zm[length(zm)+1] = 0
sizes = 0
colors = 0
maxim = max(gravedad)
minim = min(gravedad)

for(i in 1:(length(gravedad)))
{
  perc = (gravedad[i]-minim)/(maxim - minim)
  print(perc)
  sizes[i] = (15-3)*perc + 3
  #print(sizes[i])
  colors[i] = sizes[i]
}
colors[length(colors)+1] = '#176ABD'
sizes[length(sizes)+1] = d/(60000)


cat(length(sizes), " ", length(cities), length(zm))

plot_ly(x=xm, y=ym, z=zm, type="scatter3d", mode="markers", name="Earth",text=city_info,  projection = list(x=(show=FALSE)),hoverinfo='text',
        marker = list(size=sizes, sizemin=3, color=colors, colorscale = c('#FFE1A1', '#683531'), showscale = TRUE, opacity=1))
