set.seed(417)
rm(list=ls())
library(plotly)
data<-read.csv("https://raw.githubusercontent.com/SergioMejia2/AnaNum1910/master/Proyecto/datacsv.csv")
#gravedad = c(9.7979, 9.7904, 9.8159, 9.7799, 9.7724, 9.8024, 9.8189, 9.7964, 9.8129, 9.8189, 9.8144, 9.8189, 9.8009, 9.7964, 9.7844, 9.8039, 9.8069, 9.8024, 9.7814, 9.7949, 9.7964, 9.8039, 9.7979, 9.7979, 9.7994, 9.8024, 9.7919, 9.8024, 9.7964, 9.7994, 9.7859, 9.7829, 9.7979, 9.7979, 9.8099, 9.8114, 9.7904, 9.7844, 9.7889, 9.8069, 9.8069, 9.8054, 9.8099, 9.8114, 9.7979, 9.8099, 9.7799, 9.7829, 9.7799, 9.7964, 9.8129, 9.8039, 9.7814, 9.7829, 9.7844, 9.8159, 9.8009, 9.8054, 9.7904, 9.8189, 9.7814, 9.7919, 9.8024, 9.8084)
#latitud  = c(35.180282, 25.037519, 55.686724, 36.784407, -0.220164, 39.921522, 60.167409, -34.905904, 51.225402, 59.91333, 51.507322, 61.216313, 37.984149, 33.749099, 14.622233, 42.360253, 47.498382, 41.875562, -6.175394, 32.776272, 33.302431, 42.331551, 35.147361, 34.053683, 37.566679, 40.730862, 29.373312, 39.952415, 33.89592, 37.779026, -20.169373, 10.506098, -34.607562, -33.854816, 48.208354, 50.846557, 26.223504, -16.495637, -15.793404, 45.497216, 45.421106, 43.653963, 49.260872, 50.087465, -33.437797, 22.350627, 4.598077, 9.932543, 19.432601, 34.022405, 52.37276, -41.288749, 8.971449, -12.062106, 14.590622, 51.892584, 38.743665, 44.436141, 24.631969, 59.325117, 1.290475, -26.205, 40.416705, 46.948271)
#longitud = c(33.373696, 121.563679, 12.570072, 10.183001, -78.512327, 32.853793, 24.942568, -56.191357, 6.776314, 10.73897, -0.127647, -149.894852, 23.727984, -84.390185, -90.518519, -71.058291, 19.040471, -87.624421, 106.827183, -96.796856, 44.378799, -83.04664, 138.948903, -118.242767, 126.978291, -73.987156, 47.977835, -75.163575, 35.47843, -122.419906, 57.509494, -66.914602, -58.437076, 15.1216454, 16.372504, 4.351697, 50.582244, -68.133635, -47.882308, -73.610364, -75.690308, -79.387207, -123.113953, 14.421254, -70.650445, 114.184916, -74.076103, -84.079578, -99.133342, -6.834543, 4.893604, 174.777898, -79.53418, -77.036526, 120.97997, 21.920666, -9.151736, 26.10272, 46.715065, 18.071094, 103.852036, 28.049722, -3.703582, 7.451451)
#altitud  = c(139, 25, 11, 26, 2877, 871, 29, 21, 41, 0, 33, 0, 88, 309, 1509, 8, 137, 175, 8, 132, 39, 183, 146, 102, 51, 13, 17, 7, 0, 70, 191, 1078, 25, 7, 189, 28, 5, 3931, 1089, 59, 58, 91, 18, 195, 567, 96, 2749, 1182, 2240, 7, 5, 112, 15, 161, 7, 179, 81, 83, 591, 29, 3, 1747, 698, 557)
#cities = c("Nicosia","Taipei","Copenague","Tunez","Quito","Ankara","Helsinki","Montevideo","Dusseldorf","Oslo","Londres","Anchorage","Atenas","Atlanta","Ciudad de Guatemala","Boston","Budapest","Chicago","Jakarta","Dallas","Bagdad","Detroit","Mishima","Los Angeles","Seul","Nueva York","Kuwait","Filadelfia","Beirut","San Francisco","Puerto Luis","Caracas","Buenos Aires","Sidney","Viena","Bruselas","Manama","La Paz", "Brasilia", "Montreal", "Ottawa","Toronto","Vancouver","Praga","Santiago","Hong Kong","Bogota","San Jose","Ciudad de Mexico","Rabat","Amsterdam","Wellington","Ciudad de Panama","Lima","Manila","Swider","Lisboa","Bucarest","Riyad","Estocolmo","Singapur","Johannesburg","Madrid","Berna")

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
