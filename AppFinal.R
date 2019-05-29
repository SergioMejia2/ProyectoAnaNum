#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(PolynomF)
library(plotly)
library(rgl)
library(rsm)
set.seed(417)

#Data
data = read.csv("https://raw.githubusercontent.com/SergioMejia2/AnaNum1910/master/Proyecto/datacsv.csv")
sort1.data <- data[order(data$Latitud, data$Altitud, data$Gravedad), ]

gravedad = sort1.data$Gravedad
altitud = sort1.data$Altitud
latitud = sort1.data$Latitud

tam = length(gravedad)

z <- lm(gravedad~(poly(latitud,3)+poly(altitud,3)))#splinefun(1:tam,latitud)
z$coefficients

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

gravedadUser <- function(x,y)
{
  a = 9.7821+4.2044*10^-05*x+1.2357*10^-05*x^2-4.3090*10^-08*x^3+7.5599*10^-06*y-7.4256*10^-09*y^2+1.3883*10^-12*y^3
  return(round(a,4))
}

ax <- list(
  title = "",
  zeroline = FALSE,
  showline = FALSE,
  showticklabels = FALSE,
  showgrid = FALSE
)

grav = 0
grav = gravedadTeorica(latitud)
err = 0
err = error(gravedad,grav)
err2 = error(datosAjustados$Fitted,grav)

# Define UI for application that draws a histogram
ui <- navbarPage("Analisis de la Gravedad",
    
    tabPanel("Pagina Principal",
                          mainPanel(
                          h1("Analisis Gravitacional"),
                          h4("Desde el descubrimiento del concepto de gravedad práctica y teórica en la época de Sir Isaac Newton,
se han solucionado múltiples problemas de cinética, mecánica, entre otras ramas de la física. Sin embargo,
junto con los planteamientos teóricos de las fórmulas físicas, se vio la necesidad de calcular el valor para la
constante gravitacional de la tierra."),
                          h4("Así, se realizaron múltiples experimentos para aproximar un valor de esta constante. Los experimentos
tales como la caída libre de un objeto en un escenario normal y uno en vacío, una balanza de torsión, etc.
fueron usados para este fin. Sin embargo, hasta un punto se ignoró las variaciones que tiene este valor con
respecto a la altitud, la posición latitudinal en la tierra. Esto podría afectar en cálculos de construcción e
ingeniería civil o de aplicaciones de la física (cinética y mecánica) en el uso cotidiano de la ingeniería y la
cotidianidad."),
                          h4("Se calcularon por lo tanto valores en las ciudades más importantes del mundo, a la vez que se halló
la variación de la gravedad en la tierra por medio de fórmulas generales. Sin embargo, existen puntos en
el mundo que no necesariamente pueden estar sujetos directamente a esta fórmula o que no tengan la
capacidad de aplicarlas. Para esto se propone el uso de métodos numéricos para aproximar el valor de la
gravedad en un punto de la Tierra dados los valores actualmente conocidos."),
                          imageOutput("Imagenpp") )),
                 
    tabPanel("Mapa de Calor", mainPanel(h1("Mapa de Calor"),plotOutput("Imagen"),
                              h4("Gráfica bidimensional representando la relación
                                 entre la altitud y la gravedad y el valor relativo
                                 de la gravedad en cada punto. El valor de la gravedad
                                 se ve representado como un mapa de calor, donde
                                 los colores fríos (iniciando en morado) indican
                                 valores bajos de la gravedad y los colores mas cálidos
                                 (verde, amarillo y anaranjado) indican valores
                                 crecientes de la gravedad. Así, se puede observar
                                 que los valores más bajos de la gravedad se encuentran
                                 en las latitudes cercanas al Ecuador con altitudes
                                 mayores a los 1000msnm."))
    ),
    tabPanel("Diagrama de Contorno",
        mainPanel(h1("Diagrama de Contorno"),plotOutput("Contorno"),
                  h4("Gráfica bidimensional que ilustra de manera más numérica las 
                     variaciones de la gravedad cuando la latitud y la altitud varían. 
                     Los contornos varían entre 9.78m/s^2 y 9.82m/s^2, observando una 
                     gran área correspondiente a 9.78m/s^2 en la sección entre 
                     aproximadamente -20° y 20° latitud y 1500-3500msnm. Este gráfico 
                     permite observar las variaciones con mayor claridad que un mapa 
                     de calor, y dependiendo del uso, puede llegar a ser más claro 
                     que una gráfica en tres dimensiones."))
    ),
    tabPanel("Ajuste de Superficie",
      sidebarLayout(
        sidebarPanel( 
          sliderInput("Theta","Selecione el valor de theta: ", min=0, max=90, value=30,step=5),
        sliderInput("Phi","Selecione el valor de phi: ", min=0, max=90, value=30,step=5))
        ,
      mainPanel(h1("Ajuste de Superficie"),
                h4("Gráfica tridimensional resultado de un ajuste de superficie de grado 3 
                   que ilustra el comportamiento de la gravedad cuando la latitud y la altitud
                   varían. Este gráfica oscila entre las latitudes -40° y 60° y las altitudes
                   entre 0 y 4000 msnm ya que son los límites de los datos usados para el ajuste
                   de superficie. Presenta un mapa de calor correspondiente donde los colores
                   fríos (iniciando en morado) indican valores bajos de la gravedad y los colores
                   mas cálidos (amarillo, anaranjado y rojo) indican valores crecientes de la
                   gravedad. Así, se puede observar que los valores más bajos de la gravedad 
                   se encuentran en las latitudes cercanas al Ecuador con altitudes mayores a 
                   los 1000msnm."),
                plotOutput("Perspectiva"),
                textOutput("Description"))
      )
    ),
    navbarMenu("Errores",
               tabPanel("Gravedad Experimental vs Gravedad Teorica",mainPanel(
                 h1("Gravedad Experimental vs Gravedad Teorica"),
                 plotOutput("Error1"),
                 h4("En esta imagen se evidencia el análisis realizado por medio de la normalización
                    mediante el comando qqnorm() de R. Así, se confrontaron los puntos a la distribución
                    normal y a las varianzas del cálculo, con la esperanza que los resultados de
                    los cuantiles sea un crecimiento constante. Cuanto más constante sea el 
                    crecimiento, se puede decir que los errores están distribuidos normalmente 
                    y son aceptables. Como se puede ver en la gráfica, se observa una relación 
                    aproximadamente constante de crecimiento entre los valores de la gravedad 
                    obtenidos de forma experimental con mediciones directas en algunos puntos 
                    sobre la tierra contra la gravedad que nos ofrece el modelo teórico de acuerdo
                    con la latitud del punto, por lo que se puede decir que los errores no son 
                    muy grandes o erráticos.")
               )
               ),
               tabPanel("Gravedad del Ajuste de Superficie vs Gravedad Teorica",mainPanel(
                 h1("Gravedad del Ajuste de Superficie vs Gravedad Teorica"),
                 plotOutput("Error2"),
                 h4("En esta imagen se evidencia el análisis realizado por medio de la normalización 
                    mediante el comando qqnorm() de R. Así, se confrontaron los puntos a la distribución
                    normal y a las varianzas del cálculo, con la esperanza que los resultados de los 
                    cuantiles sea un crecimiento constante. Cuanto más constante sea el crecimiento, 
                    se puede decir que los errores están distribuidos normalmente y son aceptables.
                    Como se puede ver en la gráfica, se observa una relación aproximadamente 
                    constante de crecimiento entre los valores de la gravedad obtenidos a través del
                    ajuste de superficie realizado contra la gravedad que nos ofrece el modelo teórico 
                    de acuerdo con la latitud del punto, por lo que se puede decir que los errores no 
                    son muy grandes o erráticos.")
               )
               )
               ),
    tabPanel("Globo Terráqueo",mainPanel(
      h1("Globo Terráqueo"),
      plotlyOutput("GloboM"),
      h4("Render tridimensional de una porción de los datos correspondiente al continente
         Americano. El tamaño del círculo indica la fuerza de la gravedad en ese punto, 
         siendo un menor punto cuando la gravedad es menor.")
    )
    ),
    tabPanel("Calcular Gravedad",mainPanel(
      textInput("Latitud","Ingrese el valor de la latitud (grados): ",52.225402),
      textInput("Altitud","Ingrese el valor de la altitud (metros): ",41),
      textOutput("ValText"),
      textOutput("Val")
      
    )),
    tabPanel("Muestreo de datos", 
             mainPanel(h1("Muestra de datos"),
                       tableOutput("data"))),
    tabPanel("About Us",
             mainPanel(h1("Proyecto Análisis Numérico"),
                       h4("Proyecto desarrollado en el curso de Análisis Numérico de la 
                          Pontificia Universidad Javeriana con la finalidad de presentar
                          alguna de las aplicaciones en la vida diaria que se tienen de 
                          los métodos numéricos vistos en clase a lo largo del semestre."),
                       h3("Desarrollado por:"),
                       h4("Sergio Andrés Mejía Tovar – sergio.mejia@javeriana.edu.co"),
                       h4("Julian David Parada Galvis – Julian_parada@javeriana.edu.co")))
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
    output$Imagenpp <-  renderImage({
      return(list(
        src = "./pp.jpg",
        contentType = "image/jpg",
        alt = "PP"
      ))
    }, deleteFile = FALSE)  
  
    output$Imagen <- renderPlot({
      image(z, altitud ~ latitud,d=100,col=rev(rainbow(100, s = 1, v = 1, start = 0, end = 0.85)))})
    
    output$Contorno <- renderPlot({
      contour(z, altitud ~ latitud, labcex = 1.5)
      })
    
    output$Perspectiva <- renderPlot({
      if(as.numeric(input$Phi)<15){
        if(as.numeric(input$Theta)<15){
          output$Description <- renderText("Con estos valores se puede observar la variación de la gravedad con respecto a la latitud. Principalmente se observa una función con un mínimo cercano a las latitudes cercanas al Ecuador, aumentando conforme se aleja del centro del planeta. Esta es la variación más importante en el modelo de la gravedad planetaria, ya que los cambios por latitud son muchos mayores que los cambios por altitud.")
          persp(z, altitud ~ latitud,theta=as.numeric(input$Theta),phi=as.numeric(input$Phi),
                col = rev(rainbow(100, s = 1, v = 1, start = 0, end = 0.7))
                ,zlab="gravedad", contours = list(z="bottom", col="blue"))
        }else if(as.numeric(input$Theta)>70){
          output$Description <- renderText("Con estos valores se puede observar la variación de la gravedad con respecto a la altitud. Principalmente se observa una onda suave que disminuye conforme la altitud aumenta (aunque volviendo a aumentar después de los 3000msnm).")
          persp(z, altitud ~ latitud,theta=as.numeric(input$Theta),phi=as.numeric(input$Phi),
                col = rev(rainbow(100, s = 1, v = 1, start = 0, end = 0.7))
                ,zlab="gravedad", contours = list(z="bottom", col="blue"))
        }else{
          output$Description <- renderText("")
          persp(z, altitud ~ latitud,theta=as.numeric(input$Theta),phi=as.numeric(input$Phi),
                col = rev(rainbow(100, s = 1, v = 1, start = 0, end = 0.7))
                ,zlab="gravedad", contours = list(z="bottom", col="blue"))
        }
      }else{
        if(as.numeric(input$Theta)<15){
          output$Description <- renderText("Con estos valores se puede observar la variación de la gravedad con respecto a la latitud. Principalmente se observa una función con un mínimo cercano a las latitudes cercanas al Ecuador, aumentando conforme se aleja del centro del planeta. Esta es la variación más importante en el modelo de la gravedad planetaria, ya que los cambios por latitud son muchos mayores que los cambios por altitud.")
          persp(z, altitud ~ latitud,theta=as.numeric(input$Theta),phi=as.numeric(input$Phi),
                col = rev(rainbow(100, s = 1, v = 1, start = 0, end = 0.7))
                ,zlab="gravedad", contours = list(z="bottom", col="blue"))
         }else if(as.numeric(input$Theta)>70){
          output$Description <- renderText("Con estos valores se puede observar la variación de la gravedad con respecto a la altitud. Principalmente se observa una onda suave que disminuye conforme la altitud aumenta (aunque volviendo a aumentar después de los 3000msnm).")
          persp(z, altitud ~ latitud,theta=as.numeric(input$Theta),phi=as.numeric(input$Phi),
                col = rev(rainbow(100, s = 1, v = 1, start = 0, end = 0.7))
                ,zlab="gravedad", contours = list(z="bottom", col="blue"))
        }else{
          output$Description <- renderText("")
          persp(z, altitud ~ latitud,theta=as.numeric(input$Theta),phi=as.numeric(input$Phi),
                col = rev(rainbow(100, s = 1, v = 1, start = 0, end = 0.7))
                ,zlab="gravedad", contours = list(z="bottom", col="blue"))
        }
      }
    })

    output$Error1 <- renderPlot({
      qqnorm(err, main = "Error Gravedad Experimental vs Gravedad Teorica")
      qqline(err)
    })
    
    output$Error2 <- renderPlot({
      qqnorm(err2, main = "Error Gravedad del Ajuste de Superficie vs Gravedad Teorica")
      qqline(err)
    })
    
    output$GloboM <- renderPlotly({
      
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
      altitud = altitud*exp(1)^(0.0001*(altitud-12741500))
      
      for(i in 1:length(longitud))
      {
        if(longitud[i] < 0)
        {
          longitud[i] = longitud[i] + 360
        }
      }
      
      latitud = -latitud*(pi/180)
      longitud = longitud*(pi/180)
      
      #print(length(latitud))
      #print(length(longitud))
      
      #print(altitud)
      #print(longitud)
      #print(latitud)
      
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
        #print(perc)
        sizes[i] = (15-3)*perc + 3
        #print(sizes[i])
        colors[i] = sizes[i]
      }
      colors[length(colors)+1] = '#176ABD'
      sizes[length(sizes)+1] = d/(60000)
      
      
      #cat(length(sizes), " ", length(cities), length(zm))
      
      plot_ly(x=xm, y=ym, z=zm, type="scatter3d", mode="markers", name="Earth",text=city_info,  projection = list(x=(show=FALSE)),hoverinfo='text',
              marker = list(size=sizes, sizemin=3, color=colors, colorscale = c('#FFE1A1', '#683531'), showscale = TRUE, opacity=1)) %>% layout(scene = list(xaxis=ax,yaxis=ax,zaxis=ax))
      
    })
    
    output$data <- renderTable(data)
  
    output$Val <- renderText({
      valor = gravedadUser(as.numeric(input$Latitud),as.numeric(input$Altitud))
      valor})
    output$ValText <- renderText("El valor de la gravedad para esa posicion es (m/s^2): ")

}
# Run the application 
shinyApp(ui = ui, server = server)