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
  a = 9.782121+4.204448*10^-05*x+1.235670*10^-05*x^2-4.308964*10^-08*x^3+7.559932*10^-06*y-7.425638*10^-09*y^2+1.388293*10^-12*y^3
  return(a)
}

grav = 0
grav = gravedadTeorica(latitud)
err = 0
err = error(gravedad,grav)
err2 = error(datosAjustados$Fitted,grav)

# Define UI for application that draws a histogram
ui <- navbarPage("Analisis de la Gravedad",
    
    tabPanel("Imagen", mainPanel(plotOutput("Imagen"),textOutput("Description"))
    ),
    tabPanel("Contorno",
        mainPanel(plotOutput("Contorno"),textOutput("Description2"))
    ),
    tabPanel("Perspectiva",
      sidebarLayout(
        sidebarPanel( 
          sliderInput("Theta","Selecione el valor de theta: ", min=0, max=90, value=30,step=5),
        sliderInput("Phi","Selecione el valor de phi: ", min=0, max=90, value=30,step=5))
        ,
      mainPanel(plotOutput("Perspectiva") )
      )
    ),
    navbarMenu("Error",
               tabPanel("Gravedad Experimental vs Gravedad Teorica",mainPanel(
                 plotOutput("Error1")
               )
               ),
               tabPanel("Gravedad del Ajuste de Superficie vs Gravedad Teorica",mainPanel(
                 plotOutput("Error2")
               )
               )
               ),
    tabPanel("Globo",mainPanel(
     plotlyOutput("GloboM")
    )
    ),
    tabPanel("Calcular Gravedad",mainPanel(
      textInput("Latitud","Ingrese el valor de la latitud (grados): ",52.225402),
      textInput("Altitud","Ingrese el valor de la altitud (metros): ",41),
      textOutput("ValText"),
      textOutput("Val")
      
    ))
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    output$Imagen <- renderPlot({
      output$Description <- renderText("DESCRIPCION URGENTE ACA!!!!!")
      image(z, altitud ~ latitud,d=100,col=rev(rainbow(100, s = 1, v = 1, start = 0.1, end = 0.85)))})
    output$Contorno <- renderPlot({
      output$Description2 <- renderText("DESCRIPCION URGENTE ACA!!!!! 2")
      contour(z, altitud ~ latitud, labcex = 1.5)
      })
    output$Perspectiva <- renderPlot({
      output$Description <- renderText("DESCRIPCION URGENTE ACA!!!!! 3")
      persp(z, altitud ~ latitud,theta=as.numeric(input$Theta),phi=as.numeric(input$Phi),
            col = rev(rainbow(100, s = 1, v = 1, start = 0, end = 0.7))
            ,zlab="gravedad", contours = list(z="top", col="blue"))
    })
    output$Perspectiva <- renderPlot({
      output$Description <- renderText("DESCRIPCION URGENTE ACA!!!!! 3")
      persp(z, altitud ~ latitud,theta=as.numeric(input$Theta),phi=as.numeric(input$Phi),
            col = rev(rainbow(100, s = 1, v = 1, start = 0, end = 0.7))
            ,zlab="gravedad", contours = list(z="top", col="blue"))
    })
    output$Error1 <- renderPlot({
      output$Description <- renderText("DESCRIPCION URGENTE ACA!!!!! 4")
      qqnorm(err, main = "Error Gravedad Experimental vs Gravedad Teorica")
      qqline(err)
    })
    output$Error2 <- renderPlot({
      output$Description <- renderText("DESCRIPCION URGENTE ACA!!!!! 5")
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
              marker = list(size=sizes, sizemin=3, color=colors, colorscale = c('#FFE1A1', '#683531'), showscale = TRUE, opacity=1))
      
    })
  
    output$Val <- renderText({
      valor = gravedadUser(as.numeric(input$Latitud),as.numeric(input$Altitud))
      valor})
    output$ValText <- renderText("El valor de la gravedad para esa posicion es (m/s^2): ")

}



# Run the application 
shinyApp(ui = ui, server = server)
