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

grav = 0
grav = gravedadTeorica(latitud)
err = 0
err = error(gravedad,grav)
err2 = error(datosAjustados$Fitted,grav)

# Define UI for application that draws a histogram
ui <- fluidPage(
    headerPanel("Analisis de la Gravedad"),
    
    sidebarPanel(
        selectInput("Tipo", "Seleccione el tipo de grafica a mostrar",
                    choices=c("Imagen", "Contorno", "Perspectiva", 
                              "Error: Gravedad Experimental vs Gravedad Teorica",
                              "Error: Gravedad del Ajuste de Superficie vs Gravedad Teorica",
                              "Globo")),
        
        conditionalPanel(condition = "input.Tipo == 'Perspectiva'", 
                         sliderInput("Theta","Selecione el valor de theta: ",
                                     min=0, max=90, value=30,step=5),
                         sliderInput("Phi","Selecione el valor de phi: ",
                                     min=0, max=90, value=30,step=5))
    ),
    
    mainPanel(
        plotOutput("AjusteSuperficie"),
        textOutput("Description")
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$AjusteSuperficie <- renderPlot({
        grapgType <- input$Tipo
        
        if(grapgType == "Imagen"){
            #pdf(file = "imagen.pdf")
            output$Description <- renderText("DESCRIPCION URGENTE ACA!!!!!")
            image(z, altitud ~ latitud,d=100,col=rev(rainbow(200)))
            #dev.off()
        }
        if(grapgType == "Contorno"){
            output$Description <- renderText("DESCRIPCION URGENTE ACA!!!!! 2")
            contour(z, altitud ~ latitud)
        }
        if(grapgType == "Perspectiva"){
            output$Description <- renderText("DESCRIPCION URGENTE ACA!!!!! 3")
            persp(z, altitud ~ latitud,theta=as.numeric(input$Theta),phi=as.numeric(input$Phi),
                  col = rev(rainbow(100, s = 1, v = 1, start = 0, end = 0.7))
                  ,zlab="gravedad", contours = list(z="top", col="blue"))
        }
        if(grapgType == "Error: Gravedad Experimental vs Gravedad Teorica"){
            output$Description <- renderText("DESCRIPCION URGENTE ACA!!!!! 4")
            qqnorm(err, main = "Error Gravedad Experimental vs Gravedad Teorica")
            qqline(err)
        }
        if(grapgType == "Error: Gravedad del Ajuste de Superficie vs Gravedad Teorica"){
            output$Description <- renderText("DESCRIPCION URGENTE ACA!!!!! 5")
            qqnorm(err2, main = "Error Gravedad del Ajuste de Superficie vs Gravedad Teorica")
            qqline(err)
        }
    })
    
    
    
}



# Run the application 
shinyApp(ui = ui, server = server)
