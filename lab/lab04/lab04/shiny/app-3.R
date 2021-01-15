library(shiny)
library(leaflet)
library(readxl)
library(RColorBrewer)
library(DT)
library(plotly)
library(tidyverse)
library(mgcv)
library(car)
library(leaps)
library(janitor)
library(dplyr)


madrid <- read.csv("../data/Resultados_Madrid1.csv",header = TRUE,dec = ".")

madrid <- madrid[-1]

madrid <- cbind('id' = seq(1:26), madrid)
madrid <- clean_names(madrid)
# Define UI for application that draws a histogram
ui <- fluidPage(

  
    titlePanel("Mapa comunidad Madrid"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          selectInput('renta', 
                      'Seleccione la categoría de renta predicha:', 
                      choices = c('Bajo' = 1, 
                                  'Medio' = 2, 
                                  'Alto' = 3), 
                      selected = 1),
          
   
        
          
        ),

        # Show a plot of the generated distribution
        mainPanel(
          
          leafletOutput("map",
                        height = 600, 
                        width = 1860)
        )
    )
    )
    

# Define server logic required to draw a histogram
server <- function(input, output) {
 
   filteredData <- reactive({
    
    madrid[((madrid$prediccion == input$renta)), ]
    
  })
   
   #icons <- makeIcon(icon = 'flag')
   output$map <- renderLeaflet({
     
      
     
     
     leaflet(filteredData()) %>% 
               
               addTiles() %>%
               
               # Setview nos permite situarnos en una ubicaciÃ³n concreta
               # dentro del mapa por defecto. En nuestro caso, viendo la
               # disposiciÃ³n de los pisos en el mapa, decidimos que ese
               # punto de ubicaciÃ³n de partida fuese El Retiro.
               
               setView(lng = -3.1906821, 
                       lat = 40.6321888, 
                       zoom = 8) %>%
               
               # Vinculamos los cambios en el estilo del mapa sobre los
               # madrid del mapa:
               
               # AÃ±adimos los diferentes iconos que queremos sobre el 
               # mapa para la ubicacion de los pisos. 
               # Importante hacerlo sobre los valores reactivos porque,
               # si no, no se actualizara de forma correcta.
               
                  addMarkers(
                 lng = ~ longitud,
                 lat = ~ latitud,
                 #icon = icons,
                 layerId = filteredData()$id,
                 popup = paste0(
                   "Municipio:",
                   "<b>", as.character(filteredData()$municipio), 
                   "</b><br/>",
                   
                   "Categoria de Renta Real:",
                   "<b>", as.character(filteredData()$renta),  
                   "</b><br/>",
                   
                   "Categoria de Renta Predicha:",
                   "<b>", as.character(filteredData()$prediccion),  
                   "</b><br/>"),
                 
          
                 
               )
     
    
           })
}

# Run the application 
shinyApp(ui = ui, server = server)
