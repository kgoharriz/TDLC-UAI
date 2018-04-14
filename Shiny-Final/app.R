#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
setwd("/Users/kiumarzgoharrizchahin/Desktop/Shiny-Final/")
library(shiny)
library(ggplot2)
library(openxlsx)
library(dplyr)
dt_matrix <- read.xlsx('matriz-final.xlsx')
#dt_matrix$Ponderado <- as.double(as.character(dt_matrix$Prediccion))

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Visualizador documentos Tribunal de Libre Competencia"),
   
   # Sidebar with a slider input for number of bins
     sidebarLayout(

       #Creamos un panel principal para inputs
       sidebarPanel(
         #sliderInput("TopicoInput", "Topico", min = 1, max = 6,
          #           value = c(1, 6), pre = " "),
         radioButtons("TopicoInput", "Topico",
                      choices = seq(1:6),
                      selected = 1),
         selectInput("PrediccionInput", "Prediccion",
                     choices = c("APROBADO","RECHAZADO"))
        ),
      
      # Show a plot of the generated distribution
      mainPanel(
        plotOutput("coolplot"),
        br(), br(),
        tableOutput("results")
       )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  filtered <- reactive({ # Definimos los criterios bajo los cuales queremos que `reaccione`
    #la app. Para esto, usamos la función REACTIVE
    dt_matrix %>%
      filter(Topico == input$Topico,
             #Resultado == input$Resultado,
             Prediccion == input$Prediccion
      )
  })
  
  output$coolplot <- renderPlot({
    ggplot(filtered(), aes(Ponderado)) +
      geom_histogram()
  })
  
  output$results <- renderTable({
    filtered()
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
