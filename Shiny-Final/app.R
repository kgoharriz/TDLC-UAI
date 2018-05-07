# ==============================================================================
# Proyecto Final - Shiny Dashboard
# Diplomado Big Data para Políticas Públicas
# Universidad Adolfo Ibáñez
# 27 de abril de 2018
# ==============================================================================

# Libraries ----
library(shiny)
library(shinydashboard)
library(ggplot2)
library(openxlsx)
library(dplyr)
library(LDAvis)
library(tm)
library(wordcloud)
library(memoise)
library(ECharts2Shiny)
library(treemap)

# Folders and files ----
setwd('/Users/kiumarzgoharrizchahin/Documents/Formación y estudios/UAI_Diplomado_Big_Data/Proyecto.Final/Shiny-TDLC/')

# Recommendation <- read.csv('recommendation.csv',stringsAsFactors = F,header=T)
TDLC <- read.xlsx('TDLC.xlsx')
Monogram <- read.xlsx('monogram-6GAMMA.xlsx')

myLDA <- readRDS('myLDA_vis.rds')
shiny_df <- readRDS('wcloud_df.rds')

# Trabajamos primero con los datos para el wordcloud
# ============================================================================ #
# Lista de documentos disponibles ----
#doc_list <- shiny_df$doc_id
doc_list <- as.list(as.character(shiny_df$doc_id))
names(doc_list) <- as.character(shiny_df$doc_id)

# Función 'get_term_matrix' ----
# Using "memoise" to automatically cache the results.
get_term_matrix <- memoise(function(doc_tdlc) {
  
  # Individualiza documento ----
  #shiny_vector <- shiny_df$text[shiny_df$doc_id == doc_tdlc]
  
  # Corpus ----
  shiny_corpus <- Corpus(DataframeSource(shiny_df))
  #shiny_corpus <- Corpus(VectorSource(shiny_vector))
  
  # Term-Document Matrix (TDM) ----
  shiny_dtm <- DocumentTermMatrix(shiny_corpus,
                                  control = list(minWordLength = 2,
                                                 wordLengths = c(0, Inf)
                                  )
  )
  
  # Matrix ----
  shiny_m0 <- as.matrix(shiny_dtm)
  
  # Individualiza documento ----
  pos <- which(shiny_df$doc_id == doc_tdlc)
  shiny_m <- shiny_m0[pos, ]
  
  # Ordena términos por frecuencia ----
  #sort(rowSums(shiny_m), decreasing = TRUE)
  
})

# ============================================================================ #

# Head TDLCeader carrying the title of the dashboard
header <- dashboardHeader(title = "Analisis del TDLC") 

# Sidebar content of the dashboard
sidebar <- dashboardSidebar(collapsed = TRUE,
  sidebarMenu(selected = 'dashboard',badgeColor = "light grey",
    menuItem("dashboard", tabName = "dashboard",icon = icon("dashboard")),
    menuItem("wordcloud", tabName = "wordcloud", icon = icon("cloud",lib='glyphicon')),
    menuItem("ldavis", tabName = "ldavis", icon = icon("globe",lib='glyphicon')),
    menuItem("treemap", tabName = "treemap", icon = icon("tree-conifer",lib='glyphicon')),
    menuItem("UAI.cl", icon = icon("send",lib='glyphicon'),href = "http://www.uai.cl")
    ) # End sidebarMenu
) # End dashboardSidebar

frow1 <- fluidRow(
  valueBoxOutput("value1"),
  valueBoxOutput("value2"),
  valueBoxOutput("value3")
) # End fluidRow

frow2 <- fluidRow(
  
  box(
    title = "Histograma de Mercado",
    status = "primary",
    solidHeader = TRUE,
    collapsible = TRUE,
    plotOutput("histmercado", height = "300px")
  ),# End box
  
  box(
    title = "Histograma de Materia",
    status = "primary",
    solidHeader = TRUE,
    collapsible = TRUE, 
    plotOutput("histmateria", height = "300px")
  ) # End box
  
) # End fluidRow

frow3 <- fluidRow(
  
  box(
    title = "Histograma de Categoria",
    status = "primary",
    solidHeader = TRUE,
    collapsible = TRUE,
    plotOutput("histcategoria", height = "300px")
  ),# End box
  
  box(
    title = "Histograma de Conclusiones",
    status = "primary",
    solidHeader = TRUE,
    collapsible = TRUE, 
    plotOutput("histconclusion", height = "300px")
  ) # End box
  
) # End fluidRow

# Define UI ----
fpage1 <- shinyUI(
  fluidPage( responsive = TRUE,
     
     # Application title ----
     titlePanel("Word Cloud de documentos del TDLC"),
     
     # Sidebar layout with input and output definitions ----
     sidebarLayout(
       
       # Sidebar panel for inputs ----
       sidebarPanel(
         
         # Input: Slider for the list of documents ----
         selectInput("in_selection", "Seleccione un documento:",
                     choices = doc_list),
         
         # Input: Action button to refresh data ----
         actionButton("in_update", "Actualizar"),
         hr(),
         
         # Input: Slider for the minimum frequency ----
         sliderInput("in_freq",
                     "Frecuencia mínima:",
                     min = 1, max = 50, value = 10),
         
         # Input: Slider for the maximum number of words ----
         sliderInput("in_max",
                     "Número máximo de palabras:",
                     min = 1, max = 100, value = 50)
         
       ),  # end sidebarPanel
               
      # Main panel for displaying outputs ----
      mainPanel(
      # Output: Wordcloud ----
      plotOutput("out_plot")
      )  # end mainPanel
    )  # end sidebarLayout
  )  # end fluidPage
) # End shinyUI

fpage2 <- shinyUI(
  fluidPage( responsive = TRUE, 
    sliderInput("nTerms", "Number of terms to display", min = 20, max = 40, value = 30),
    visOutput('myChart')
  ) # End fluidPage
) # End shinyUI

fpage3 <- shinyUI(
  fluidPage( 
    tabsetPanel(
      tabPanel("Mercados",
    responsive = TRUE,
      loadEChartsLibrary(),
      #tags$div(id="TreeMap", style="width:100%;height:500px;"),
      plotOutput("TreeMap")),
    tabPanel("Conclusiones del TDLC",
             responsive = TRUE,
             loadEChartsLibrary(),
             plotOutput("TreeMap1")
    ),
    
    tabPanel("Categorías de documentos",
             responsive = TRUE,
             loadEChartsLibrary(),
             plotOutput("TreeMap2")
    ),
    
    
    tabPanel("Materias",
             responsive = TRUE,
             loadEChartsLibrary(),
             plotOutput("TreeMap3")
    )
    
    )
    
  ) # End fluidPage
) # End shinyUI

#fpage4 <- shinyUI(
 # fluidPage( responsive = TRUE,
  #           loadEChartsLibrary(),
   #          plotOutput("TreeMap1")
#  ) # End fluidPage
#) # End shinyUI

#fpage5 <- shinyUI(
#  fluidPage( responsive = TRUE,
#             loadEChartsLibrary(),
#             tags$div(id="test", style="width:100%;height:500px;"),
#             deliverChart(div_id = "test")
#  ) # End fluidPage
#) # End shinyUI

# Mezclamos en el dashboard el contenido
body <- dashboardBody(
  tabItems(
    tabItem("dashboard", frow1, frow2,frow3),
    tabItem("wordcloud", fpage1),
    tabItem("ldavis", fpage2),
    tabItem("treemap", fpage3)
    ) # end tabItems
) # end dashboardBody

#completing the ui part with dashboardPage
ui <- dashboardPage(title = 'Text Mining & Law', header, sidebar, body ,skin='green')

# create the server functions for the dashboard  

server <- function(input, output, session) { 

  # create the server functions for the dashboard  

    #some data manipulation to derive the values of KPI boxes
    TDLC$Value <- as.integer(TDLC$Value)
    
    mercado <- as.numeric(as.factor(as.character(TDLC$mercado)))
    materia <- as.numeric(as.factor(as.character(TDLC$materia)))
    
    categoria <- as.numeric(as.factor(as.character(TDLC$categoria)))
    conclusion <- as.numeric(as.factor(as.character(TDLC$conclusión)))
    
    categorias <- TDLC %>% group_by(categoria) %>% summarise(value = sum(Value)) %>% filter(value==max(value))
    mercados <- TDLC %>% group_by(mercado) %>% summarise(value = sum(Value)) %>% filter(value==max(value))
    conclusiones <- TDLC %>% group_by(conclusión) %>% summarise(value = sum(Value)) %>% filter(value==max(value))    

    #creating the valueBoxOutput content
    output$value1 <- renderValueBox({
      valueBox(
        formatC(mercados$value, format="d", big.mark=',')
        ,paste('Maryores quejas:',mercados$mercado)
        ,icon = icon("stats",lib='glyphicon')
        ,color = "red")  
    }) # ends output value 1
    output$value2 <- renderValueBox({
      valueBox(
        formatC(categorias$value, format="d", big.mark=',')
        ,paste('Categoría principal:',categorias$categoria)
        ,icon = icon("stats",lib='glyphicon')
        ,color = "purple")  
    }) # ends output value 2
    output$value3 <- renderValueBox({
      valueBox(
        formatC(conclusiones$value, format="d", big.mark=',')
        ,paste('Conclusión más repetida:',conclusiones$conclusión)
        ,icon = icon("menu-hamburger",lib='glyphicon')
        ,color = "yellow")   
    }) # ends output value 3
    
    #creating the plotOutput content

    output$histmercado <- renderPlot({
     hist(mercado, breaks = "Sturges",
          include.lowest = TRUE, right = TRUE,
          density = NULL, col = "light blue",
          xlab = "Mercado", axes = TRUE, plot = TRUE,probability = TRUE)
      lines(density(mercado), col="red", type = "l",lwd=2)
    })
    
    output$histmateria <- renderPlot({
      hist(materia, breaks = "Sturges",
           include.lowest = TRUE, right = TRUE,
           density = NULL, col = "green",
           xlab = "materia", axes = TRUE, plot = TRUE,probability = TRUE)
      lines(density(materia), col="orange", type = "l",lwd=2)
    })
    
    output$histcategoria<- renderPlot({
      hist(categoria, breaks = "Sturges",
           include.lowest = TRUE, right = TRUE,
           density = NULL, col = "purple",
           xlab = "categoria", axes = TRUE, plot = TRUE,probability = TRUE)
      lines(density(categoria), col="green", type = "l",lwd=2)
    })
    
    output$histconclusion <- renderPlot({
      hist(conclusion, breaks = "Sturges",
           include.lowest = TRUE, right = TRUE,
           density = NULL, col = "light grey",
           xlab = "conclusion", axes = TRUE, plot = TRUE,probability = TRUE)
      lines(density(conclusion), col="dark orange", type = "l",lwd=2)
    })
  
# ================= EDO ========================== #

  output$myChart <- memoise(renderVis({
    withProgress({
      setProgress(message = "Procesando LDAVis...")
      
    if(!is.null(input$nTerms)){
      with(myLDA, 
      createJSON(phi, theta, doc.length, vocab, term.frequency, R = input$nTerms))
    } 
    }) 
  }))

# ================= NICO ========================== #
  
  # Define a reactive expression for the document term matrix ----
  terms <- reactive({
    
    # Change when the "update" button is pressed...
    input$in_update
    # ...but not for anything else
    isolate({
      withProgress({
        setProgress(message = "Procesando Corpus...")
        get_term_matrix(input$in_selection)
      })
    })
  })
  
  # Make the wordcloud drawing predictable during a session ----
  wordcloud_rep <- repeatable(wordcloud)
  
  # Generate plot of wordcloud ----
  output$out_plot <- renderPlot({
    
    # Vector de frecuencias por palabras ----
    v <- terms()
    
    # Wordcloud ----
    wordcloud_rep(names(v), v,
                  scale = c(4, 0.5),
                  min.freq = input$in_freq,
                  max.words = input$in_max,
                  colors = brewer.pal(8, "Dark2"))
  })  # end renderPlot
  
  # Render TreeMap
  
  output$TreeMap <- renderPlot({
    mytable <- as.data.frame(table(TDLC$mercado))
    treemap(mytable, title = "Treemap Mercado aludido por el documento",
            index = "Var1", vSize = "Freq", type = "index", mirror.x = TRUE,palette="HCL", 
            fontsize.title = 14, border.col = NULL, inflate.labels = TRUE 
            )
  })  # end renderPlot
  
  output$TreeMap1 <- renderPlot({
    mytable1 <- as.data.frame(table(TDLC$conclusión))
    treemap(mytable1, title = "Treemap Conclusión del TDLC", 
            index = "Var1", vSize = "Freq", type = "index",mirror.x = TRUE,palette="HCL",
            fontsize.title = 14,  border.col = NULL,inflate.labels = TRUE 
    )
  })  # end renderPlot
  
  output$TreeMap2 <- renderPlot({
    mytable2 <- as.data.frame(table(TDLC$categoria))
    treemap(mytable2, title = "Treemap Categroías TDLC", 
            index = "Var1", vSize = "Freq", type = "index",mirror.x = TRUE,palette="HCL",
            fontsize.title = 14, border.col = NULL, inflate.labels = TRUE )
  })  # end renderPlot
  
  output$TreeMap3 <- renderPlot({
    mytable3 <- as.data.frame(table(TDLC$materia))
    treemap(mytable3, title = "Treemap Materias TDLC", 
            index = "Var1", vSize = "Freq", type = "index",mirror.x = TRUE,palette="HCL",
            fontsize.title = 14, border.col = NULL, inflate.labels = TRUE 
            )
  })  # end renderPlot
  
  #output$TreeMap2 <- renderPlot({
  #  renderTreeMap(div_id = "test", data = dat)
  #})  # end renderPlot
  
  
}  # end server

#run/call the shiny app
shinyApp(ui,server)

