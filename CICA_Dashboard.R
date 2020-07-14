library(shiny)
library(gmodels) ### crosstabs print
library(plotly)  ### Interactive plots
library(mclust)  ### adjustedRandIndex
library(caret)   ### confusion matrix

# Define UI 
ui <- fluidPage(
  
  # Application title
  titlePanel("Upload CICA partitioning and loss data"),
  
  # Sidebar with fileinput selection and reactive model input 
  sidebarLayout(
    sidebarPanel(
      
      fileInput("file1", "Choose .Rdata file",
                multiple = TRUE),
      
      ### uiOutput reactive version of ui. Works for selecting specific model
      uiOutput("reactiveInput1"),
      uiOutput("reactiveInput2")
      
    ),
    
    # mainpanel vertical layout --> ARI --> tabel --> plotly
    mainPanel(
      
      verticalLayout(
        h2("ARI", align = "center"),verbatimTextOutput('ARI'),
        h2("Table", align = "center"),verbatimTextOutput('Table'),
        plotlyOutput("Plot") 
        
      )
      
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  ### main reactive function that reads in the data.
  ### is called by all other functionalities (ARI, table, plotly)
  data <- reactive({
    file <- input$file1
    if(is.null(file)){
      return(NULL)
    }
    dat <- get( load( input$file1$datapath) )
    return(dat)
  })
  
  
  ### reactive input --> change ui modsel options based on data
  output$reactiveInput1 <- renderUI({
    dat <- data()
    dat <- dat$DFlab
    selectInput('labsel', 'Select labels', names( dat )[1:2] )
  })
  
  output$reactiveInput2 <- renderUI({
    dat <- data()
    dat <- dat$DFlab
    selectInput('datsel', 'Select model', names( dat ) )
  })
  
  
  output$ARI <- renderPrint({
    dat <- data()
    dat <- dat$DFlab
    
    truelab <- dat[ ,input$labsel]
    P <- dat[ , input$datsel]
    
    print(adjustedRandIndex(truelab, P) )
  })
  
  
  output$Table <- renderPrint({
    dat <- data()
    dat <- dat$DFlab
    
    truelab <- dat[ ,input$labsel]
    P <- dat[ , input$datsel]
    CrossTable(truelab,P, expected = F, prop.r = F, prop.c = F, 
               prop.t = F, prop.chisq = F, chisq = F)
  })
  
  output$Plot <- renderPlotly({
    dat <- data()
    dat <- dat$DFloss
    dat <- dat[[ input$datsel ]]
    plotdat <- data.frame(y = (dat/1000), x = 1:length(dat))
    
    plot_ly(plotdat, x = ~x[-1], y = ~y[-1], type = 'scatter', mode = 'lines')
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

