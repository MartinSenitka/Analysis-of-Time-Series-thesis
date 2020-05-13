library(shiny)
library(shinythemes)
library(plotly)
shinyUI(navbarPage(
    theme = shinytheme("cyborg"),
    strong("Analysis of Time Series in practice"),
    tabPanel("Your Data",
             sidebarLayout(
                 sidebarPanel(
                     radioButtons("RD1",label=h3(strong("Choose your Data")),choices = list("Your Data"=1,"Air Passengers"=2),selected = 1),
                     
                     conditionalPanel(condition="input.RD1==1",fileInput('file1', h4(strong('csv file input')),
                                                                         accept=c('text/csv', 
                                                                                  'text/comma-separated-values,text/plain', 
                                                                                  '.csv')),
                                      checkboxInput('header', 'Does your data include header?', TRUE),
                                      radioButtons('sep', 'Separator of your Data',
                                                   c(Comma=',',
                                                     Semicolon=';',
                                                     Tab='\t'),
                                                   ';'),
                                      
                     )
                     ,
                     numericInput("Col",strong("Select column for analysis"),value = 2)
                     
                     ,
                     
                     h4(strong("Time Series")),
                     numericInput("Start",strong("Beginning"),value = 1929),
                     
                     numericInput("freq",strong("Frequency"),value=12),
                     h4(strong("Setting of the holdout sample")),
                     numericInput("Starth",strong("Start"),value = 1940),
                     numericInput("Endh",strong("End"),value = 1941)
                 ),
                 
                 
                 mainPanel(fluidRow(
                     
                     column(10,h3("Summary of your Data"),verbatimTextOutput("summary")),
                     column(10,h3("Data table"),dataTableOutput("table")),
                     column(10,h3("Plot of your Data"),plotlyOutput("PlotG"))
                     
                 )
                 
                 )
                 
             )),
    
    tabPanel("Linear",
             sidebarLayout(
                 sidebarPanel(
                     
                   sliderInput("yearL",label="Select year to predict",min=1,max=100,value=7)
                   
                 ),
                 mainPanel(
                     fluidRow(
                         h3('About'),
                         p('Simple linear smoothing',align='justify'),
                         
                         column(10,h4(strong('Forecasting Plot')),plotlyOutput("LinearPlot")),
                         column(10,h4(strong('Formula')),textOutput("LinearFormula")),
                         column(10,h4(strong('Prediction')),textOutput("LinearPrediction")),
                         column(10,h4(strong('Accuracy Table')),tableOutput("LinearAccuracyTable")),
                         column(10,h4(strong('Accuracy Bar Plot')),plotlyOutput("LinearAccuracyBar"))
                     )
                 )
             )),
    
    
    tabPanel("Quadratic",
             sidebarLayout(
               sidebarPanel(
                 
                 sliderInput("yearQ",label="Select year to predict",min=1,max=100,value=7)
                 
               ),
               mainPanel(
                 fluidRow(
                   h3('About'),
                   p('Simple quadratic smoothing',align='justify'),
                   
                   column(10,h4(strong('Forecasting Plot')),plotlyOutput("QuadraticPlot")),
                   column(10,h4(strong('Formula')),textOutput("QuadraticFormula")),
                   column(10,h4(strong('Prediction')),textOutput("QuadraticPrediction")),
                   column(10,h4(strong('Accuracy Table')),tableOutput("QuadraticAccuracyTable")),
                   column(10,h4(strong('Accuracy Bar Plot')),plotlyOutput("QuadraticAccuracyBar"))
                 )
               )
             )),
    
    tabPanel("Exponentional",
             sidebarLayout(
                 sidebarPanel(
                     
                   sliderInput("yearE",label="Select year to predict",min=1,max=100,value=7)
                   
                 ),
                 mainPanel(
                     fluidRow(
                         h3('About'),
                         p('Simple exponential smoothing',align='justify'),
                         
                         column(10,h4(strong('Forecasting Plot')),plotlyOutput("ExponentialPlot")),
                         column(10,h4(strong('Formula')),textOutput("ExponentionalFormula")),
                         column(10,h4(strong('Prediction')),textOutput("ExponentionalPrediction")),
                         column(10,h4(strong('Accuracy Table')),tableOutput("ExponentialAccuracyTable")),
                         column(10,h4(strong('Accuracy Bar Plot')),plotlyOutput("ExponentialAccuracyBar"))
                     )
                 )
             )),
 
    tabPanel("Box Jenkins/Arima",
             sidebarLayout(
                 sidebarPanel(
                     
                     radioButtons("Data",h4(strong("Data")),choices = list("Use Air Passengers Data"=1,"Use input data"=2),selected = 1),
                     
                     sliderInput("CI5",label="Confidence Interval",min=0.01,max=0.99,value=0.9)
                     
                 ),
                 
                 mainPanel(
                     fluidRow(
                         h3('Model Introduction'),
                         p('Box Jenkins',align='Justify'),
                         
                         column(10,h4(strong('Forecasting Plot')),plotOutput("Plot11")),
                         column(10,h4(strong('Accuracy Table')),tableOutput("accu6")),
                         column(10,h4(strong('Accuracy Bar Plot')),plotlyOutput("Plot12"))
                     )
                 )
             )),
    
    
    tabPanel("Machine learning",
             sidebarLayout(
               sidebarPanel(
                 
               ),
               mainPanel(
                 fluidRow(
                   h3('About'),
                   p('Simple Machine Learning algorithm with Multiple Linear regression.',align='Justify'),
                   p('Plot shows differences between the dataset and predicted data for the choosen column.',align='Justify'),
                   
                   
                   
                   
                   
                   column(10,h4(strong('Forecasting Plot')),plotlyOutput("MLPlot")),
                   column(10,h4(strong('Formula')),textOutput("MlFormula")),
                   column(10,h4(strong('Results')),tableOutput("MachineLearningTable"))
                   
                   
                 )
               )
               
             )),
    
    #### INFO
    tabPanel("Info",
             h3(strong("About")),
             br(),
             p("Time series and machine learning regression shinny app",align="Justify"),
             
             br(),
             h3(strong("Author")),
             p("Author: Martin Senitka"),
             p("Email: Martinsenitka@gmail.com"),
             p("Download the project: https://github.com/MartinSenitka/Analysis-of-Time-Series-thesis")

             
    )
    
)
)



