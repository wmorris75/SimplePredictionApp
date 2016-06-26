library(shiny)
shinyUI(pageWithSidebar(
  headerPanel("Lets Make a Prediction APP"),
  sidebarPanel(
    
    fileInput('file1', 'Choose CSV File',
              accept=c('text/csv', 
                       'text/comma-separated-values,text/plain', 
                       '.csv')),
    tags$hr(),
    checkboxInput('header', 'Header', TRUE),
    radioButtons('sep', 'Separator',
                 c(Comma=',',
                   Semicolon=';',
                   Tab='\t'),
                 ','),
    radioButtons('quote', 'Quote',
                 c(None='',
                   'Double Quote'='"',
                   'Single Quote'="'"),
                 '"'),

    
    #textInput(inputId="filename", label = "Input Filename(Path)"),

    textInput(inputId="predictor", label = "State value to be predicted as it appears in file"),
    radioButtons("method", "Methods:",
                 c("Random Forest" = "rf",
                   "Bagging" = "gbm",
                   "Rpart" = "rpart",
                   "General Linear Model" = "glm")
    ),
    actionButton("predict", "Predict")
  ),
  mainPanel(
    textOutput('filename'),
    textOutput('predictor'),
    
    hr(),
    
    textOutput('results'),
    fluidRow(column(6, verbatimTextOutput("file"))),
    fluidRow(column(6, verbatimTextOutput("pred"))),
    p('Results'),
    fluidRow(column(6, verbatimTextOutput("value"))),
    tableOutput('contents')
  )
))