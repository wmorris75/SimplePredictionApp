library(shiny)
shinyUI(pageWithSidebar(
  headerPanel("General Purpose Prediction App"),
  sidebarPanel(
    

    checkboxInput('header', 'Header', TRUE),
    radioButtons('sep', 'Separator',
                 c(Comma=',',
                   Semicolon=';',
                   Tab='\t'),
                 ',', inline=T),
    tags$hr(),
    radioButtons('pred_option', 'What are you trying to predict?',
                 c('Numeric Value'='numeric',
                   'Boolean Value'="bool"),
                 inline=T ),
    
    tags$hr(),
    radioButtons('quote', 'Quote',
                 c(None='',
                   'Double Quote'='"',
                   'Single Quote'="'"),
                 '"', inline=T),

    tags$hr(),
    #textInput(inputId="filename", label = "Input Filename(Path)"),

    textInput(inputId="predictor", label = "State the parameter you want to predict as it appears in file"),
    
    radioButtons("all_features", "Features:", c("Use selected features" = "sel_feature", "Use all features" = "use_all_features")),
    textInput(inputId="features", label = "List features/parameters to be used for prediction (features should be comma separated)"),
    textInput(inputId="feature_value", label = "Enter each feature value with comma separation"),
    
    radioButtons("method", "Methods:",
                 c("Random Forest" = "rf",
                   "Bagging" = "gbm",
                   "Rpart" = "rpart",
                   "General Linear Model" = "glm"),
                 inline=T),
    tags$hr(),
    
    fileInput('file1', 'Choose CSV File',
              accept=c('text/csv', 
                       'text/comma-separated-values,text/plain', 
                       '.csv')),
    tags$hr(),
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