library(shiny)
library(datasets)
library(caret)
library(randomForest)
library(gbm)
library(ggplot2)
library(MASS)
library(e1071)
data("mtcars")

# We tweak the "am" field to have nicer factor labels. Since this doesn't
# rely on any user inputs we can do this once at startup and then use the
# value throughout the lifetime of the application
mpgData <- mtcars
mpgData$am <- factor(mpgData$am, labels = c("Automatic", "Manual"))

get_pred<-function(data, algorithm, predictor){
  pred_variable <- gsub("\\s", ".", predictor)
  inTrain <- createDataPartition(data[[pred_variable]], p=0.7, list=FALSE)
  training<-data[inTrain,]
  testing<-data[-inTrain,]
  
  #set.seed(323)
  nbFit<-train(training[[pred_variable]] ~ ., data=training, method=algorithm)
  nbprediction<-predict(nbFit, testing) 
  output<-confusionMatrix(nbprediction, testing[[pred_variable]])$overall[1]
  return (output)
}

#Enter the attribute as it appears in the csv 
get_prediction<-function(data, algorithm, predictor){
  out <- tryCatch({
    pred_variable <- gsub("\\s", ".", predictor)
    inTrain <- createDataPartition(data[[pred_variable]], p=0.7, list=FALSE)
    training<-data[inTrain,]
    testing<-data[-inTrain,]
    
    #set.seed(323)
    nbFit<-train(training[[pred_variable]] ~ ., data=training, method=algorithm)
    nbprediction<-predict(nbFit, testing)
    
    #Accuracy
    e_message<-""
    results = tryCatch({
      output<-confusionMatrix(nbprediction, testing[[pred_variable]])$overall[1]
      return(output)
    },
    error = function(e){
      e_out = "Most likely no values were predicted correctly. Will display in table format"
      m<-message(e)
      return((e));
      
    }) #end of inner tryCatch()
    if(grep("most likely no values", results, ignore.case=TRUE)){
      output<-table(nbprediction, testing[[pred_variable]])$overall[1]
      return(output)
    }
  }, error = function(e){
    e_message = "Ensure the data is properly cleaned such as removing NAs, using correct data types, etc."
    #message(e_message)
    e_message<- message(e)
    return (e_message)
  }) 
  return (out)
}

shinyServer(
  function(input, output) {
    output$file <- renderText(paste("Filename: ", {input$filename}))
    output$pred <- renderText(paste("Value to forecast: ", {input$predictor}))
     
    
    output$value<-renderText({
      input$predict
      if({input$predictor != ""}){

        inFile <- input$file1
        
        if (is.null(inFile))
          return(NULL)
        
        df<-read.csv(inFile$datapath, header=input$header, sep=input$sep, 
                 quote=input$quote)
        
        out<-{get_prediction(df, input$method, input$predictor)}
        if(is.numeric(out)){
          isolate(paste("Accuracy is: ", out))
        }
        else if(is.character(out)){
          isolate(paste("ERROR ----> ", out))
        }
        else if(is.table(out)){
          isolate(paste("TABLE ----> ", out))
        }
      }
    })
    
  }
)