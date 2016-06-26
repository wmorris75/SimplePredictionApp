library(caret)
library(randomForest)
library(gbm)
library(ggplot2)
library(MASS)
library(shiny)

#Enter the attribute as it appears in the csv 
get_prediction<-function(path, algorithm, predictor){
  out<-tryCatch({
    data<-read.csv(path)
    pred_variable <- gsub("\\s", ".", predictor)
    inTrain <- createDataPartition(data[[pred_variable]], p=0.7, list=FALSE)
    print(pred_variable)
    training<-data[inTrain,]
    testing<-data[-inTrain,]
    
    #set.seed(323)
    nbFit<-train(training[[pred_variable]] ~ ., data=training, method=algorithm)
    nbprediction<-predict(nbFit, testing)
    print ("Reach")
    #Accuracy
    e_message<-""
    results = tryCatch({
      outpput<-confusionMatrix(nbprediction, testing[[pred_variable]])
      print(output)
      return("success")
    },
    error = function(e){
      e_out = "Most likely no values were predicted correctly. Will display in table format"
      message(e_out)
      return((e_out));
      #e_message<- as.character(message(e)); print(error); return(message(e));
      
    }) #end of inner tryCatch()
    if(grep("most likely no values", results, ignore.case=TRUE)){
      output<-table(nbprediction, testing[[pred_variable]]) 
      print(output)
      }
  }, error = function(e){
    message("Ensure the data is properly cleaned such as removing NAs, using correct data types, etc.")
    message(e)
    return()
  }) 
  return (out)
}
 