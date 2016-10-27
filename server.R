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


build_formula<-function(value_to_predict, input_vector){
  vector<-get_feature_entry(input_vector)
  formula<-paste(value_to_predict, " ~ ")
  if(length(vector) > 1){
    for(item in 2:length(vector) - 1){print(item);
      print(formula)
      formula<-paste(formula, vector[item], " + ")
    }
  }
  formula <- paste(formula, vector[length(vector)])
  print(formula)
  return(formula)
}

get_df_names<-function(df){
  names <- names(df)
  name_values <- strsplit(names, " ")
  vector <- c()
  for (i in 1:length(name_values)){
    vector <- c(vector, name_values[i])
  }
  return (vector)
}

get_feature_entry<-function(input){
  sep_val <- strsplit(input, ",")
  name_values <- strsplit(sep_val[[1]], " ")

  vector <- c()
  for (i in 1:length(name_values)){
    vector <- c(vector, name_values[i])
  }
  return (vector)
}

train_data_partitiion<-function(data, algorithm, predictor){
    pred_variable <- gsub("\\s", ".", predictor)
    inTrain <- createDataPartition(data[[pred_variable]], p=0.7, list=FALSE)
    return(inTrain)
}

overall_accurracy<-function(predicted, actual){
  diff = actual - predicted
  diff[which(diff < 0)] = -1 * diff[which(diff < 0)]
  accuracy <- 100 * (1 - mean((diff/actual)/actual))
  return(accuracy)
}

determine_data_type<-function(input){
    for(value in 1:length(input)){
      # if(length(grep("^[[:digit:]]$ | ^.[[:digit:]] | ^[[:digit:]].[[:digit:]]$", input[value])) > 0){
      if(!(is.na(as.numeric(input[value])))){
        input[value] <- as.numeric(input[value])
      } 
    }
  return(input)
}

#Enter the attribute as it appears in the csv 
get_prediction<-function(data, algorithm, predictor, inTrain, option, features){
  out <- tryCatch({
    pred_variable <- gsub("\\s", ".", predictor)
    # inTrain <- createDataPartition(data[[pred_variable]], p=0.7, list=FALSE)
    training<-data[inTrain,]
    testing<-data[-inTrain,]

    if(option == "sel_feature"){
      formula = build_formula(pred_variable, features)
      nbFit<-train(as.formula(formula), data=training, method=algorithm)
    }
    else{
      nbFit<-train(training[[pred_variable]] ~ ., data=training, method=algorithm)
    }
    
    return (nbFit)
    nbprediction<-predict(nbFit, testing) 
    #Accuracy
    e_message<-""
    results = tryCatch({
      output<-table(nbprediction, testing[[pred_variable]])#$overall[1]
      return(output)
    },
    error = function(e){
      e_out = "Most likely no values were predicted correctly. Will display in table format"
      m<-message(e)
      return((e));
      
    }) #end of inner tryCatch()
    if(grep("most likely no values", results, ignore.case=TRUE)){
      output<-table(nbprediction, testing[[pred_variable]])#$overall[1]
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
        
        inTrain <- train_data_partitiion(df, input$method, input$predictor)
        train_data <- df[inTrain,]
        test_data <- df[-inTrain,]
        names <- get_df_names(train_data)

        if(input$all_features == "sel_feature"){
          vector<- get_feature_entry(input$features)
          print (vector)
          testing <- subset(test_data, select=as.character(vector))
          names <- head(testing)
        }
        
        feature_value<-get_feature_entry(input$feature_value)
        
        if (length(feature_value)> 0){
          feature_value<-determine_data_type(feature_value)
        }
        n_columns <- length(feature_value)

        df_forecast <- data.frame(matrix(ncol = n_columns, nrow = 0))
        colnames(df_forecast) <- get_feature_entry(input$features)

        f_value<-rbind(df_forecast, feature_value)
        names(f_value) <- names(testing)
        option <- input$all_features
        features <- input$features
        
        model<-get_prediction(df, input$method, input$predictor, inTrain, option, features)
        
        prediction<-predict(model, testing)
        out<-predict(model, f_value)
        isolate(paste("Result is: ", out))
        # out<-{get_prediction(df, input$method, input$predictor, inTrain, option, features)}
        # if(is.numeric(out)){
        #   isolate(paste("Result is: ", out))
        # }
        # else if(is.character(out)){
        #   isolate(paste("ERROR ----> ", out))
        # }
        # else if(is.table(out)){
        #   isolate(paste("TABLE ----> ", out))
        # }
      }
    })
    
  }
)