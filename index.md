---
title       : A Simple App for Making Predictions
subtitle    : A Simple App for Making Predictions Using Machine Learning Algorithms
author      : Wayne Morris
job         : Software Engineer/Data Scientist
framework   : io2012        # {io2012, html5slides, shower, dzslides, ...}
highlighter : highlight.js  # {highlight.js, prettify, highlight}
hitheme     : tomorrow      # 
widgets     : []            # {mathjax, quiz, bootstrap}
mode        : selfcontained # {standalone, draft}
knit        : slidify::knit2slideslis
---

## Introduction

This app utilizes random forest, bagging, general linear models and rpart as the Machine Learning(ML) Alogrithms to choose from when making predictions. The app is very simple to use and all it requires is for the user to state the filename which contains the data, the value to be predicted and a ML algorithm to use for making the prediction. 


--- .class #id 

## How to Use
The app requires some amount of data preprocessing/cleaning before usage. For instances, the removal of NAs may be necessary before using this app. The app also assumes that the data being entered is already cleaned and contains all the required features.

Make sure you have all the required libraries to run this app. The app uses the following libraries:
      <ul>
        <li>Shiny</li>
        <li>gbm</li>
        <li>ggplot2</li>
        <li>Caret</li>
        <li>MASS</li>
        <li>Random Forest</li>
      </ul>

--- .class #id 

## The R Code for App
The simple version of the R code is shown below. In the actual version on <a href="http://www.github.com/wmorris75/PredictionApp">github</a>, it incorporates error handling technique for returning customized messages to the user and in cases where there is no prediction results, the output produces a table to show comparison of the actual results with the prediction results.

---

## The Code for App(cont)

```r
get_prediction<-function(path, algorithm, predictor){
    data<-read.csv(path)
    pred_variable <- gsub("\\s", ".", predictor)
    inTrain <- createDataPartition(data[[pred_variable]], p=0.7, list=FALSE)
    training<-data[inTrain,]
    testing<-data[-inTrain,]
    
    #set.seed(323)
    nbFit<-train(training[[pred_variable]] ~ ., data=training, method=algorithm)
    nbprediction<-predict(nbFit, testing)
    
    output<-confusionMatrix(nbprediction, testing[[pred_variable]])$overall[1]
    return(output)
}
```

---

## App Limitations and Future Work

There are a number of limitations with the App. As mentioned, the app does not do any form of data preprocessing. Preprocessing has to be handled as desired by the user. This is one aspect that will be focused on improving to lessen the manual processing by the user.

The app also will throw an error when it does not find any overlapping reference value in its prediction results. This means that the algorithm used did not produce a any matching values from the model developed by the training set with the test sets. The results of these occurrences could be attributed to a number of issues, for example, float data types which differ by decimal instead of numeric values being rounded to the nearest integer. When cases like these occur, it does not mean the app cannot predict the value but the prediction given might match closely to what would be obtained similar to a regression model. 

To improve this feature of the app, the focus would be to utilize other models, such as linear regression model, that are a better fit for events like these. Displaying statistical output to give user a better interpretation of the results will also be considered.



 







