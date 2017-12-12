---
title: 'Prediction Assignment  '
author: "Abhishek"
output:
  html_document:
    keep_md: yes
  pdf_document: default
---



## Introduction

Using devices such as Jawbone Up, Nike FuelBand, and Fitbit it is now possible to collect a large amount of data about personal activity relatively inexpensively.In this project, our goal will be to use data from accelerometers on the belt, forearm, arm, and dumbell of 6 participants and  predict the manner in which they did the exercise. This is the "classe" variable in the training set. 


```r
download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv","trainingdata.csv")
download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv","testingdata.csv")
training_data_all_cols <- read.csv("trainingdata.csv",na.strings = c("NA",""))
testing_data_all_cols <- read.csv("testingdata.csv",na.strings = c("NA",""))
```
## Splitting Training Data and creating Cross Validation Data with 30% of the data

```r
library("caret")
```

```
## Warning: package 'caret' was built under R version 3.4.3
```

```r
training_data_index  <- createDataPartition(training_data_all_cols$classe,p=0.7, list = FALSE)
training_data_set <- training_data_all_cols[training_data_index,]
training_data_crossvalid <- training_data_all_cols[-training_data_index,]
```

## Cleaning Data . This will be done in two steps.  

```r
##Since we need to use data from accelerometers on the belt, forearm, arm, and dumbell we will drop the other columns.

training_data_set <- training_data_set[grep('belt|arm|dumbell|forearm|classe', colnames(training_data_set)) ]
training_data_crossvalid <- training_data_crossvalid[grep('belt|arm|dumbell|forearm|classe', colnames(training_data_crossvalid)) ]
testing_data_set <- testing_data_all_cols[grep('belt|arm|dumbell|forearm|classe', colnames(testing_data_all_cols)) ]

##In the second step we will drop the columns which have more than 50% of NA

training_data_set   <-       training_data_set[, -which(colMeans(is.na(training_data_set)) > 0.5)]
training_data_crossvalid   <-       training_data_crossvalid[, -which(colMeans(is.na(training_data_crossvalid)) > 0.5)]
testing_data_set   <-       testing_data_set[, -which(colMeans(is.na(testing_data_set)) > 0.5)]
```
## Using the Random Forest Model for Prediction.

```r
library(randomForest)
```

```
## Warning: package 'randomForest' was built under R version 3.4.3
```

```r
randomforestmodelfit <- randomForest(classe ~ . , data = training_data_set, na.action = na.omit)
randomforestmodelfit
```

```
## 
## Call:
##  randomForest(formula = classe ~ ., data = training_data_set,      na.action = na.omit) 
##                Type of random forest: classification
##                      Number of trees: 500
## No. of variables tried at each split: 6
## 
##         OOB estimate of  error rate: 0.81%
## Confusion matrix:
##      A    B    C    D    E class.error
## A 3901    3    1    1    0 0.001280082
## B   14 2632   12    0    0 0.009781791
## C    1   29 2347   18    1 0.020450751
## D    0    0   22 2226    4 0.011545293
## E    0    0    1    4 2520 0.001980198
```

##We will Cross Validate our model using the cross validation data . We will use comfusion matrix to determine our accuracy


```r
predictedcross <- predict(randomforestmodelfit,newdata = training_data_crossvalid)

pconfusion <- confusionMatrix(predictedcross, training_data_crossvalid$classe)
pconfusion
```

```
## Confusion Matrix and Statistics
## 
##           Reference
## Prediction    A    B    C    D    E
##          A 1673    6    1    0    0
##          B    1 1132    5    0    0
##          C    0    1 1011   16    0
##          D    0    0    8  947    3
##          E    0    0    1    1 1079
## 
## Overall Statistics
##                                           
##                Accuracy : 0.9927          
##                  95% CI : (0.9902, 0.9947)
##     No Information Rate : 0.2845          
##     P-Value [Acc > NIR] : < 2.2e-16       
##                                           
##                   Kappa : 0.9908          
##  Mcnemar's Test P-Value : NA              
## 
## Statistics by Class:
## 
##                      Class: A Class: B Class: C Class: D Class: E
## Sensitivity            0.9994   0.9939   0.9854   0.9824   0.9972
## Specificity            0.9983   0.9987   0.9965   0.9978   0.9996
## Pos Pred Value         0.9958   0.9947   0.9835   0.9885   0.9981
## Neg Pred Value         0.9998   0.9985   0.9969   0.9965   0.9994
## Prevalence             0.2845   0.1935   0.1743   0.1638   0.1839
## Detection Rate         0.2843   0.1924   0.1718   0.1609   0.1833
## Detection Prevalence   0.2855   0.1934   0.1747   0.1628   0.1837
## Balanced Accuracy      0.9989   0.9963   0.9909   0.9901   0.9984
```

```r
## We observed that our model is giving an accuracy of around 99%
```

##We will now use our model to make a prediction on the Test Data


```r
predictedtest <- predict(randomforestmodelfit,newdata = testing_data_set)
predictedtest
```

```
##  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 
##  B  A  B  A  A  E  D  B  A  A  B  C  B  A  E  E  A  B  B  B 
## Levels: A B C D E
```
