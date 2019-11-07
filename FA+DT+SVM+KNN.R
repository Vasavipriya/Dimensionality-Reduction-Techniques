library(caret)
library(psych)
library(GPArotation)
library(magrittr)
library(e1071)
library(rpart)
library(class)
library(parallelDist)

#IMPORTING THE DATA
data<-read.csv(file.choose(), header=TRUE, sep=",")

#REMOVE ZERO VARIANCE COLUMNS
data<-data[ , apply(data, 2, var) != 0]

#CONVERT CLASS TO FACTOR
data$class <- factor(data$class)

dim(data)
str(data$class)

#Factor Analysis

data1<-data[-118]
parallel <- fa.parallel(data1, fm = 'minres', fa = 'fa')

nofactor <- fa(data1,nfactors = 30,rotate = "oblimin",fm="minres",warnings=FALSE)
nofactor$Vaccounted


factordata<-nofactor$scores
DATA<-cbind(factordata,data[118])


#Decision Tree
set.seed(567)
k.folds <- function(k) {
    folds <- createFolds(DATA$class, k = k, list = TRUE, returnTrain = TRUE)
    for (i in 1:k) {
    
    control <- rpart.control(minsplit = 2,
    minbucket = round(2/ 3),
    maxdepth = 30,
    cp = 0)
    model <- rpart(class~.,data=DATA[folds[[i]],],method = "class",control = control)
        
        predictions <- predict(object = model, newdata =(DATA[-folds[[i]],])[-36], type = "class")
        accuracies.dt <- c(accuracies.dt, 
                           confusionMatrix(predictions,DATA[-folds[[i]], ]$class)$overall[[1]])
        f1.dt <- c(f1.dt, 
                           mean(na.omit(confusionMatrix(predictions,DATA[-folds[[i]], ]$class)$byClass[1:40,7])))

    }
    accuracies.dt
    f1.dt
}

accuracies.dt<-c()
f1.dt <- c()
accuracies.dt<-k.folds(10)
f1.dt <- k.folds(10)
f1.dt
accuracies.dt

#SVM
library(kernlab)
rbf <- rbfdot(sigma=0.1)
rbf

set.seed(666)
k.folds <- function(k) {
    folds <- createFolds(DATA$class, k = k, list = TRUE, returnTrain = TRUE)
    for (i in 1:k) {
  
    model <- ksvm(class~.,data=DATA[folds[[i]],],kernel="rbf",C =1000,gamma=0.0001,nu = 0.2, epsilon = 0.1, prob.model = FALSE,scale = TRUE)
        
        predictions <- predict(object = model, newdata = (DATA[-folds[[i]],])[-31])
        accuracies.dt <- c(accuracies.dt, 
                           confusionMatrix(predictions,DATA[-folds[[i]], ]$class)$overall[[1]])
        f1.dt <- c(f1.dt, 
                           mean(na.omit(confusionMatrix(predictions,DATA[-folds[[i]], ]$class)$byClass[1:40,7])))

    }
    accuracies.dt
    f1.dt
}

accuracies.dt<-c()
f1.dt <- c()
accuracies.dt<-k.folds(10)
f1.dt <- k.folds(10)
accuracies.dt
f1.dt

#Knn
library(class)

normalize<- function(x){
  return((x-min(x))/(max(x)-min(x)))
}
DATA[,1:35]<- normalize(DATA[,1:35])

set.seed(666)
k.folds <- function(k) {
    folds <- createFolds(DATA$class,k = k,list = TRUE,returnTrain = TRUE)
    for (i in 1:k) {
  
    traindata<-(DATA[folds[[i]],])[-36]
    newdata<-(DATA[-folds[[i]],])[-36]
    

    model <- knn(traindata,newdata,DATA[folds[[i]],]$class,k =4,l = 0, prob = FALSE, use.all =TRUE)
        
       
        accuracies.dt <- c(accuracies.dt, 
                           confusionMatrix(model,DATA[-folds[[i]], ]$class)$overall[[1]])
        f1.dt <- c(f1.dt, 
                           mean(na.omit(confusionMatrix(model,DATA[-folds[[i]], ]$class)$byClass[1:40,7])))

    }
    accuracies.dt
    f1.dt
}

accuracies.dt<-c()
f1.dt <- c()
accuracies.dt<-k.folds(10)
f1.dt <- k.folds(10)
accuracies.dt
f1.dt
