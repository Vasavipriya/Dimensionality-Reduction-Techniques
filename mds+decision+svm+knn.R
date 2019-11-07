library(caret)
library(psych)
library(GPArotation)
library(magrittr)
library(e1071)
library(rpart)
library(class)
library(parallelDist)

data<-read.csv(file.choose(), header=TRUE, sep=",")

data<-data[ , apply(data, 2, var) != 0]

data$Class <- factor(data$Class)
dim(data)
str(data$Class)


data1<-data[-121]
x<-as.matrix(data1)
d<-parDist(x, method = "euclidean", threads = NULL,upper = TRUE,diag = TRUE)
fit <- cmdscale(d,eig=TRUE, k=20) # k is the number of dim

var<-sum(fit$eig[1:20])/sum(fit$eig)
var
indvar<-fit$eig[1:20]/sum(fit$eig)
indvar

z<-fit$points[,1:20]
MDSdata<-cbind(z,data[121])
set.seed(567)

#Decision Tree
k.folds <- function(k) {
    folds <- createFolds(MDSdata$Class, k = k, list = TRUE, returnTrain = TRUE)
    for (i in 1:k) {
    
    control <- rpart.control(minsplit = 2,
    minbucket = round(2/ 3),
    maxdepth = 30,
    cp = 0)
    model <- rpart(Class~.,data=MDSdata[folds[[i]],],method = "class",control = control)
        
        predictions <- predict(object = model, newdata =(MDSdata[-folds[[i]],])[-21], type = "class")
        accuracies.dt <- c(accuracies.dt, 
                           confusionMatrix(predictions,MDSdata[-folds[[i]], ]$Class)$overall[[1]])
        f1.dt <- c(f1.dt, 
                           mean(na.omit(confusionMatrix(predictions,MDSdata[-folds[[i]], ]$Class)$byClass[1:37,7])))

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
    folds <- createFolds(MDSdata$Class, k = k, list = TRUE, returnTrain = TRUE)
    for (i in 1:k) {
  
    model <- ksvm(Class~.,data=MDSdata[folds[[i]],],kernel="rbf",C =1000,gamma=0.0001,nu = 0.2, epsilon = 0.1, prob.model = FALSE,scale = TRUE)
        
        predictions <- predict(object = model, newdata = (MDSdata[-folds[[i]],])[-21])
        accuracies.dt <- c(accuracies.dt, 
                           confusionMatrix(predictions,MDSdata[-folds[[i]], ]$Class)$overall[[1]])
        f1.dt <- c(f1.dt, 
                           mean(na.omit(confusionMatrix(predictions,MDSdata[-folds[[i]], ]$Class)$byClass[1:36,7])))

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
MDSdata[,1:20]<- normalize(MDSdata[,1:20])

set.seed(666)
k.folds <- function(k) {
    folds <- createFolds(MDSdata$Class,k = k,list = TRUE,returnTrain = TRUE)
    for (i in 1:k) {
  
    traindata<-(MDSdata[folds[[i]],])[-21]
    newdata<-(MDSdata[-folds[[i]],])[-21]
    

    model <- knn(traindata,newdata,MDSdata[folds[[i]],]$Class,k =1,l = 0, prob = FALSE, use.all = TRUE)
        
       
        accuracies.dt <- c(accuracies.dt, 
                           confusionMatrix(model, MDSdata[-folds[[i]], ]$Class)$overall[[1]])
        f1.dt <- c(f1.dt, 
                           mean(na.omit(confusionMatrix(model, MDSdata[-folds[[i]], ]$Class)$byClass[1:36,7])))

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
