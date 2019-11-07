library(caret)
library(psych)
library(GPArotation)
library(magrittr)
library(e1071)
library(rpart)
library(class)
library(parallelDist)

data<-read.csv(file="C:/Users/shilp/Downloads/DatasetsAML/Datasets/Gear21.csv", header=TRUE, sep=",")

data<-data[ , apply(data, 2, var) != 0]

data$class <- factor(data$class)
dim(data)
str(data$class)
data1<-data[-118]

pcadata<-prcomp(data1,scale = TRUE, center = TRUE)
DATA<-cbind(pcadata$x[ ,1:30],data[118])
std_dev <- pcadata$sdev
pr_var <- std_dev^2

prop_varex <- pr_var/sum(pr_var)
prop_varex[1:30]
variability<-sum(prop_varex[1:30])
variability

set.seed(567)

#Decision Tree
k.folds <- function(k) {
    folds <- createFolds(DATA$class, k = k, list = TRUE, returnTrain = TRUE)
    for (i in 1:k) {
    
    control <- rpart.control(minsplit = 2,
    minbucket = round(2/ 3),
    maxdepth = 30,
    cp = 0)
    model <- rpart(class~.,data=DATA[folds[[i]],],method = "class",control = control)
        
        predictions <- predict(object = model, newdata =(DATA[-folds[[i]],])[-31], type = "class")
        accuracies.dt <- c(accuracies.dt, 
                           confusionMatrix(predictions, DATA[-folds[[i]], ]$class)$overall[[1]])
        f1.dt <- c(f1.dt, 
                           mean(na.omit(confusionMatrix(predictions, DATA[-folds[[i]], ]$class)$byClass[1:40,7])))

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
  
    model <- ksvm(class~.,data=DATA[folds[[i]],],kernel="rbf",C =100,gamma=0.0001,nu = 0.2, epsilon = 0.1, prob.model = FALSE,scale = TRUE)
        
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
DATA[,1:30]<- normalize(DATA[,1:30])

set.seed(666)
k.folds <- function(k) {
    folds <- createFolds(DATA$class,k = k,list = TRUE,returnTrain = TRUE)
    for (i in 1:k) {
  
    traindata<-(DATA[folds[[i]],])[-31]
    newdata<-(DATA[-folds[[i]],])[-31]
    

    model <- knn(traindata,newdata,DATA[folds[[i]],]$class,k =3,l = 0, prob = FALSE, use.all = TRUE)
        
       
        accuracies.dt <- c(accuracies.dt, 
                           confusionMatrix(model, DATA[-folds[[i]], ]$class)$overall[[1]])
        f1.dt <- c(f1.dt, 
                           mean(na.omit(confusionMatrix(model, DATA[-folds[[i]], ]$class)$byClass[1:40,7])))

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
