setwd("/Users/hsinhua/Desktop/Coursera/Practical Machine Learning/Prediction_Project")
# file1url <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
# file2url <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
# download.file(file1url, destfile = "training.csv")
# download.file(file2url, destfile = "testing.csv")

## First let's import the raw train and test data use read.csv 
rawdata_train = read.csv("~/Desktop/Coursera/Practical Machine Learning/Prediction_Project/training.csv",  na.strings = c("","NA", "NULL"))
rawdata_test = read.csv("~/Desktop/Coursera/Practical Machine Learning/Prediction_Project/testing.csv", na.strings = c("","NA", "NULL"))

## Check the dims of the data
dim(rawdata_train)
dim(rawdata_test)

## Let us import the packages that we will use in this project
library(caret)
library(dplyr)

napercent <- colSums(is.na(rawdata_train))/nrow(rawdata_train)
print(napercent)

process_train <- rawdata_train[,colSums(is.na(rawdata_train)) == 0]
dim(process_train)
names(process_train)

process2_train <- process_train[,8:length(process_train[1,])]
dim(process2_train)

col_nnzv <-  which(nearZeroVar(process2_train, saveMetrics = TRUE)$nzv == FALSE)
process3_train <- process2_train[,col_nnzv]

## let's find highly correlated variables and remove them
corrMatrix <- cor(process3_train[,sapply(process3_train, is.numeric)])
highcorrvb <- findCorrelation(corrMatrix, cutoff = .9, verbose = TRUE)

process4_train <- process3_train[,-highcorrvb]
dim(process4_train)

## Now we split the data into training and testing so that we can 
## check how good the model is before using it for prediction
inTrain <- createDataPartition(process4_train$classe, p = 3/4, list = FALSE)
training <- process4_train[inTrain,]
testing <- process4_train[-inTrain,]

## Now let's analyze the data using caret package
## rpart
modrpart <- train(classe ~., method = "rpart", data = training)
print(modrpart$finalModel)
plot(modrpart$finalModel, uniform = TRUE, main = "Classification Tree")
text(modrpart$finalModel, use.n = TRUE, all = FALSE, cex = 0.8)

##check accuracy 
predrpart <- predict(modrpart, testing)
table(predrpart, testing$classe)
confusionMatrix(testing$classe, predrpart)$overall['Accuracy']
## give pretty bad accuracy

##gbm ?
# modgbm<- train(classe ~., method = "gbm", data = training)
library(gbm)
modgbm <- gbm(classe ~., data = training, distribution = "multinomial", n.trees = 200, interaction.depth = 4, shrinkage = 0.005)
predgbm <- predict(modgbm, n.trees = 200, newdata= testing, type = 'response')
## The predict returns back the probability for each classe
## Below for each row we pick the one with largest probability
maxpredgbm <- apply(predgbm, 1, which.max)

## Since 1~5 means A ~ E, we rename them below
maxpredgbm[which(maxpredgbm == 1)] <- "A"
maxpredgbm[which(maxpredgbm == 2)] <- "B"
maxpredgbm[which(maxpredgbm == 3)] <- "C"
maxpredgbm[which(maxpredgbm == 4)] <- "D"
maxpredgbm[which(maxpredgbm == 5)] <- "E"
maxpredgbm <- as.factor(maxpredgbm)

# check the accuracy using confusionMatrix
confusionMatrix(testing$classe, maxpredgbm)$overall['Accuracy']



# random forest: The caret package is too too too too too slow
# modrf <- train(classe ~ ., method = "rf", data = process4_train, prox = TRUE)

library(randomForest)
modrf <- randomForest(classe~., data = training, ntree=100, importance=TRUE, prox = TRUE)

predrf <- predict(modrf, testing)
table(predrf, testing$classe)
confusionMatrix(testing$classe, predrf)$overall['Accuracy']

performance <- predict(modrf, rawdata_test)
print(performance)