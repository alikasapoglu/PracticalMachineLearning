
library(caret)
library(rpart) 
library(randomForest)

library(rpart.plot)
library(RColorBrewer)
library(rattle)
set.seed(142)


##download data
trainUrl <- "http://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
testUrl <- "http://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"

data_raw <- read.csv(url(trainUrl), na.strings=c("NA","#DIV/0!",""))
data_test <- read.csv(url(testUrl), na.strings=c("NA","#DIV/0!",""))

data <- data_raw
str(data)
summary(data)


#Clear data
## Remove unnecessary columns

tempcolnames <- grep("name|timestamp|window|X", colnames(data), value=F) 
data <- data[,-tempcolnames]
rm("tempcolnames")


## Remove columnss having too much NA
treshold <- nrow(data)*0.75
okCols <- names(data[,colSums(is.na(data)) < treshold] )
data <- data[,okCols]
rm(okCols)
dim(data)

## Remove zero variance variables
NZV <- nearZeroVar(data, saveMetrics=TRUE)
'there isnt any near zero data, so no cleaning needed for this '


set.seed(142)
totrain <- createDataPartition(y=data$classe,p=.70,list=F)
data_train <- data[totrain,]
data_mytest <- data[-totrain,]
rm(totrain)

##Decision Tree
modelDT <- rpart(data_train$classe ~ ., data=data_train, method="class")

predDT <- predict(modelDT, data_mytest, type = "class")
confusionMatrix(predDT, data_mytest$classe)
confusionMatrix(predDT, data_mytest$classe)$overall[1]

## Random Forest
modelRF <- randomForest(data_train$classe ~ .,   data=data_train, do.trace=F)
print(modelRF) # view results 

predRF <- predict(modelRF, data_mytest, type = "class")
confusionMatrix(predRF, data_mytest$classe)
confusionMatrix(predRF, data_mytest$classe)$overall[1]


pred_test <- predict(modelRF, newdata = data_test, type="class")
print(pred_test)






dim(data)
dim(data_train)
dim(testing)


