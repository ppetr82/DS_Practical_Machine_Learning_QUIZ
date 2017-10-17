#Prepare the environment
library(caret)
library(rpart)
library(rattle)		
library(rpart.plot)
library(corrplot)

setwd("/Users/petrpodrouzek/Documents/coursera/PracticalML/QUIZ/")
dfTrain <- read.csv("pml-training.csv", stringsAsFactors = FALSE)
dfTest <- read.csv("pml-testing.csv")

#take only accelerometer measurements
#columnsToKeep <- grepl("^accel|^total_accel", names(dfTrain))
columnsToKeep <- grepl("^accel", names(dfTrain))

#take also the last column "classe"
columnsToKeep[length(columnsToKeep)] = TRUE
dfTrain <- dfTrain[,columnsToKeep]
columnsToKeep[length(columnsToKeep)] = FALSE
dfPredict <- dfTest[,columnsToKeep]

inTrain<-createDataPartition(y=dfTrain$classe,p=0.75, list=FALSE)
dfTest<-dfTrain[-inTrain,]
dfTrain<-dfTrain[inTrain,]

set.seed(1234)

modelCART <- rpart(classe ~ ., data = dfTrain)
predictCART <- predict(modelCART, dfTest, type="class")
confusionMatrix(dfTest$classe,predictCART)


#testing on correlation
correlationMatrix <- cor(dfTrain[,1:12])
corrplot(correlationMatrix)
#"classe"n# summarize the correlation matrix
print(correlationMatrix)
# find attributes that are highly corrected (ideally >0.75)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.5)
# print indexes of highly correlated attributes
print(highlyCorrelated)

modelSVM <- train(classe ~ ., data = dfTrain, method = "svm", verbose = FALSE)
modelRF <- train(classe ~ ., data = dfTrain, method = "rf", verbose = FALSE)
modelGBM <- train(classe ~ ., data = dfTrain, method = "gbm", verbose = FALSE)
modelLDA <- train(classe ~ ., data = dfTrain, method = "lda", verbose = FALSE)