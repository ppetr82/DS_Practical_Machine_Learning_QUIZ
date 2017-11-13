#Prepare the environment
#=======================
#include libraries required
library(caret)
library(rpart)
#library(rattle)		
library(rpart.plot)
library(corrplot)
library(plyr)
#set up seed and WD
setwd("/Users/petrpodrouzek/Documents/coursera/PracticalML/QUIZ/")
set.seed(1234)

#Load datasets
#=============
dfTrain <- read.csv("pml-training.csv", stringsAsFactors = FALSE, na.strings=c("",NA))
#most of the columns are numeric but contains empty strings -> needed to convert those)
dfTrain[,8:159] <- sapply(dfTrain[,8:159],as.numeric)
#load the data to predict on
dfPredict <- read.csv("pml-testing.csv")

#Investigate the features
#========================
#it seems that if new_window = TRUE then aggregative measures are present
#measures like min, max, skeweness etc.; I will firstly try to train the model
#on the basic features for which we always have all the functions
featureDF<-stack(sapply(dfTrain[1:160], function(y) sum(is.na(y))))
colnames(featureDF)[1] <- "Count"
colnames(featureDF)[2] <- "Feature"
featureDF$ToKeep<-featureDF$Count == 0
#the first seven columns not to be used - these are dimensions not facts
featureDF[1:7,]$ToKeep <- FALSE
#this is just a helper vector
factualFeaturesToKeep<-featureDF$ToKeep
factualFeaturesToKeep[160]<-FALSE
#calculate correlations
correlationMatrix <- cor(dfTrain[,factualFeaturesToKeep], use = "pairwise.complete.obs")
#corrplot(correlationMatrix)
print(correlationMatrix)
#find attributes that are highly corrected (ideally >0.75)
highlyCorrelated <- findCorrelation(na.omit(correlationMatrix), cutoff=0.75, names = TRUE)
#print indexes of highly correlated attributes
print(highlyCorrelated)
corrplot(correlationMatrix)
#remove hihgly correlated features from the feature list
featureDF[featureDF$Feature %in% highlyCorrelated,]$ToKeep<-FALSE
factualFeaturesToKeep<-featureDF$ToKeep
factualFeaturesToKeep[160]<-FALSE
correlationMatrix2 <- cor(dfTrain[,factualFeaturesToKeep], use = "pairwise.complete.obs")
print(correlationMatrix2)
corrplot(correlationMatrix2)

#Model training and prediction
#==============================
#divide into training and test datasets
inTrain<-createDataPartition(y=dfTrain$classe,p=0.75, list=FALSE)
dfTest<-dfTrain[-inTrain,]
dfTrain<-dfTrain[inTrain,]

#decision tree - low accuracy, not working; accuracy is only 0.64
modelCART <- rpart(classe ~ ., data = dfTrain[,featureDF$ToKeep])
predictCART <- predict(modelCART, dfTest[,featureDF$ToKeep], type="class")
confusionMatrix(dfTest$classe,predictCART)

#random forrest - better as accuracy is about 0.98
modelRF <- train(classe ~ ., data = dfTrain[,featureDF$ToKeep], method = "rf", verbose = FALSE, ntree = 10)
predictRF <- predict(modelRF, dfTest[,featureDF$ToKeep], type="raw")
confusionMatrix(dfTest$classe,predictRF)

#review importance of various features
importance <- varImp(modelRF, scale=FALSE)
print(importance)
plot(importance)


