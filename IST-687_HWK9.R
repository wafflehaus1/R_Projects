#HWK9 ####

#Package Installation and Verification ####
install.packages("kernlab")
install.packages("e1071")
install.packages("ggplot2")
install.packages("gridExtra")
install.packages("bindr")
install.packages("caret")
install.packages("arulesViz")
install.packages("caTools")

library(kernlab)
library(e1071)
library(ggplot2)
library(gridExtra)
library(bindr)
library(caret)
library(arulesViz)
library(caTools)

#specify the packages of interest
packages=c("kernlab","e1071","gridExtra","ggplot2", "bindr", "caret", "arulesViz")

#use this function to check if each package is on the local machine
#if a package is installed, it will be loaded
#if any are not, the missing package(s) will be installed and loaded
package.check <- lapply(packages, FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
    library(x, character.only = TRUE)
  }
})
search()

#Step 1: Load the data ####
  #Let's go back and analyze the air quality dataset (if you remember, we used that previously, in the visualization lab). Remember to think about how to deal with the NAs in the data.
air <- data.frame(airquality) 
colnames(air)[colSums(is.na(air)) > 0] 
air$Ozone[is.na(air$Ozone)] <- mean(air$Ozone, na.rm=TRUE) 
air$Solar.R[is.na(air$Solar.R)] <- mean(air$Solar.R, na.rm=TRUE)
colnames(air)[colSums(is.na(air)) > 0] #verify NAs in Ozone & Solar.R columns were replaced
str(air)
View(air)

#Step 2: Create train and test data sets ####
  #Using techniques discussed in class, create two datasets -- one for training and one for testing.
set.seed(123)
split <- sample.split(air, SplitRatio = 0.67)
trainData <- subset(air, split == TRUE)
testData <- subset(air, split == FALSE)
nrow(trainData)
nrow(testData)

#Step 3: Build a Model using KVSM & visualize the results ####
  #1) Build a model (using the 'kvsm' function, trying to predict ozone). You can use all the possible attributes, or select the attributes that you think would be the most helpful.
ksvmModel <- ksvm(Ozone~., data = trainData, kernel = "rbfdot", kpar = "automatic", C = 10, cross = 10, prob.model = TRUE)
ksvmModel

  #2) Test the model on the testing dataset, and compute the Root Mean Squared Error
ksvmPred <- predict(ksvmModel, testData, type = "votes")
str(ksvmPred)

compTable <- data.frame(testData[,1], ksvmPred[,1])
colnames(compTable) <- c("test","Pred")
sqrt(mean((compTable$test-compTable$Pred)^2))

  #3) Plot the results. Use a scatter plot. Have the x-axis represent temperature, the y-axis represent wind, the point size and color represent the error, as defined by the actual ozone level minus the predicted ozone level).
compTable$error <- abs(compTable$test - compTable$Pred)
ksvmPlot <- data.frame(compTable$error, testData$Temp, testData$Wind, testData$Ozone)
colnames(ksvmPlot) <- c("error","Temp","Wind", "Ozone")
plot.ksvm <- ggplot(ksvmPlot, aes(x=Temp,y=Wind)) + geom_point(aes(size=error, color=error)) + ggtitle("KSVM Plot")
plot.ksvm

  #4) Compute models and plot the results for 'svm' (in the e1071 package) and 'lm'. Generate similar charts for each model.
#SVM Model------------------------
svmModel <- svm(Ozone~., data=trainData, kernel="radial", C=10,cross=10, prob.model=TRUE)
svmModel

svmPred <- predict(svmModel, testData, type = "votes")
str(svmPred)

compTable2 <- data.frame(testData[,1], svmPred)
colnames(compTable2) <- c("test","Pred")
sqrt(mean((compTable2$test-compTable2$Pred)^2))

compTable2$error <- abs(compTable2$test - compTable2$Pred)
svmPlot <- data.frame(compTable2$error, testData$Temp, testData$Wind, testData$Ozone)
colnames(svmPlot) <- c("error","Temp","Wind", "Ozone")
plot.svm <- ggplot(svmPlot, aes(x=Temp,y=Wind)) + geom_point(aes(size=error, color=error)) + ggtitle("SVM Plot")
plot.svm

#Linear Model-----------------------
lmModel <- lm(Ozone ~., data=trainData)
summary(lmModel)
lmPred <- predict(lmModel,testData)

str(lmPred)
compTable3 <- data.frame(testData[,1],lmPred)
colnames(compTable3) <- c("test","Pred")
sqrt(mean((compTable3$test-compTable3$Pred)^2))

compTable3$error <- abs(compTable3$test - compTable3$Pred)
lmPlot <- data.frame(compTable3$error, testData$Temp, testData$Wind)
colnames(lmPlot) <- c("error","Temp","Wind")
plot.lm <- ggplot(lmPlot, aes(x=Temp,y=Wind)) + geom_point(aes(size=error, color=error)) + ggtitle("Linear Model Plot")
plot.lm

  #5) Show all three results (charts) in one windows, using the grid.arrange function. 
grid.arrange(plot.ksvm,plot.svm,plot.lm, ncol=2, nrow=2, top="Comparing Three Models")

#Step 4: Create a 'goodOzone' variable ####
  #This variable should be either 0 or 1. It should be 0 if the ozone is below the average for all the data obersvations, and 1 if it is equal to or above the average ozone observed.
meanOzone <- mean(air$Ozone,na.rm=TRUE)
trainData$goodOzone <- ifelse(trainData$Ozone<meanOzone, "low", "high")
testData$goodOzone <- ifelse(testData$Ozone<meanOzone, "low", "high")
trainData <- trainData[,-1]
testData <- testData[,-1]


#Step 5: See if we can do a better job predecting 'good' and 'bad' days ####
trainData$goodOzone <- as.factor(trainData$goodOzone)
testData$goodOzone <- as.factor(testData$goodOzone)

  #1) Build a model (using the 'kvsm' fucntion, trying to predict 'goodOzone'). You can use all the possible attributes, or select the attributes that you think would be the most helpful.
ksvmGood <- ksvm(goodOzone~., data=trainData, kernel="rbfdot", kpar ="automatic", C=10, cross=10, prob.model=TRUE)
ksvmGood

  #2) Test the model on the testing dataset, and compute the percent of 'goodOzone' that was correctly predicted.
goodPred <- predict(ksvmGood, testData)
View(goodPred)

compGood <- data.frame(testData[,6], goodPred)
colnames(compGood) <- c("test","Pred")
ksvmperc <- length(which(compGood$test==compGood$Pred))/dim(compGood)[1]
ksvmperc

resultsKSVM <- table(test=compGood$test, pred=compGood$Pred)
print(resultsKSVM)

  #3) Plot the results. Use a scatter plot. Have the x-axis represent temperature, the y-axis represent wind, the shape representing what was predicted (good or bad day), the color representing the actual value of 'goodOzone' (i.e., if the actual ozone level was good) and the size represent if the prediction was correct (larger symbols should be the observations that the model got wrong).
compGood$correct <- ifelse(compGood$test==compGood$Pred,"correct","wrong")
ksvmPlot2 <- data.frame(compGood$correct,testData$Temp,testData$Wind,testData$goodOzone,compGood$Pred)
colnames(ksvmPlot2) <- c("correct","Temp","Wind","goodOzone","Predict")
plot.ksvm.good <- ggplot(ksvmPlot2, aes(x=Temp,y=Wind)) + geom_point(aes(size=correct,color=goodOzone,shape = Predict)) + ggtitle("KSVM - Good vs. Bad Ozone")
plot.ksvm.good

  #4) Compute models and plot the results for 'svm' (in the e1071 package) and 'nb' (Naive Bayes, also in the e1071 package).
#SVM Function
svmGood <- svm(goodOzone~.,data=trainData,kernel="radial",C=10,cross=10,prob.model=TRUE)
svmGood

goodPred2 <- predict(svmGood,testData)
compGood2 <- data.frame(testData[,6],goodPred2)
colnames(compGood2) <- c("test","Pred")
svmperc <- length(which(compGood2$test==compGood2$Pred))/dim(compGood2)[1]
svmperc

resultsSVM <- table(test=compGood2$test,pred=compGood2$Pred)
print(resultsSVM)

compGood2$correct <- ifelse(compGood2$test==compGood2$Pred,"correct","wrong")
plot.svm <- data.frame(compGood2$correct,testData$Temp,testData$Wind,testData$goodOzone,compGood2$Pred)
colnames(plot.svm) <- c("correct","Temp","Wind","goodOzone","Predict")
plot.svm.good <- ggplot(plot.svm,aes(x=Temp,y=Wind)) + geom_point(aes(size=correct,color=goodOzone,shape=Predict)) + ggtitle("SVM -- Good vs. Bad Ozone")
plot.svm.good

  #Naive Bayes Function
nbGood <- naiveBayes(goodOzone~.,data=trainData)
nbGood

goodPred3 <- predict(nbGood,testData)
compGood3 <- data.frame(testData[,6],goodPred3)
colnames(compGood3) <- c("test","Pred")
nbperc <- length(which(compGood3$test==compGood3$Pred))/dim(compGood3)[1]
nbperc

resultsNB <- table(test=compGood3$test,pred=compGood3$Pred)
print(resultsNB)

compGood3$correct <- ifelse(compGood3$test==compGood3$Pred,"correct","wrong")
NaiveBayesPlot <- data.frame(compGood3$correct,testData$Temp,testData$Wind,testData$goodOzone,compGood3$Pred)
colnames(NaiveBayesPlot) <- c("correct","Temp","Wind","goodOzone","Predict")
plot.nb.good <- ggplot(NaiveBayesPlot,aes(x=Temp,y=Wind)) + geom_point(aes(size=correct,color=goodOzone,shape=Predict)) + ggtitle("Naive Bayesian--Good vs. Bad Ozone")
plot.nb.good


  #5) Show all three results (charts) in one window, using the grid.arrange function (have two charts in one row).
grid.arrange(plot.ksvm.good,plot.svm.good,plot.nb.good, ncol=2, nrow=2, top="Comparing Three Models")

#Step 6: Which are the best Models for this data? ####
  #Review what you have done and state which is the best and why.

#Looking at original Ozone value; the Root Mean Squared Error yielded the following per model:
#ksvm: 20.04376
#svm: 18.8184
#lm: 20.38583
#Based on these results, ksvm and lm were similar in results, but the svm model had a lower value, 
#making it the best model of the three created.

#--------------------------------------------------------------
#Looking at the "goodOzone" value created, the percent correct for each model was:
#ksvm: 0.66
#svm: 0.76
#naive bayesion: 0.74

#Based on these results, the svm model had the highest percentage correct with Naive Bayesian close 
#behind--ksvm yielded much lower percent correct. 

