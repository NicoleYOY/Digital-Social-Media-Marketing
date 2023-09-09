
# Read in dataset

setwd("/Users/Nicole/Desktop/dataset")
logistic = read.csv("logistic_predict.csv")

# Look at the structure of the dataset
str(logistic)

# summarize the outcome for the column "Email"
table(logistic$Email)

# Install and load caTools package. This package can split the given dataset into training set and testing set
# Install and load ROCR package for the ROCR graph

install.packages("caTools")
install.packages("ROCR")

library(caTools)
library(ROCR)

# Randomly split data

 set.seed(88)
 split = sample.split(logistic$Purchase, SplitRatio = 0.75)
 head(split)

# Create training and testing sets

 logisticTrain = subset(logistic, split == TRUE) #training set

 logisticTest = subset(logistic, split == FALSE) #testing set

 head(logisticTrain)
 head(logisticTest)

# Logistic Regression Model
 
 logisticModel = glm(Purchase ~ Email + Display + Search, data=logisticTrain, family=binomial)
 
 summary(logisticModel)

# Logistic Regression Model with interaction terms
 
  logisticModel2= glm(Purchase ~ Email + Display + Search+Email*Display+Email*Search+Display*Search+Display*Search*Email, data=logisticTrain, family=binomial)
  summary(logisticModel2)


# Generate the estimated probability of purchasing for each consumer in the testing set.


 predictTest = predict(logisticModel2, type = "response", newdata = logisticTest)

 table(logisticTest$Purchase, predictTest >= 0.05)#confusion matrix when the threshold is 0.05

 table(logisticTest$Purchase, predictTest >= 0.04) #confusion matrix when the threshold is 0.04

# generate Prediction function for ROC plot

  ROCRpred = prediction(predictTest, logisticTest$Purchase)

# Performance function

  ROCRperf = performance(ROCRpred, "tpr", "fpr")

# Plot ROC curve
  plot(ROCRperf, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.01), text.adj=c(-0.2,1.7))




