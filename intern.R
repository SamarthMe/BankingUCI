#loading the required packages
library(ggplot2)
library(knitr)
library(cluster)
library(HSAUR)
library(fpc)
library(lattice)
library(rpart)
library(kernlab)
library(randomForest)
library(tidyverse)  
library(cluster)    
library(factoextra) 
library(gmodels)
library(rpart)
library(rpart.plot)
library(rattle)
library(caret)
library(C50)

#setting Working Directory
  setwd("C:/Users/USER/Downloads/bank-additional/bank-additional")
#cleaning the data set and creating new CSV
  bank_additional_full <- read.csv(file.choose(), sep=";", stringsAsFactors=FALSE)
  write.csv(bank_additional_full, "cleaned_bank_additional_full.csv")

#reading the data
  data <- read.csv(file.choose(), header = T)
  head(data)

#exploratory data analysis
  ggplot(data, aes(x = default, fill = default) + geom_bar()
  ggplot(data, aes(x = marital, fill = marital)) + geom_bar()
  ggplot(data, aes(x = education, fill = education)) + geom_bar()
  ggplot(data, aes(x = job, fill = job)) + geom_bar()

  

#density plot of the age
  ggplot(data, aes(x = age)) + geom_density()

#Creating Cross validated tables to visualize the frequency of each instance in terms of the result variable
  mytable <- xtabs(~y, data = data)
  mytable <- xtabs(~y+marital, data = data)
  mytable <- xtabs(~y+education, data = data)
  mytable <- xtabs(~y+job, data = data)
  mytable <- xtabs(~y+housing, data = data)
  CrossTable(data$default, data$y)

  
#calculating pearson's correlation between different dependent factors with the independent factor  
  cor(as.numeric(as.factor(data$y)), as.numeric(as.factor(data$job)))
  cor(as.numeric(as.factor(data$y)), as.numeric(as.factor(data$education)))
  cor(as.numeric(as.factor(data$y)), as.numeric(as.factor(data$marital)))
  cor(as.numeric(as.factor(data$y)), as.numeric(as.factor(data$loan)))
  cor(as.numeric(as.factor(data$y)), as.numeric(as.factor(data$housing)))
  cor(as.numeric(as.factor(data$y)), as.numeric(as.factor(data$default)))
  
#Converting non-numeric factors to numeric ones
BankAdditionalNum <- data.frame(as.numeric(as.factor(data$age)),
                                as.numeric(as.factor(data$job)),
                                as.numeric(as.factor(data$marital)),
                                as.numeric(as.factor(data$education)),
                                as.numeric(as.factor(data$housing)),
                                as.numeric(as.factor(data$loan)))

#Rename the columns
  colnames(BankAdditionalNum) <- c("Age", "Job", "Marital", "Education", "Housing", "Loan")

#Reduce the amount of dataset records for legibility within clusters
  BankAdditionalNum2 <- BankAdditionalNum[sample(nrow(BankAdditionalNum),500),]

#Kmeans clustering to create 5 clusters
  set.seed(12345)
  BankAdditionalNum_k5 <- kmeans(BankAdditionalNum2, centers=5)

#Visualising the cluster centers and the clusters and the size
  BankAdditionalNum_k5$size
  BankAdditionalNum_k5$centers
  hullplot(BankAdditionalNum2, BankAdditionalNum_k5$cluster)

#Forming the testing and training data set
  inTrain <- createDataPartition(y=data$y ,p=0.7,list=FALSE)
  training <- data[inTrain,]
  testing <- data[-inTrain,]

#Checking set dimensions  
  dim(training);dim(testing)
  table(training$y); table(testing$y)

#Simulating a model to predict the independent variable  
  dt_model<- rpart(y ~ ., data = training)
  summary(dt_model)
  predictions <- predict(dt_model, testing, type = "class")
  predict(dt_model, testing[1,-10])
  table(predictions)

  confusion.matrix <- prop.table(table(predictions, testing$y))
  confusion.matrix
  confusionMatrix(predictions,testing$y)

#Creating a radndom forest model to calculate efficiency
  model <- randomForest(y ~ ., data=training)
  model
  importance(model)

#Testing the model accuracy
  predicted <- predict(model, testing)
  table(predicted)
  confusionMatrix(predicted, testing$y)

accuracy=c()
for (i in seq(1,50, by=1)) {
  modFit <- randomForest(y ~ ., data=training, ntree=i)
  accuracy <- c(accuracy, confusionMatrix(predict(modFit, testing, type="class"), testing$y)$overall[1])
}
par(mfrow=c(1,1))
plot(x=seq(1,50, by=1), y=accuracy, type="l", col="green",
     main="Accuracy VS Tree-Size", xlab="Tree Size", ylab="Accuracy")

