if (!require("pacman")) install.packages("pacman")
pacman::p_load(xlsx, glmnet, MASS, jtools, huxtable, ggplot2, 
               ggthemes, gridExtra, ROCR, broom, caret, e1071,
               kknn, tidyr, dplyr,reshape2, glmnet)
options("jtools-digits" = 2, scipen = 999)
#Loading Input files
spam_data <- read.xlsx("spambase.xlsx", sheetName = "spambase_data")
print(spam_data)
spam_data$Spam <- as.factor(spam_data$Spam) 
spam_data$Spam
tecator= read.xlsx("tecator.xlsx", sheetName = "data")
tecator= tecator[,2:NCOL(tecator)] # removing sample column
## 1.1 Import the data into R 
set.seed(12345)
n =  NROW(spam_data)
id = sample(1:n, floor(n*0.5))
train = spam_data[id,]
test = spam_data[-id,]
##1.2 Use logistic regression (functions glm(), predict())
great_model <- glm(formula = Spam ~., family = binomial, data = train)
great_model
summary(great_model)
##  Prediction for probability greater than 50%  
train$prob <- predict(great_model, data = train, type = "response")
test$prob <- predict(great_model, data = test , type = "response")
train$prob & test$prob
train$prediction_group_50 <- ifelse(train$prob > 0.50, 1, 0)
test$prediction_group_50 <- ifelse(test$prob > 0.50, 1, 0)
train$prediction_group_50 & test$prediction_group_50 
##  confusion table
library(caret)
conf_train50 <- table(train$Spam, train$prediction_group_50)
names(dimnames(conf_train50)) <- c("accurate Train", "Predicted Train")
confusionMatrix(conf_train50)
conf_test50 <- table(test$Spam, test$prediction_group_50)
names(dimnames(conf_test)) <- c("accurate Test", "Predicted Test")
confusionMatrix(conf_test50)
## 1.3 Use logistic regression to classify the test data by the classification principle probability>90%

train$prediction_group_90 <- ifelse(train$prob > 0.90, 1, 0)
test$prediction_group_90 <- ifelse(test$prob > 0.90, 1, 0)
#confusion table
conf_train90 <- table(train$Spam, train$prediction_group_90)
names(dimnames(conf_train1)) <- c("accurate Train", "Predicted Train")
confusionMatrix(conf_train90)
conf_test90 <- table(test$Spam, test$prediction_group_90)
names(dimnames(conf_test1)) <- c("accurate Train", "Predicted Test")
confusionMatrix(conf_train90)
#analaysis
## 1,4 Use standard classifier kknn() with K=30 from package kknn, report the the misclassification 
## rates for the training and test data and compare the results with step 2
library(kknn)
knn30 <- train.kknn(Spam ~., data = train, kmax = 30)
train$knn_prediction_class <- predict(knn30, train)
test$knn_prediction_class <- predict(knn30, test)
conf_train2 <- table(train$Spam, train$knn_prediction_class)
names(dimnames(conf_train2)) <- c("accurate test", "Predicted Train")
confusionMatrix(conf_train2)
conf_test2 <- table(test$Spam, test$knn_prediction_class)
names(dimnames(conf_test2)) <- c("accurate test", "Predicted Test")
confusionMatrix(conf_test2)

#1.5 Repeat step 4 for K=1 and compare the results with step 4. What effect does the decrease of K lead to and why?

knn1<- train.kknn(Spam ~., data = train, kmax = 1)
train$knnpred  <- predict(knn1, train)
test$knn_pred <- predict(knn1, test)
conf_train2 <- table(train$Spam, train$knn_pred)
names(dimnames(conf_train2)) <- c("accurate test", "Predicted Train")
confusionMatrix(conf_train2)
conf_test2 <- table(test$Spam, test$knn_pred)
names(dimnames(conf_test2)) <- c("accurate test", "Predicted Test")
confusionMatrix(conf_test2)



 