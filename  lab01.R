if (!require("pacman")) install.packages("pacman")
pacman::p_load(xlsx, glmnet, MASS, jtools, huxtable, ggplot2, 
               ggthemes, gridExtra, ROCR, broom, caret, e1071,
               kknn, tidyr, dplyr,reshape2, glmnet)
options("jtools-digits" = 2, scipen = 999)
#Loading Input files
library(xlsx)
spam_data <- read_xlsx("spambase.xlsx", sheetName = "spambase_data")
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
train$prob <- predict(great_model, data = train)
test$prob <- predict(great_model, data = test)
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
conf_test50
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



 
# Assignment 2 Feature selection by cross-validation in a linear model

## 2.1 Implement an R function that performs feature selection (best subset selection)
  
subset_select <- function(X,Y,N) {
   
  df <- cbind(X,Y)
  subs<- c()
  final <-c()
  
  for(i in 1:ncol(X)){
    combs <- as.data.frame(gtools::combinations(ncol(X), r=i, v=colnames(X)))
    combs <- tidyr::unite(combs, "formula", sep = ",")
    subs <- rbind(combs,subs)
  }  
  
  set.seed(12345)
  df2 <- df[sample(nrow(df)),]
  df2$k_fold <- sample(N, size = nrow(df), replace = TRUE)
  
  result <- NULL 
  for (j in 1:NROW(subs))
  {
    for(i in 1:N){
      train = df2[df2$k_fold != i,]
      test = df2[df2$k_fold == i,]
      
      vec <- subs[j,]
      train_trimmed = lapply(strsplit(as.character(vec), ","), function(x) train[x])[[1]]
      test_trimmed = lapply(strsplit(as.character(vec), ","), function(x) test[x])[[1]]
      y_train = train[,c("Y")]
      y_test = test[,c("Y")]
      train_trimmed = as.matrix(train_trimmed)
      test_trimmed = as.matrix(test_trimmed)
      y_test = as.matrix(y_test) 
      y_train = as.matrix(y_train) 
      t_train =  as.matrix(t(train_trimmed)) 
      t_test =  as.matrix(t(test_trimmed)) 
      betas = solve(t_train %*% train_trimmed) %*% (t_train %*% y_train)
      train_trimmed = as.data.frame(train_trimmed)
      test_trimmed = as.data.frame(test_trimmed)
      train_trimmed$type = "train"
      test_trimmed$type = "test"
      final <- rbind(train_trimmed, test_trimmed)
      y_hat_val = as.matrix(final[,1:(ncol(final)-1)]) %*% betas
      mse = (Y - y_hat_val)^ 2
      data <- cbind(i, vec, mse, type = final$type)
      result <- rbind(data, result)
      
    }
  }
  result <- as.data.frame(result)
  colnames(result) <- c("kfold", "variables", "mse", "type")
  result$mse <- as.numeric(result$mse)
  result$no_variables <- nchar(as.character(result$variables)) - nchar(gsub('\\,', "", result$variables)) + 1
  result_test <- result %>% filter(type == "test") 
  variable_performance <- result_test %>% group_by(kfold, no_variables) %>% 
    summarise(MSE = mean(mse, na.rm = TRUE))
  myplot <- ggplot(data = variable_performance, aes(x = no_variables, y = MSE, color=kfold)) + 
    geom_line() + ggtitle("Plot of MSE(test) vs. Number of variables")
  myplot2 <- ggplot(data = result_test, aes(x = variables, y = mse, color=kfold)) + 
    geom_bar(stat="identity") + ggtitle("Plot of MSE(test) vs. Features by folds") + coord_flip() 
  best_variables <- result_test %>% group_by(variables) %>% 
    summarise(MSE = mean(mse)) %>% arrange(MSE) %>% 
    select(variables) %>% slice(1) %>% as.vector()
  return(list(myplot,  best_variables))
}
 


## 2.2 Test your function on data set swiss available in the standard R repository:
 
subset_select(X = swiss[,2:6], Y = swiss[,1:1], N = 5)





 


 


