title: "Lab01 Machine Learning"
subtitle: "Machine Learning - 732A99"
Name: "Nikodimos Gezahegn (nikge687)"



## 1.1 Import the data into R
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
# Assignment 4
### 1

library(ggplot2)
#4.1
tecator <- read_excel("tecator.xlsx")
plot <- ggplot(tecator, aes(x=tecator$Protein, y=tecator$Moisture)) + geom_point()
plot
# 4.2
#The probabilistic model that describes Mi is: Moisture = B0 + B1Protein + B2Protein^2 + ... + BiProtein^i + error. 

#It is appropriate to use MSE in this case, because MSE is an unbiased estimator of the variance of the error term. Because MSE is the average of the squared value of all the error terms, and it is computed by dividing by the degrees of freedom, the MSE measures how well the model fits the data [@ostertagova2012modelling].
#ot


# 4.3
# Dividing data into 50%/50% train/test
n <- dim(tecator)[1]
set.seed(12345)
id <- sample(1:n, floor(n*0.5))
train <- tecator[id,]
test <- tecator[-id, ]
# M1
model_m1 <- lm(Moisture ~ Protein, data = train)
m1_predict_test <- predict(model_m1, test)
m1_predict_train <- predict(model_m1, train)
m1_residual_test <- (sum((test$Moisture - m1_predict_test)^2))/nrow(test)
m1_residual_train <- (sum((train$Moisture - m1_predict_train)^2))/nrow(train)
#M2
model_m2 <- lm(Moisture ~ Protein + I(Protein^2), data = train)
m2_predict_test <- predict(model_m2, test)
m2_predict_train <- predict(model_m2, train)
m2_residual_test <- (sum((test$Moisture - m2_predict_test)^2))/nrow(test)
m2_residual_train <- (sum((train$Moisture - m2_predict_train)^2))/nrow(train)
#M3
model_m3 <- lm(Moisture ~ Protein + I(Protein^2) + I(Protein^3), data = train)
m3_predict_test <- predict(model_m3, test)
m3_predict_train <- predict(model_m3, train)
m3_residual_test <- (sum((test$Moisture - m3_predict_test)^2))/nrow(test)
m3_residual_train <- (sum((train$Moisture - m3_predict_train)^2))/nrow(train)
#M4
model_m4 <- lm(Moisture ~ Protein + I(Protein^2) + I(Protein^3) + I(Protein^4), data = train)
m4_predict_test <- predict(model_m4, test)
m4_predict_train <- predict(model_m4, train)
m4_residual_test <- (sum((test$Moisture - m4_predict_test)^2))/nrow(test)
m4_residual_train <- (sum((train$Moisture - m4_predict_train)^2))/nrow(train)
#M5
model_m5 <- lm(Moisture ~ Protein + I(Protein^2) + I(Protein^3) + I(Protein^4)
               + I(Protein^5), data = train)
m5_predict_test <- predict(model_m5, test)
m5_predict_train <- predict(model_m5, train)
m5_residual_test <- (sum((test$Moisture - m5_predict_test)^2))/nrow(test)
m5_residual_train <- (sum((train$Moisture - m5_predict_train)^2))/nrow(train)
#M6
model_m6 <- lm(Moisture ~ Protein + I(Protein^2) + I(Protein^3) + I(Protein^4)
               + I(Protein^5) + I(Protein^6), data = train)
m6_predict_test <- predict(model_m6, test)
m6_predict_train <- predict(model_m6, train)
m6_residual_test <- (sum((test$Moisture - m6_predict_test)^2))/nrow(test)
m6_residual_train <- (sum((train$Moisture - m6_predict_train)^2))/nrow(train)
# Create dataframe of MSE outcomes
mse_matrix <- matrix(NA, ncol = 3, nrow = 6)
mse_matrix
colnames(mse_matrix) <- c("train", "test", "i")
model_i <- c(1:6)
mse_matrix[,3] <- model_i
mse_matrix[1,1] <- m1_residual_train
mse_matrix[1,2] <- m1_residual_test
mse_matrix[2,1] <- m2_residual_train
mse_matrix[2,2] <- m2_residual_test
mse_matrix[3,1] <- m3_residual_train
mse_matrix[3,2] <- m3_residual_test
mse_matrix[4,1] <- m4_residual_train
mse_matrix[4,2] <- m4_residual_test
mse_matrix[5,1] <- m5_residual_train
mse_matrix[5,2] <- m5_residual_test
mse_matrix[6,1] <- m6_residual_train
mse_matrix[6,2] <- m6_residual_test
mse_df <- as.data.frame(mse_matrix)
mse_df
# Plot MSE's of different models on training and test data
plot_mse <- ggplot(data = mse_df, aes(x=i, y=train)) + geom_line(color = 'darkblue') + geom_line(aes(x=i, y=test))
plot_mse <- plot_mse + ylab("residual")
plot_mse 

#4.4

library("MASS")
# Subsetting dataframe for the model
df_4 <- tecator
class(tecator)
tecator_q4 <- tecator[, 2:102]
full_model <- lm(Fat ~ ., data = tecator_q4)
summary(full_model)
stepwise <- stepAIC(full_model, direction = "both", trace = FALSE)

# The intercept does not count as a variable, therefore subtract 1
number_of_variables <- length(stepwise$coefficients) - 1
number_of_variables
# 4.5 ####
# Installing and importing packages
library(glmnet)
# Preparing data
dim(tecator_q4)
covariates <- tecator_q4[, 1:100]
response <- tecator_q4[, 101]
dim(response)
# 4.5  
model_ridge <- glmnet(as.matrix(covariates), as.matrix(response), alpha = 0, family = "gaussian")
plot(model_ridge, xvar = "lambda", label = TRUE)


#All coefficients converge to zero, as log(lambda) increases.

### 6

model_lasso <- glmnet(as.matrix(covariates), as.matrix(response), alpha = 1, family = "gaussian")
plot(model_lasso, xvar = "lambda", label = TRUE)


#Comparing results from in the LASSO regression, less variables are selected. As log Lambda increases the coefficients in both models converge to zero. However, in the LASSO regression, the coefficients converge much faster as log(lambda) increases.
# Use cv.glmnet function, set alpha = 1, as a LASSO model is implemented
#7
model_cv <- cv.glmnet(as.matrix(covariates), as.matrix(response), alpha = 1, family = "gaussian",
                      lambda = seq(0,1,0.001))
model_cv$lambda.min
plot(model_cv)
coef(model_cv, s="lambda.min")
#The optimal lambda is 0. Excluding the intercept, a total of 100 independent variables were selected. In this dataset, this means that all the independent variables were incorporated in the model.

### 8

#The model implemented in question 4, by means of stepAIC, selects a total of 63 independent variables, whereas the model in question 7 selects 100 independent variables.
