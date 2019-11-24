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
#3.1 Import data to R and create a plot of Moisture versus Protein. Do you think that these data are described well by linear model.
tecator= read_xlsx("tecator.xlsx")
tecator= tecator[,2:NCOL(tecator)] # removing sample column 
ggplot(data = tecator, aes(x = Protein, y = Moisture)) + 
  geom_point() + 
  geom_smooth()
  ggtitle("Plot of Moisture vs. Protein")
 
#3.2 Analysis: In this case we are given that mositure is normally distributed, thus the loss function to minimize had to be (actual-predicted)^2, if we were to minimize the absoulte value, then this would assume a Laplace distribution.
  
#3.3
  final_data <- tecator_data
  magic_function <- function(df, N)
  {
    df2 <- df  
    for(i in 2:N) 
    {
      df2[paste("Protein_",i,"_power", sep="")] <- (df2$Protein)^i
    }
    df2 <- df2[c("Protein_2_power", "Protein_3_power", 
                 "Protein_4_power", "Protein_5_power", 
                 "Protein_6_power")]
    df <- cbind(df,df2)  
    return(df)
  }
  final_data <- magic_function(final_data, 6)
  set.seed(12345)
  n =  NROW(final_data)
  id = sample(1:n, floor(n*0.5))
  train = final_data[id,]
  test = final_data[-id,]
  # model building
  M_1 <- lm(data = train, Moisture~Protein)
  M_2 <- lm(data = train, Moisture~Protein+Protein_2_power)
  M_3 <- lm(data = train, Moisture~Protein+Protein_2_power+Protein_3_power)
  M_4 <- lm(data = train, Moisture~Protein+Protein_2_power+Protein_3_power+
              Protein_4_power)
  M_5 <- lm(data = train, Moisture~Protein+Protein_2_power+Protein_3_power+
              Protein_4_power+Protein_5_power)
  M_6 <- lm(data = train, Moisture~Protein+Protein_2_power+Protein_3_power+
              Protein_4_power+Protein_5_power+Protein_6_power)
  train$type <- "train"
  test$type <- "test"
  final_data <- rbind(test, train)
  # predicting new values
  M_1_predicted <- predict(M_1, newdata = final_data)
  M_2_predicted <- predict(M_2, newdata = final_data)
  M_3_predicted <- predict(M_3, newdata = final_data)
  M_4_predicted <- predict(M_4, newdata = final_data)
  M_5_predicted <- predict(M_5, newdata = final_data)
  M_6_predicted <- predict(M_6, newdata = final_data)
  # calculating the MSE
  final_data$M_1_error <- (final_data$Moisture - M_1_predicted)^2
  final_data$M_2_error <- (final_data$Moisture - M_2_predicted)^2
  final_data$M_3_error <- (final_data$Moisture - M_3_predicted)^2
  final_data$M_4_error <- (final_data$Moisture - M_4_predicted)^2
  final_data$M_5_error <- (final_data$Moisture - M_5_predicted)^2
  final_data$M_6_error <- (final_data$Moisture - M_6_predicted)^2
  # Chainning like Chainsaw
  final_error_data <- final_data %>% select(type, M_1_error, M_2_error, M_3_error, 
                                            M_4_error, M_5_error, M_6_error) %>% 
    gather(variable, value, -type) %>% 
    separate(variable, c("model", "power", "error"), "_") %>% 
    group_by(type, power) %>% 
    summarise(MSE = mean(value, na.rm=TRUE))
  ggplot(final_error_data, aes(x = power, y = MSE, color=type)) + geom_point() +
    ggtitle("Mean squared error vs. model complexitiy by dataset type")
 
  #Analysis: As evident from the plot above, we see that as we increase the model complexitiy (higher powers of the 'protein'), the trainning error reduces however the model becomes too biased towards the trainning set (overfits) and misses the test datasets prediction by larger margins in higher powers. 
  
  #The best model is M1, that is Moisture~Protein as evident from the least test error (MSE).
  
 # The above is a classical case of bias-varience trade-off, which is as follows, as one makes the model fit the trainning dataset better the model becomes more biased and its ability to handle variation to new dataset decreases(varience), thus one should also maintain a good trade off between these two.

## 3.4 Perform variable selection of a linear model in which Fat is response and Channel1:Channel100 are predicted by using stepAIC. Comment on how many variables were selected.
 
  min.model1 = lm(Fat ~ 1, data=tecator_data[,-1])
  biggest1 <- formula(lm(Fat ~.,  data=tecator_data[,-1]))
  step.model1 <- stepAIC(min.model1, direction ='forward', scope=biggest1, trace = FALSE)
  summ(step.model1)
  
  ## 3.5 Fit a Ridge regression model with the same predictor and response variables. Present a plot showing how model coefficients depend on the log of the penalty factor lambda and report how the coefficients change with lambda.
  
 
  y <- tecator_data %>% select(Fat) %>% data.matrix()
  x <- tecator_data %>% select(-c(Fat)) %>% data.matrix()
  lambda <- 10^seq(10, -2, length = 100)
  ridge_fit <- glmnet(x, y, alpha = 0, family = "gaussian", lambda = lambda)
  plot(ridge_fit, xvar = "lambda", label = TRUE, 
       main = "Plot showing shrinkage of coefficents with rise in log of lambda")
  
  result <- NULL
  for(i in lambda){
    temp <- t(coef(ridge_fit, i)) %>% as.matrix()
    temp <- cbind(temp, lambda = i)
    result <- rbind(temp, result)
  }
  result <- result %>% as.data.frame() %>% arrange(lambda)
 
  
   
  table_cofe <- head(result, 10) %>% select(Channel1, Channel2, Channel84, Channel62, 
                                            Channel53, Channel75, Channel57,Protein, 
                                            lambda)
  ## 3.6 Repeat step 5 but fit LASSO instead of the Ridge regression and compare the plots from steps 5 and 6. Conclusions? 
  
   
  lambda <- 10^seq(10, -2, length = 100)
  lasso_fit <- glmnet(x, y, alpha = 1, family = "gaussian", lambda = lambda)
  plot(lasso_fit, xvar = "lambda", label = TRUE, 
       main = "Plot showing shrinkage of coefficents with rise in log of lambda")
 

  ## 3.7 Use cross-validation to find the optimal LASSO model, report the optimal lambda and how many variables were chosen by the model and make conclusions. Present also a plot showing the dependence of the CV score and comment how the CV score changes with lambda.
  
  ```{r}
  #find the best lambda from our list via cross-validation
  lambda_lasso <- 10^seq(10, -2, length = 100)
  lambda_lasso[101] <- 0
  lasso_cv <- cv.glmnet(x,y, alpha=1, lambda = lambda_lasso, type.measure="mse")
  coef(lasso_cv, lambda = lasso_cv$lambda.min)
  lasso_cv$lambda.min
  
  ## Change of coefficent with respect to lambda
  result_lasso <- NULL
  for(i in 1:length(lambda_lasso)){
    temp <- lasso_cv$cvm[i] %>% as.matrix()
    temp <- cbind(CVM_error = temp, lambda = lasso_cv$lambda[i])
    result_lasso <- rbind(temp, result_lasso)
  }
  result_lasso <- result_lasso %>% as.data.frame() %>% arrange(lambda)
  colnames(result_lasso) <- c("Cross_Mean_Error", "Lambda")
  ggplot(result_lasso, aes(x = log(Lambda), y = Cross_Mean_Error)) + geom_point() + 
    ggtitle("Cross Validation Error vs. Lambda")
  ## 3.8 Compare the results from steps 4 and 7.
  
 # Analysis: In order to find the best predictors for a given model we employ various techniques.
  
  #In step4 we analytically arrive at the best variables to model 'Fat' using multiple variables, using stepAIC we arrive at 29 variables excluding the Intercept.
  
  #In step5 we use regularisation techniques and start introducing bias to eliminate the variables which maybe corellated with each other, employing this we get further reduction of about 10 variables at log lambda ~ 5.
  
  #Using Lasso in step6 we get only about 5 variables at lambda close to 1, however the exact number of variables to choose is depended on the lambda value and the corresponding error. However having used the whole dataset, we need to employee cross validation to get the precise value of lambda.
  
  #In step 7 we get the best value of lambda for lasso for which the mean squared error is the least and here we are left with 9 variables excluding the intercept. The mean squared error is also low (~10).
  
  #Thus we have learned how to perform regression and how to account for multicorrlinearity and possible ways to detect and negate the same.
  
