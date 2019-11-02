rm(list = ls())
lib <- c("dplyr", "glmnet", "mice")
lapply(lib, require, character.only = TRUE)

Data <- read.csv("F:\\JRF\\Coursework\\Regression Techniques\\train.csv")

Data <- Data %>% 
  select(-one_of("PassengerId", "Name", "Ticket", "Cabin"))
Data$Survived <- as.factor(Data$Survived)
Data$Pclass <- as.factor(Data$Pclass)
Data$Embarked[Data$Embarked == ""] <- NA
sapply(Data, function(x) sum(is.na(x)))
#sapply(Train_Data, function(x) sum(x == ""))
dim(Data)

# The Complete Case Analysis has been done to check the accuracy if we do the simplest possible analysis. It can always be improved upon (by imputation) to get better results. One such method (MICE Imputation) has been done. It gives an accuracy of around 80%

# Complete Case Analysis 
Data_complete <- Data[complete.cases(Data),]
set.seed(3)
n <- nrow(Data_complete)
Train_rows <- sample(1:n, round(0.67 * n))
Train_Data <- Data_complete[Train_rows,]
Test_Data <- Data_complete[-Train_rows,]

# Logistic Regression
logit_fit <- glm(Survived ~., data = Train_Data, family = binomial)
summary(logit_fit)
pred_prob <- predict(logit_fit, type = "response")
pred_response <- I(pred_prob > 0.5)
conf_matrix <- table(pred_response, Train_Data$Survived);conf_matrix
Train_Accuracy <- (conf_matrix[1,1] + conf_matrix[2,2])/sum(conf_matrix);Train_Accuracy

Test_pred <- I(predict(logit_fit, newdata = Test_Data, type = "response") > 0.5)
Test_conf_matrix <- table(Test_pred, Test_Data$Survived); Test_conf_matrix
Test_Accuracy <- (Test_conf_matrix[1,1] + Test_conf_matrix[2,2])/sum(Test_conf_matrix); Test_Accuracy

# Probit Regression
probit_fit <- glm(Survived ~., data = Train_Data, family = binomial(link = "probit"))
summary(probit_fit)
pred_prob <- predict(probit_fit, type = "response")
pred_response <- I(pred_prob > 0.5)
conf_matrix <- table(pred_response, Train_Data$Survived);conf_matrix
Train_Accuracy <- (conf_matrix[1,1] + conf_matrix[2,2])/sum(conf_matrix);Train_Accuracy

Test_pred <- I(predict(logit_fit, newdata = Test_Data, type = "response") > 0.5)
Test_conf_matrix <- table(Test_pred, Test_Data$Survived); Test_conf_matrix
Test_Accuracy <- (Test_conf_matrix[1,1] + Test_conf_matrix[2,2])/sum(Test_conf_matrix); Test_Accuracy

# Elastic Net
Dummy_pred <- model.matrix(~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data = Data_complete)[, - 1]
Dummy_pred_train <- Dummy_pred[Train_rows,]
Dummy_pred_test <- Dummy_pred[-Train_rows,]
y_train <- Data_complete$Survived[Train_rows]
y_test <- Data_complete$Survived[-Train_rows]

elnet_fit <- glmnet(x = Dummy_pred_train, y = y_train, alpha = 0.5, family = "binomial")
cv_fit <- cv.glmnet(x = Dummy_pred_train, y = y_train, alpha = 0.5, family = "binomial", nfolds = 10)
coef(elnet_fit, cv_fit$lambda.min)

train_prediction <- I(predict(elnet_fit, s= cv_fit$lambda.min, newx = Dummy_pred_train, type = "response") > 0.5)
elnet_train_cm <- table(train_prediction, y_train); elnet_train_cm
elnet_train_acc <- (elnet_train_cm[1,1] + elnet_train_cm[2,2])/sum(elnet_train_cm);elnet_train_acc

test_prediction <- I(predict(elnet_fit, s = cv_fit$lambda.min, newx = Dummy_pred_test, type = "response")> 0.5)
elnet_test_cm <- table(test_prediction, y_test);elnet_test_cm
elnet_test_acc <- (elnet_test_cm[1,1] + elnet_test_cm[2,2])/sum(elnet_test_cm);elnet_test_acc

# MICE imputation and Logistic Regression
set.seed(5)
n <- nrow(Data)
Train_rows <- sample(1:n, round(0.67 * n))
Train_Data <- Data[Train_rows,]
Test_Data <- Data[-Train_rows,]

MICE_Impute <- mice(Train_Data, m = 100)
fit <- with(data = MICE_Impute, exp = glm(Survived ~ Pclass + Sex + Age +SibSp + Parch + Fare + Embarked, family = binomial))
Pred_prob <- lapply(fit$analyses, function(x) {predict(x, type = "response")})
Pooled_prob <- Reduce("+", Pred_prob)/length(Pred_prob)
Pred <- ifelse(Pooled_prob > 0.5 ,1, 0)
conf_matrix <- table(Pred, Train_Data$Survived)
Accuracy <- (conf_matrix[1,1] + conf_matrix[2,2])/sum(conf_matrix);Accuracy

Test_Data_Impute <- mice(Test_Data, m = 100) # MICE Imputation to get rid of NA's in Test Data
Pooled_Test_prob <- list()
for(i in 1:Test_Data_Impute$m)
{
  print(i)
  #i = 1
  Test_pred_prob <- lapply(fit$analyses, function(x) {predict(x, newdata = complete(Test_Data_Impute, i), type = "response")})
  Pooled_Test_prob[[i]] <- Reduce("+", Test_pred_prob)/length(Test_pred_prob)
}
Final_Test_prob <- Reduce("+", Pooled_Test_prob)/length(Test_pred_prob)
Test_Pred <- ifelse(Final_Test_prob > 0.5 ,1, 0)
Test_conf_matrix <- table(Test_Pred, Test_Data$Survived)
Test_Accuracy <- (Test_conf_matrix[1,1] + Test_conf_matrix[2,2])/sum(Test_conf_matrix); Test_Accuracy



