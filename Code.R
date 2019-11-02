data=read.csv('E:/ISI/Regression/titanic.csv')

data$SibSp=as.factor(data$SibSp)
data$Parch=as.factor(data$Parch)
data$Embarked=as.factor(data$Embarked)
data$Pclass=as.factor(data$Pclass)
data$Sex=as.factor(data$Sex)

smp_size <- floor(0.80 * nrow(data))

#set.seed(123)
f=function()
{
train_ind <- sample(seq_len(nrow(data)), size = smp_size)

train <- data[train_ind, ]
test <- data[-train_ind, ]

for(i in c(4,6))
{
ll=unique(train[,i])
for(j in seq(1,nrow(test),1))
{
if(!(test[j,i] %in% ll))
test[j,i]=0
}
} 

logitMod <- glm(Survived ~., family=binomial(link="logit"), data = train)

predictedY <- predict(logitMod, test, type="response")
fitted.results <- ifelse(predictedY > 0.5,1,0)
fitted_train=predict(logitMod, train, type="response")
fitted_train=ifelse(fitted_train > 0.5,1,0)

misClasificError <- mean(fitted.results != test$Survived)
mis_train=mean(fitted_train != train$Survived)
print(paste('Accuracy',1-misClasificError, 'Accuracy_Train',1-mis_train))
print(summary(logitMod))

gamMod=gam(Survived~s(Age,df=3)+s(Fare,df=3)+Pclass+Sex+SibSp+Parch+Embarked,family=binomial(link="logit"), data = train)
predicted_g <- predict(gamMod, test, type="response")
fitted.results_g <- ifelse(predicted_g > 0.5,1,0)
fitted_train_g=predict(gamMod, train, type="response")
fitted_train_g=ifelse(fitted_train_g > 0.5,1,0)

misClasificError_g <- mean(fitted.results_g != test$Survived)
mis_train_g=mean(fitted_train_g != train$Survived)
print(paste('Accuracy',1-misClasificError_g,'Accuracy_Train',1-mis_train_g))
print(summary(gamMod))
}

f()