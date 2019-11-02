Missing Data Imputation:
For Age- Imputaion by Conditional mean Conditioned on Sex alone
For Embarked- Im putaion by Mode for Fare between 70 and 90

Data Preprocessing:
converted each categorical variables to levels
division into train and test (80-20)

Model:
Logistic Model

Accuracy:
Around 80%


Note: Here is a useful link

https://www.r-bloggers.com/how-to-perform-a-logistic-regression-in-r/

Accuracies:

GLM:-
Train=0.813202247191011
Test=0.798882681564246

GAM:-
Train=0.813202247191011
Test=0.804469273743017

Model for GAM= Survived ~ s(Age, df = 3) + s(Fare, df = 3) + Pclass + Sex + SibSp + Parch + Embarked