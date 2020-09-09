# Support Vector Machine(SVM)

# Prepare a classification model using SVM for salary data 
library(kernlab)
library(ggplot2)
library(dplyr)

#Lets Import the Data
#The data here in already partitioned so we are loading it seperately
train <- read.csv("E:/Data science Excelr/Assigments/svm/Salary/Salary_Train.csv") #This data is used to train the Model
attach(train)

test <- read.csv("E:/Data science Excelr/Assigments/svm/Salary/Salary_Test.csv") #This data is used to Test the Model & predict on it
attach(test)

#Lets us First Analyze the train Data
summary(train)
str(train)
names(train)
#So, Here their are many variables that we need to convert into an Categorical Variable. 
#

train$workclass <- as.factor(train$workclass)
train$education <-as.factor(train$education)
train$maritalstatus <- as.factor(train$maritalstatus)
train$occupation <- as.factor(train$occupation)
train$relationship <- as.factor(train$relationship)
train$race <- as.factor(train$race)
train$sex <- as.factor(train$sex)
train$native <- as.factor(train$native)
train$Salary <- as.factor(train$Salary)

summary(test)
str(test)
names(test)
#Lets Apply the same Transformation to the salary_test Data
test$workclass <- as.factor(test$workclass)
test$education <-as.factor(test$education)
test$maritalstatus <- as.factor(test$maritalstatus)
test$occupation <- as.factor(test$occupation)
test$relationship <- as.factor(test$relationship)
test$race <- as.factor(test$race)
test$sex <- as.factor(test$sex)
test$native <- as.factor(test$native)
test$Salary <- as.factor(test$Salary)

#Plotting the Data
plot()

ggplot(train, aes(x = sex,y = educationno)) + geom_bar(stat = "Identity",col = "pink3") + theme_classic()

#Lets build the SVM model with Kernel vanilladot
model1 <- ksvm(Salary~. , data = train, kernel = "vanilladot") #Vanilladot - Linear kernel

print(model1) #Training error : 0.151719 
#Lets Evaluate the Model Performance
pred1 <- predict(model1, test[-14])
tab1 <- table(test[,14], pred1)
equals <- test[,14] == pred1
prop.table(table(equals))
#The Accuracy of the Model is 84.62%

#lets build Models using Different Kernels and Compare their Accuracy
model2 <- ksvm(Salary~. , data = train, kernel = "rbfdot") # rbfdot - radial basis function kernel(Gaussian) #Sigma parametr can be used
model2 #Training Error : 0.1377
pred2 <- predict(model2, test[-14])
tab2 <- table(test[,14], pred2)
equals2 <- test[,14] == pred2
prop.table(table(equals2))
#The Accuracy of the Model is 85.44%

model3 <- ksvm(Salary~. , data = train, kernel = "polydot", kpar = list(degree = 2, scale = 1)) #polydot - Polynomial kernel
model3 #Training Error - 0.131
pred3 <- predict(model3, test[-14])
tab3 <- table(test[,14], pred3)
equals3 <- test[,14] == pred3
prop.table(table(equals3))

#The Accuracy of the Model is 84.44%

model4 <- ksvm(Salary~. , data = train, kernel = "tanhdot") #tanhdot - Hyperbolic Tangent kernel
model4 #Training Error - 0.336666
pred4 <- predict(model4, test[-14])
tab4 <- table(test[,14], pred4)
equals4 <- test[,14] == pred4
prop.table(table(equals4))

#The Accuracy of the Model is 66.38%

model5 <- ksvm(Salary~. , data = train, kernel = "laplacedot", kpar = list(sigma =1.5), C = 1.5) #laplacedot - Laplacian kernel
model5 #Training Error - 0.053844
pred5 <- predict(model5, test[-14])
tab5 <- table(test[,14], pred5)
equals5 <- test[,14] == pred5
prop.table(table(equals5))

#The Accuracy of the Model is 84.35%

model6 <- ksvm(Salary~. , data = train, kernel = "anovadot") #anovadot - ANOVA RBF kernel - parameters(Sigma, degree)
model6 #Training Error - 0.218295
pred6 <- predict(model6, test[-14])
tab6 <- table(test[,14], pred6)
equals6 <- test[,14] == pred6
prop.table(table(equals6))

#The Accuracy of the Model is 78.26%

model7 <- ksvm(Salary~. , data = train, kernel = "splinedot", C=10) #splinedot - spline kernel 
model7 #Training Error - 0.383243
pred7 <- predict(model7, test[-14])
tab7 <- table(test[,14], pred7)
equals7 <- test[,14] == pred7
prop.table(table(equals7))

#The Accuracy of the Model is 75.16%

model8 <- ksvm(Salary~. , data = train, kernel = "besseldot", kpar = list(degree = 2), C= 15) #besseldot - String kernel - parameters(degree, sigma,order)
model8
pred8 <- predict(model8, test[-14])
tab8 <- table(test[,14], pred8)
equals8 <- test[,14] == pred8
prop.table(table(equals8))
#Here the Accuracy of the Model is 77.22%

#So among all Models, Laplacedot kernel Model has the Lowest Training Error and an Accuracy of 84.35%
#So we can say Laplacedot kernel Model is the best Model for Prediction for our Data.