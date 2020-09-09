#Decision Tree

#USing decision trees we have to prepare a model on fraud data as those who have taxable_income <= 30000 as "Risky" and others are "Good"

library(party)
library(gmodels)
library(ggplot2)
library(dplyr)
library(caret)
library(C50)

#Lets Import the Data
fraud <- read.csv("E:/Data science Excelr/Assigments/Decision Tree/Fraud/Fraud_check.csv")

dim(fraud) #Returns the dimension of the Dataset

names(fraud) #the Column Names of the dataset

str(fraud) #Gives the Entire Structure of the dataset
#Here we can see that there are 3 Numerical Columns and 3 Categorical variable. 
#Lets convert the Categorical columns into Factors

fraud$Undergrad <- as.factor(fraud$Undergrad)
fraud$Marital.Status <- as.factor(fraud$Marital.Status)
fraud$Urban <- as.factor(fraud$Urban)

#Lets Find the Summary of the Data
summary(fraud)

#Standard Deviation
sd(fraud$Taxable.Income)
sd(fraud$City.Population)
sd(fraud$Work.Experience)

#Variance
var(fraud$Taxable.Income)
var(fraud$City.Population)
var(fraud$Work.Experience)


#Lets Plot the Data

ggplot(fraud) + geom_histogram(aes(Taxable.Income), binwidth = 100, fill = "Pink3") + xlab("Taxable Income")

ggplot(fraud) + geom_histogram(aes(City.Population), binwidth = 100, fill = "Orange")

ggplot(fraud) + geom_histogram(aes(Work.Experience), binwidth = 0.5, fill = "darkgreen")

ggplot(fraud %>% group_by(Undergrad) %>% summarise(Count = n())) + geom_bar(aes(Undergrad, Count), stat = "identity", fill = "green")

ggplot(fraud %>% group_by(Marital.Status) %>% summarise(Count = n())) + geom_bar(aes(Marital.Status, Count), stat = "identity", fill = "red3")

ggplot(fraud %>% group_by(Urban) %>% summarise(Count = n())) + geom_bar(aes(Urban, Count), stat = "identity", fill = "yellow3")

#We have to Build a classification Tree
#The Condition is if Taxable.Income is <= 30000 then it is "Risky otherwise "Good"
#So here we will have to convert the Taxable.Income column as Categorical Value with 2 Levels "Risky" and "Good"

#I will use the Ifelse condition to convert the data into Binomial
risk <- ifelse(fraud$Taxable.Income <= 30000, "Risky","Good")
risk <- as.factor(risk)
fraud1 <- cbind(fraud[,-3],risk) #Here we Exclude Taxable.Income Variable as we derived Responsive Variable risk Using it.

#Now lets divide the data into Train and Test Data with 70% partion

intraininglocal <- createDataPartition(fraud1$risk, p=.70, list = F) #Here p=.70 means 70% Partition For training
train <- fraud1[intraininglocal,]
test <- fraud1[-intraininglocal,]

#Now lets Buld the Decision Tree
tree <- C5.0(risk~. ,data = train)

#Predict for test data

pred<- predict.C5.0(tree,newdata = test[,-7])
a<- table(test$risk,pred) #Creating a table with the test$Taxable.Income values and predicted values
sum(diag(a))/sum(a) #to find the Accuracy of the Model. here it is 79%

CrossTable(test$risk,pred)

#Lets Plot the Model
plot(mtree)

#Lets see the Summary of the Model
summary(mtree)