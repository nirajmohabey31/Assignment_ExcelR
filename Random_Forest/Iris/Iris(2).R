#Random forest

#we Build a Random Forest for the Iris Data 
data("iris")
datasets::iris
attach(iris)

library(moments)
library(descr)
library(caret)
library(randomForest)

summary(iris)

names(iris) #Gives the Names of the columns 

dim(iris) #Returns the dimension of table

str(iris) #Gives the Structure of the  entire Data

#Standard Deviation
sd(Sepal.Length)
sd(Sepal.Width)
sd(Petal.Length)
sd(Petal.Width)

#Variance
var(Sepal.Length)
var(Sepal.Width)
var(Petal.Length)
var(Petal.Width)

#Skewness
Skewness(Sepal.Length)
skewness(Sepal.Width)
skewness(Petal.Length)
skewness(Petal.Width)

#Kurtosis
kurtosis(Sepal.Length)
kurtosis(Sepal.Width)
kurtosis(Petal.Length)
kurtosis(Petal.Width)


boxplot(iris) #Displays the Boxplot for every column in the Dataset #sepal width has many outliers 

pairs(iris) #Using Pairs Function we get plot with which we can see their behaviour towards eachother

#Lets Divide the Data for building the model
ind <- createDataPartition(iris$Species,p=.70,list = F) #.70 means 70% Partition
training<- iris[ind,]
testing<- iris[-ind,]

#Lets Build the Random forest Model
#500 Trees
rf1 <- randomForest(Species~. , data = training, ntree = 500)
rf1
print(importance(rf1))
pred1 <- predict(rf1, testing[,-5])
CrossTable(testing[,5], pred1)
tab1 <- table(testing[,5], pred1)
sum(diag(tab1))/ sum(tab1)


#800 Trees
rf2 <- randomForest(Species~. , data = training, ntree = 800)
rf2
print(importance(rf2))
pred2 <- predict(rf2, testing[,-5])
CrossTable(testing[,5], pred2)
tab2 <- table(testing[,5], pred2)
sum(diag(tab2))/ sum(tab2)


#1200 tREES
rf3 <- randomForest(Species~. , data = training, ntree = 1200)
rf3
print(importance(rf3))
pred3 <- predict(rf3, testing[,-5])
CrossTable(testing[,5], pred3)
tab3 <- table(testing[,5], pred3)
sum(diag(tab3))/ sum(tab3)