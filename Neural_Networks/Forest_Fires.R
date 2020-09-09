#Neural Network

#PREDICT THE BURNED AREA OF FOREST FIRES WITH NEURAL NETWORKS
library(nnet)
library(neuralnet)
library(caret)
library(ggplot2)

forest <- read.csv("E:/Data science Excelr/Assigments/Supervised Machine Learning/Neural Networks/Forest Fires/forestfires.csv")
attach(forest)
#We will Exclude column 1,2 and from 12 to 30
forest <- forest[,-c(1,2,12:30)]
summary(forest)
names(forest)

dim(forest)

#In this dataset we have to Convert the Size_category into a categorical Variable
forest$size_category<- as.factor(forest$size_category)
category <- forest$size_category


#Lets Normalise the data Using ScaLe Method
forest_scale <- as.data.frame(scale(forest[,-10]))
forest_scale <- cbind(forest_scale, category)
str(forest_scale)
prop.table(table(category))
attach(forest_scale)

#Plots
ggplot(forest_scale) + geom_histogram(aes(temp), binwidth = 0.1,fill = ' black' , col = 'white') + ggtitle("Temperature") #The Temperature Data is Normally Distributed

ggplot(forest_scale) + geom_bar(aes(wind), width = 0.1) + ggtitle("Wind") #The Wind Data is Slightly Skewed on the right

ggplot(forest_scale) + geom_point(aes(FFMC, DMC)) #Most of the Points are Positive

ggplot(forest_scale) + geom_histogram(aes(area), binwidth = 0.5)

#Lets Divide the Data in Train and Test set
datapart <- sample(nrow(forest_scale), nrow(forest_scale)*.7)
train1 <- forest_scale[datapart,]
test1 <- forest_scale[-datapart,]

#Lets Build a Model and Check the Accuracy
nnclassifier <- neuralnet(formula = category~. , data = train1, linear.output = T)
plot(nnclassifier) #error=0.11
summary(nnclassifier)
comp <- compute(nnclassifier, test1[,-10])
comp
comp.weights <- comp$net.result
idx <- apply(comp.weights, 1, which.max)
pred <- c("large", "small")[idx]
conf <- confusionMatrix(test1$category, factor(pred), mode = "everything")
conf  #Accuracy = 98.72%

#Lets Improve the Model Accuracy by adding hidden layers
nnclassifier1 <- neuralnet(formula = category~. , data = train1, hidden = c(2,1))
plot(nnclassifier1) #error= 0.0052
summary(nnclassifier1)
comp1 <- compute(nnclassifier1, test1[,-10])
comp1
comp.weights1 <- comp1$net.result
idx1 <- apply(comp.weights1, 1, which.max)
pred1 <- c("large", "small")[idx1]
conf1 <- confusionMatrix(test1$category, factor(pred1), mode = "everything")
conf1  #Accuracy = 98.72%

nnclassifier2 <- neuralnet(formula = category~. , data = train1, hidden = c(3,3,2,1))
plot(nnclassifier2) #0.0027
summary(nnclassifier2)
comp2 <- compute(nnclassifier2, test1[,-10])
comp2
comp.weights2 <- comp2$net.result
idx2 <- apply(comp.weights2, 1, which.max)
pred2 <- c("large", "small")[idx2]
conf2 <- confusionMatrix(test1$category, factor(pred2), mode = "everything")
conf2 #Accuracy = 98.72 %

nnclassifier3 <- neuralnet(formula = category~. , data = train1, hidden = c(5,4,3,2))
plot(nnclassifier3) #0.000297
summary(nnclassifier3)
comp3 <- compute(nnclassifier3, test1[,-10])
comp3
comp.weights3 <- comp3$net.result
idx3 <- apply(comp.weights3, 1, which.max)
pred3 <- c("large", "small")[idx3]
conf3 <- confusionMatrix(test1$category, factor(pred3), mode = "everything")
conf3  #Accuracy= 97.44%

nnclassifier4 <- neuralnet(formula = category~. , data = train1, hidden = c(10,8,6,4,3,2))
plot(nnclassifier4) #0.003
summary(nnclassifier4)
comp4 <- compute(nnclassifier4, test1[,-10])
comp4
comp.weights4 <- comp4$net.result
idx4 <- apply(comp.weights4, 1, which.max)
pred4 <- c("large", "small")[idx4]
conf4 <- confusionMatrix(test1$category, factor(pred4), mode = "everything")
conf4 #Accuracy = 95.51%

#Here we can say nnclassifier2 is best of all with highest accuracy ,less error ,pvalue<0.05 and also kappa value is 0.96