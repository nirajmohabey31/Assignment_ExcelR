#K-Nearest Neighbour Classifier for Glass 

#Prepare a model for glass classification using KNN

#Loading the packages
library(class)
library(moments)
library(ggplot2)
library(gmodels)
library(caret)

#Lets Import the Data
glass <- read.csv("E:/Data science Excelr/Assigments/KNN/Glass/glass.csv")
attach(glass)
summary(glass)
str(glass) #Here we can see that Type variable is recognized as Integer. We have to change it into Categorical Variable

Type <- as.factor(glass$Type)

dim(glass) #Gives the Number of dimension of the Dataset

#Standard Deviation
sd(RI)
sd(Na)
sd(Mg)
sd(Al)
sd(Si)
sd(K)
sd(Ca)
sd(Ba)
sd(Fe)

#Variance
var(RI)
var(Na)
var(Mg)
var(Al)
var(Si)
var(K)
var(Ca)
var(Ba)
var(Fe)

#Skewness
skewness(RI)
skewness(Na)
skewness(Mg)
skewness(Al)
skewness(Si)
skewness(K)
skewness(Ca)
skewness(Ba)
skewness(Fe)

#Kurtosis
kurtosis(RI)
kurtosis(Na)
kurtosis(Mg)
kurtosis(Al)
kurtosis(Si)
kurtosis(K)
kurtosis(Ca)
kurtosis(Ba)
kurtosis(Fe)


#Data profiling
DataExplorer::create_report(glass)
#Gives a html page which has basic statistics,missing data profile,data structure,univariate distribution
#QQplot,COrrelation matrix,PCA.

#Plots
ggplot(glass, aes(x=Na)) +geom_histogram( binwidth=0.5, fill="pink3", color="black") + ggtitle("Plot for Na") +theme_bw()

ggplot(glass, aes(x=K)) +geom_histogram( binwidth=0.1, fill="pink3", color="black") + ggtitle("Plot for K") +theme_gray()

ggplot(glass, aes(x=Al)) +geom_histogram( binwidth=0.05, fill="pink4", color="black") + ggtitle("Plot for Al") +theme_get()

ggplot(glass, aes(x=Fe)) +geom_histogram( binwidth=0.055, fill="pink4", color="black") + ggtitle("Plot for Fe") +theme_dark()

#Lets derive the Normalization Function

normalise <- function(x) 
{
  return((x - min(x))/(max(x) - min(x)))
}

#Lets apply the Function on the Data

glass_n <- as.data.frame(lapply(glass[,-10], normalise))
glass_nl <- cbind(glass_n,Type) #Combining the Normlized data and the Type Column


#Lets Divide the DataSet into Training and Testing Sets
indatapartition <- createDataPartition(glass_nl$Type, p=.70, list = FALSE)
training <- glass_nl[indatapartition,]
testing <- glass_nl[-indatapartition,]

#Creating a loop to find the optimum k value
i=1
k.optm=1
for (i in 1:30) {
  
  knn.mod<-knn(train = training, test = testing,cl = training[,10],k=i)
  k.optm[i] <- 100*sum(training[,10] == knn.mod)/NROW(training[,10])
  k=i
  cat(k,"=",k.optm[i],'\n')
  
}

plot(k.optm,type = "b")
#We can the k value doesn't make much difference. 


#Lets Build the KNN model
#For K=1
glassknn <- knn(train = training[,-10], test = testing[,-10], cl = training[,10], k=1) #cl stands for Classification
glassknn

#Lets Evaluate the Accuracy of the Model
CrossTable(testing$Type, glassknn, prop.r = F, prop.c = F, prop.chisq = F)
tab <- table(testing$Type, glassknn)
Accuracy <- round(sum(diag(tab))/sum(tab)*100, digits = 2) #Here Digits attribute Specifies the Number of digits after Decimal Point
Accuracy   #70.49 %

#For K=4
glassknn4 <- knn(train = training[,-10], test = testing[,-10], cl = training[,10], k=4) #cl stands for Classification

#Lets Evaluate the Accuracy of the Model
CrossTable(testing$Type, glassknn4, prop.r = F, prop.c = F, prop.chisq = F)
tab4 <- table(testing$Type, glassknn4)
Accuracy4 <- round(sum(diag(tab4))/sum(tab4)*100, digits = 2) #Here Digits attribute Specifies the Number of digits after Decimal Point
Accuracy4   #67.21 %

#For K=6
glassknn6 <- knn(train = training[,-10], test = testing[,-10], cl = training[,10], k=6) #cl stands for Classification

#Lets Evaluate the Accuracy of the Model
CrossTable(testing$Type, glassknn6, prop.r = F, prop.c = F, prop.chisq = F)
tab6 <- table(testing$Type, glassknn6)
Accuracy6 <- round(sum(diag(tab6))/sum(tab6)*100, digits = 2) #Here Digits attribute Specifies the Number of digits after Decimal Point
Accuracy6   #60.66%

#For K=10
glassknn10 <- knn(train = training[,-10], test = testing[,-10], cl = training[,10], k=10) #cl stands for Classification


#Lets Evaluate the Accuracy of the Model
CrossTable(testing$Type, glassknn10, prop.r = F, prop.c = F, prop.chisq = F)
tab10 <- table(testing$Type, glassknn10)
Accuracy10 <- round(sum(diag(tab10))/sum(tab10)*100, digits = 2) #Here Digits attribute Specifies the Number of digits after Decimal Point
Accuracy10   #59.01 %

#So we can see that the Accuracy is decreasing as we increase the k value.

#Lets Improve the Model Performance
#Lets Scale the Values of the Dataset Using Scale() function
glass_sc <- as.data.frame(scale(glass[,-10]))
glass_scaled <- cbind(glass_sc, Type)


#Lets Divide the Data in Training and Testing Sets
indatapartition1 <- createDataPartition(glass_scaled$Type, p=.50, list = FALSE)
train_scaled <- glass_scaled[indatapartition1,]
test_scaled <- glass_scaled[-indatapartition1,]

#Creating a loop to find the optimum k value
j=1
k.optm1=1
for (j in 1:30) {
  
  knn.mod1<-knn(train = train_scaled, test = test_scaled,cl = train_scaled[,10],k=j)
  k.optm1[j] <- 100*sum(train_scaled[,10] == knn.mod1)/NROW(train_scaled[,10])
  k=j
  cat(k,"=",k.optm1[j],'\n')
  
}

plot(k.optm1,type = "b")
#As we can see as we are increasing k value optimization is decreasing

#Lets Build the KNN Classifier Model for Scaled Values
#For K=1
knn1scale <- knn(train = train_scaled[,-10], test = test_scaled[,-10], cl = train_scaled[,10], k=1) #cl stands for Classification

#Lets Evaluate the Accuracy of the Model
CrossTable(test_scaled$Type,knn1scale, prop.r = F, prop.c = F, prop.chisq = F)
tabscale1 <- table(test_scaled$Type,knn1scale)
Accuracy1 <- round(sum(diag(tabscale1))/sum(tabscale1)*100, digits = 2) #Here Digits attribute Specifies the Number of digits after Decimal Point
Accuracy1  #60.95%

#So here We can see that the Model hasnt been changed much using Scale Function, still Lets do it for Different k values

#For K=3
knn3scale <- knn(train = train_scaled[,-10], test = test_scaled[,-10], cl = train_scaled[,10], k=3) #cl stands for Classification
#Lets Evaluate the Accuracy of the Model
CrossTable(test_scaled$Type,knn3scale, prop.r = F, prop.c = F, prop.chisq = F)
tabscale3 <- table(test_scaled$Type,knn3scale)
Accuracy3 <- round(sum(diag(tabscale3))/sum(tabscale3)*100, digits = 2) #Here Digits attribute Specifies the Number of digits after Decimal Point
Accuracy3  #63.81%

#For K=4
knn4scale <- knn(train = train_scaled[,-10], test = test_scaled[,-10], cl = train_scaled[,10], k=4) #cl stands for Classification

#Lets Evaluate the Accuracy of the Model
CrossTable(test_scaled$Type,knn4scale, prop.r = F, prop.c = F, prop.chisq = F)
tabscale4 <- table(test_scaled$Type,knn4scale)
Accuracyscale4 <- round(sum(diag(tabscale4))/sum(tabscale4)*100, digits = 2) #Here Digits attribute Specifies the Number of digits after Decimal Point
Accuracyscale4  #62.86 %

#For K=7
knn7scale <- knn(train = train_scaled[,-10], test = test_scaled[,-10], cl = train_scaled[,10], k=7) #cl stands for Classification

#Lets Evaluate the Accuracy of the Model
CrossTable(test_scaled$Type,knn7scale, prop.r = F, prop.c = F, prop.chisq = F)
tabscale7 <- table(test_scaled$Type,knn7scale)
Accuracy7 <- round(sum(diag(tabscale7))/sum(tabscale7)*100, digits = 2) #Here Digits attribute Specifies the Number of digits after Decimal Point
Accuracy7 #65.57%


#As we can conclude from the above models for K= 1 with and without scaling gives much more accuracy as compared with others.