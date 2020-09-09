#K-Nearest Neighbour Classifier

#Implement a KNN model to classify the animals in to categories

#Loading the packages
library(caret)
library(gmodels)
library(class)


#Import the Data
zoo <- read.csv("E:/Data science Excelr/Assigments/KNN/Zoo/Zoo.csv")
attach(zoo)
View(zoo)
summary(zoo)#Get the Structure of the Dataset

#The data is in matrix form so we aint performing any Eda

str(zoo)
#Now We want the Type column to be a Categorical Variable as we have to Predict the Type of the Animal Category,so lets Convert it into Categorial Variable
type <- as.factor(zoo$type)

str(zoo)
#Now the type is converted into factor with 7 level

#Lets Normalize the Data
#Lets Derive a Function for Nomalize the Data

normalise <- function(x)
{
  return((x - min(x))/(max(x) - min(x)))
}

#Lets Apply the Function on the Data
zoo_n <- as.data.frame(lapply(zoo[-17], normalise))
zoo_norm <- cbind(zoo_n,type)

str(zoo_norm)

#Lets Create Data Partition for defining Training and Testing Sets
#I will Take 60% of the Total Dataset Randomly for Training set and the rest 40%in the Testing set
part <- createDataPartition(zoo_norm$type, p=.60, list = F)
training <- zoo_norm[part,]
testing <- zoo_norm[-part,]

str(training)

#Creating a loop to find the optimum k value
i=1
k.optm=1
for (i in 1:20) {
  
  knn.mod <-knn(train = training, test = testing,cl = training[,17],k=i)
  k.optm[i] <- 100*sum(training[,17] == knn.mod)/NROW(training[,17])
  k=i
  cat(k,"=",k.optm[i],'\n')
  
}

plot(k.optm,type = "b")
#As seen in plot values doesnt make much difference still we ll explore

#Now Lets Build the KNN Classifier Model
#For k=1
knn1 <-knn(train = training[,-17], test = testing[,-17], cl = training$type, k=1) 
CrossTable(testing$type,knn1, prop.r = F, prop.c = F, prop.chisq = F)
tab1 <- table(testing$type, knn1)
Acc1 <- round((sum(diag(tab1))/sum(tab1))*100, digits = 2)
Acc1
#For k=1 the Accuracy is 94.87%


#For k=3
knn3 <-knn(train = training[,-17], test = testing[,-17], cl = training$type, k=3) 
CrossTable(testing$type,knn3, prop.r = F, prop.c = F, prop.chisq = F)
tab3 <- table(testing$type, knn3)
Acc3 <- round((sum(diag(tab3))/sum(tab3))*100, digits = 2)
Acc3
#For k=3 the Accuracy is 92.31%

#For k=5
knn5 <-knn(train = training[,-17], test = testing[,-17], cl = training$type, k=5) 
CrossTable(testing$type,knn5, prop.r = F, prop.c = F, prop.chisq = F)
tab5 <- table(testing$type, knn5)
Acc5 <- round((sum(diag(tab5))/sum(tab5))*100, digits = 2)
Acc5
#For k=5 the Accuracy is 89.74%

#For K=7
knn7 <-knn(train = training[,-17], test = testing[,-17], cl = training$type, k=7) 
CrossTable(testing$type,knn7, prop.r = F, prop.c = F, prop.chisq = F)
tab7 <- table(testing$type, knn7)
Acc7 <- round((sum(diag(tab7))/sum(tab7))*100, digits = 2)
Acc7
#For k=7 the Accuracy is 87.14%

#for K=21
knn21 <-knn(train = training[,-17], test = testing[,-17], cl = training$type, k=21) 
CrossTable(testing$type,knn21, prop.r = F, prop.c = F, prop.chisq = F)
tab21 <- table(testing$type, knn21)
Acc21 <- round((sum(diag(tab21))/sum(tab21))*100, digits = 2)
Acc21
#For k=21 the Accuracy is 79.49%

#WE saw that K=1 gave more accuracy than others and accuray is kind of decreasing as we increase k


#Lets do it by Scaling the Datapoints using Scale() function.
zoo_scale <- as.data.frame(scale(zoo[,-17]))
type <- as.factor(zoo$type)
zoo_scale_data <- cbind(zoo_scale, type)


str(zoo_scale_data)

#Lets Create Data Partition for defining Training and Testing Sets
#I will Take 60% of the Total Dataset Randomly for Training set and the rest 40%in the Testing set
part1 <- createDataPartition(zoo_norm$type, p=.60, list = F)
train_scaled <- zoo_norm[part1,]
test_scaled <- zoo_norm[-part1,]


j=1
k.optm1=1
for (j in 1:30) {
  
  knn.mod1<-knn(train = train_scaled, test = test_scaled,cl = train_scaled[,17],k=j)
  k.optm1[j] <- 100*sum(train_scaled[,17] == knn.mod1)/NROW(train_scaled[,17])
  k=j
  cat(k,"=",k.optm1[j],'\n')
  
}

plot(k.optm1,type = "b")



#Lets Build the KNN Classifier Model for Scaled Values
#For K=1
knn1scale <- knn(train = train_scaled[,-17], test = test_scaled[,-17], cl = train_scaled[,17], k=1) #cl stands for Classification

#Lets Evaluate the Accuracy of the Model
CrossTable(test_scaled$type,knn1scale, prop.r = F, prop.c = F, prop.chisq = F)
tabscale1 <- table(test_scaled$type,knn1scale)
Acc_Scale1 <- round(sum(diag(tabscale1))/sum(tabscale1)*100, digits = 2) #Here Digits attribute Specifies the Number of digits after Decimal Point
Acc_Scale1  
# For K= 1 accyracy is 97.44%


#For K=11
knn11scale <- knn(train = train_scaled[,-17], test = test_scaled[,-17], cl = train_scaled[,17], k=11) #cl stands for Classification

#Lets Evaluate the Accuracy of the Model
CrossTable(test_scaled$type,knn11scale, prop.r = F, prop.c = F, prop.chisq = F)
tabscale11 <- table(test_scaled$type,knn11scale)
Acc_Scale11 <- round(sum(diag(tabscale11))/sum(tabscale11)*100, digits = 2) #Here Digits attribute Specifies the Number of digits after Decimal Point
Acc_Scale11  #92.05%


#For K=25
knn25scale <- knn(train = train_scaled[,-17], test = test_scaled[,-17], cl = train_scaled[,17], k=25) #cl stands for Classification

#Lets Evaluate the Accuracy of the Model
CrossTable(test_scaled$type,knn25scale, prop.r = F, prop.c = F, prop.chisq = F)
tabscale25 <- table(test_scaled$type,knn25scale)
Acc_Scale25 <- round(sum(diag(tabscale25))/sum(tabscale25)*100, digits = 2) #Here Digits attribute Specifies the Number of digits after Decimal Point
Acc_Scale25 #69.23 %

#For K=30
knn30scale <- knn(train = train_scaled[,-17], test = test_scaled[,-17], cl = train_scaled[,17], k=30) #cl stands for Classification

#Lets Evaluate the Accuracy of the Model
CrossTable(test_scaled$type,knn30scale, prop.r = F, prop.c = F, prop.chisq = F)
tabscale30 <- table(test_scaled$type,knn30scale)
Acc_Scale30 <- round(sum(diag(tabscale30))/sum(tabscale30)*100, digits = 2) #Here Digits attribute Specifies the Number of digits after Decimal Point
Acc_Scale30 #66.67 %

#Here we see same thing as we are increasing our k value accuracy is decreasing.


#So we could conclude that model with K = 1 has best accuracy of all and will classify the animals categories best that others.