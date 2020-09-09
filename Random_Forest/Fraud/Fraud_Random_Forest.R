#Random Forest

#Random Forest to prepare a model on fraud data 
#Treating those who have taxable_income <= 30000 as "Risky" and others are "Good"
library(randomForest)
library(moments)
library(gmodels)
library(ggplot2)
library(caret)
library(dplyr)
library(pROC)


#Lets Import the Data
fraud <- read.csv("E:/Data science Excelr/Assigments/Random Forest/Fraud check/Fraud_check.csv")
attach(fraud)
dim(fraud) #Returns dimensions of the Dataset

names(fraud) #Returns the Column Names of the dataset

str(fraud) 
#Gives the Entire Structure of the dataset
#Here we can see that there are 3 Numerical Columns and 3 Categorical variable. 
#Lets convert the Categorical columns into Factors


#Lets Find the Summary of the Data
summary(fraud)


fraud$Undergrad <- as.factor(fraud$Undergrad)
fraud$Marital.Status <- as.factor(fraud$Marital.Status)
fraud$Urban <- as.factor(fraud$Urban)

#Standard Deviation
sd(fraud$Taxable.Income)
sd(fraud$City.Population)
sd(fraud$Work.Experience)

#Variance
var(fraud$Taxable.Income)
var(fraud$City.Population)
var(fraud$Work.Experience)

#Skewness
skewness(Taxable.Income)
skewness(City.Population)
skewness(Work.Experience)

#Kurtosis
kurtosis(Taxable.Income)
kurtosis(City.Population)
kurtosis(Work.Experience)


cor(fraud[,c(3,4,5)]) #Returns the Correlation Matrix 


#Lets Plot the Data

ggplot(fraud) + geom_histogram(aes(Taxable.Income), binwidth = 100, fill = "darkgreen") + xlab("Taxable Income")

ggplot(fraud) + geom_histogram(aes(City.Population), binwidth = 500, fill = "darkgreen")

ggplot(fraud) + geom_histogram(aes(Work.Experience), binwidth = 0.5, fill = "darkgreen")

ggplot(fraud %>% group_by(Undergrad) %>% summarise(Count = n())) + geom_bar(aes(Undergrad, Count), stat = "identity", fill = "pink")

ggplot(fraud %>% group_by(Marital.Status) %>% summarise(Count = n())) + geom_bar(aes(Marital.Status, Count), stat = "identity", fill = "pink3")

ggplot(fraud %>% group_by(Urban) %>% summarise(Count = n())) + geom_bar(aes(Urban, Count), stat = "identity", fill = "red4")

#We have to Build a classification model
#The Condition is if Taxable.Income is <= 30000 then it is "Risky and the rest are "Good"
#So we will convert the Taxable.Income column as Categorical Value with 2 Levels "Risky" and "Good"

#I will use the Ifelse condition to convert the data into Binomial
risk <- ifelse(fraud$Taxable.Income <= 30000, "Risky","Good")
risk <- as.factor(risk)
fraud1 <- cbind(fraud[,-3],risk) #Here we Exclude Taxable.Income Variable as we derived Responsive Variable risk Using it.
table(risk)
#Here we can see a class imbalance


#Now lets divide the data into Train and Test Data with 80% partion

p <- createDataPartition(fraud1$risk, p=.60, list = F) #Here p=.60 means 60% Partition
train1 <- fraud1[p,]
test1 <- fraud1[-p,]


#Lets Build a trainControl setup for the Training Class - K-Folds Technique
ctrl <- trainControl(method = "repeatedcv", number = 5, repeats = 10, verboseIter = TRUE, classProbs = TRUE)
####Repeatedcv= Cross validation, number=5(i.e. In 5 fold cross validation ,the train dataset is broken into 5 parts
#and then the model are made of 4 parts and 1 part is used for error estimation and this is repeated 5 times
#with a different part used for error estimation)


#Lets Build the Random forest Model
rf1 <-  randomForest(risk~. , data = train1, ntree = 500)
rf1

print(importance(rf1))

#Prediction
pred1 <- predict(rf1, test1[,-6])
CrossTable(test1[,6], pred1)
tab1 <- table(test1[,6], pred1)
sum(diag(tab1))/sum(tab1)  #78% Accuracy

trees1 <- c(100,400,700,1100,1500)

acc <- c()
precision1 <- c()
recall1 <- c()
f1score <- c()
auc <- c()

for(i in trees1) {
  print(i)
  
  forest1 <- randomForest(risk~. , data = train1, trControl = ctrl,method = "gbm", ntree = i)
  forest1
  pred <- predict(forest1,test1[,-6], type = "response")
  conf <- confusionMatrix(pred,test1$risk, mode = "everything")
  conf$byClass
  areaundercurve <- roc(response = test1$risk, predictor =factor(pred, ordered = TRUE), decreasing = TRUE)
  acc <- c(acc, conf$overall[1])
  precision1 <- c(precision1, conf$byClass[5])
  recall1 <- c(recall1, conf$byClass[6])
  f1score <- c(f1score, conf$byClass[7])
  auc <- c(auc, areaundercurve$auc)
}
recall1
precision1
acc

Evaluation <-data.frame("No of Trees" = trees1, "Accuracy" = acc, "Precision" = precision1, "Recall" = recall1, "F1" = f1score, "AUC" = auc)
Evaluation


#Lets Normalise the Data and try
normalise <- function(x) 
{
  return((x - max(x))/(max(x) - min(x)))
}

fraud2 <- as.data.frame(lapply(fraud1[,c(3,4)], normalise))
fraud2 <- cbind(fraud2, fraud1[,c(1,2,5,6)])

int <- createDataPartition(fraud2$risk, p=.60, list = F) #Here p=.60 means 60% Partition
train2 <- fraud2[int,]
test2 <- fraud2[-int,]

#Lets Build the Model
rf3 <- randomForest(risk~., data = train2, ntree = 1000)
rf3
print(importance(rf3))
pred3 <- predict(rf3, test2[,-6])
CrossTable(test2[,6], pred3)
tab3 <- table(test2[,6], pred3)
sum(diag(tab3))/ sum(tab3)  #79.4%

acc1 <- c()
precision11 <- c()
recall11 <- c()
f1score1 <- c()
auc1 <- c()

for(i in trees1) {
  print(i)
  
  forest11 <- randomForest(risk~. , data = train2, trControl = ctrl,method = "gbm", ntree = i)
  forest11
  pred1 <- predict(forest11,test2[,-6], type = "response")
  conf1 <- confusionMatrix(pred1,test2$risk, mode = "everything")
  conf1$byClass
  areaundercurve1 <- roc(response = test2$risk, predictor =factor(pred1, ordered = TRUE), decreasing = TRUE)
  acc1 <- c(acc1, conf1$overall[1])
  precision11 <- c(precision11, conf1$byClass[5])
  recall11 <- c(recall11, conf1$byClass[6])
  f1score1 <- c(f1score1, conf1$byClass[7])
  auc1 <- c(auc1, areaundercurve1$auc)
}
recall11
precision11
acc1

Evaluation1 <-data.frame("No of Trees" = trees1, "Accuracy" = acc1, "Precision" = precision11, "Recall" = recall11, "F1" = f1score1, "AUC" = auc1)
Evaluation1

#Here the Variables with high MeanDecreaseGini value are City.Poluation and Work.Experience

#Lets treat the class imbalane using the SMOTE function
library(DMwR)
newdata <- SMOTE(risk~. , data = fraud1, perc.over = 200, perc.under = 200)
newdata
table(newdata$risk)

#Now lets divide it in train and test set
library(caTools)
split <- sample.split(newdata$risk, SplitRatio = 0.75)
train3 <- newdata[split,]
test3 <- newdata[-split,]

rf3 <- randomForest(risk~. , data = train3, ntree = 1500, method = "gbm")
rf3
pred1 <- predict(rf3,test3[,-6], type = "response")
conf1 <- confusionMatrix(pred1,test3$risk, mode = "everything")
conf1$byClass
areaundercurve1 <- roc(response = test3$risk, predictor =factor(pred1, ordered = TRUE), decreasing = TRUE)
areaundercurve1  #0.889

acc1 <-  conf1$overall[1]
precision11 <-conf1$byClass[5]
recall11 <-  conf1$byClass[6]
f1score1 <- conf1$byClass[7]
auc1 <- areaundercurve1$auc

plot(areaundercurve1)
#Now we got the Best F1 score and the Area under curve