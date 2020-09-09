#Random forest

#A cloth manufacturing company is interested to know about the segment or attributes causes high sale. 

#A Random Forest can be built with target variable Sale (we will convert it in categorical variable) & all other variable will be independent in the analysis.  

library(moments)
library(randomForest)
library(caret)
library(gmodels)
library(pROC)
library(DMwR)
library(dplyr)
library(dismo)

#Lets Import the Dataset
company <- read.csv("E:/Data science Excelr/Assigments/Random Forest/Company Data/Company_Data.csv")
attach(company)
summary(company) 
names(company)#Gives the Names of the Columns in the Dataset

str(company) #Gives the Structure of the Dataset
#WE can see that there are 8 columns with numeric Variable and 3 columns with character Variable
#Lets Convert the Character Variables into Categorical Variable

company$ShelveLoc <- as.factor(company$ShelveLoc)
company$Urban <- as.factor(company$Urban)
company$US <- as.factor(company$US)

#Standard Deviation
sd(Sales)
sd(CompPrice)
sd(Income)
sd(Advertising)
sd(Population)
sd(Price)
sd(Age)
sd(Education)

#Variance
var(Sales)
var(CompPrice)
var(Income)
var(Advertising)
var(Population)
var(Price)
var(Age)
var(Education)

#skewness
skewness(Sales)
skewness(CompPrice)
skewness(Income)
skewness(Advertising)
skewness(Population)
skewness(Price)
skewness(Age)
skewness(Education)

#Kurtosis
kurtosis(Sales)
kurtosis(CompPrice)
kurtosis(Income)
kurtosis(Advertising)
kurtosis(Population)
kurtosis(Price)
kurtosis(Age)
kurtosis(Education)


#Correlation matrix
cor(company[,-c(7,10,11)]) #Categorical Variable are not used for Correlation. cor() accepts only the numeric variable

#Lets Plot the Data

ggplot(company) + geom_histogram(aes(Sales),binwidth = 0.5, color = "yellow")

ggplot(company) + geom_histogram(aes(CompPrice),binwidth = 10, color = "lavender")

ggplot(company) + geom_histogram(aes(Income),binwidth = 1, color = "red")

ggplot(company) + geom_histogram(aes(Advertising),binwidth = 0.4, color = "turquoise")

ggplot(company) + geom_histogram(aes(Population),binwidth = 20, color = "pink")

ggplot(company) + geom_histogram(aes(Price),binwidth = 9, color = "blue")

ggplot(company) + geom_histogram(aes(Age),binwidth = 0.7, color = "orange")

ggplot(company) + geom_histogram(aes(Education),binwidth = 0.2, color = "cyan")


range(Sales)
#When we got the Summary of the Dataset, we Saw that the Sales variable has a range of 0 to 16.27
#So here We will Split the Variable
#we will create a new variale "High"
#If the Sales is Greater than 9 then It is a Yes Else No

High <- ifelse(Sales > 9,"Yes","No")
High <- as.factor(High)

#Lets Combine it with the new Dataset
company_new <- cbind(company[,-1],High) #Here we have Excluded the Sales Column as we have Derived a new Variable High Using it.
str(company_new)
prop.table(table(company_new$High)) #The ratio of class imbalance here is 70:30

#Lets Create the Training and Testing sets
indatapartition <- createDataPartition(company_new$High, p=.70, list = F) #This will Hold 70% of the whole dataset
training <- company_new[indatapartition,]
testing <- company_new[-indatapartition,]


#Lets Build a trainControl setup for the Training Class - K-Folds Technique
ctrl <- trainControl(method = "repeatedcv", number = 5, repeats = 15, verboseIter = TRUE, classProbs = TRUE)
#I have used this function in the model inside the For Loop
####Repeatedcv= Cross validation, number=5(i.e. In 5 fold cross validation ,the train dataset is broken into 5 parts
#and then the model are made of 4 parts and 1 part is used for error estimation and this is repeated 5 times
#with a different part used for error estimation)

#Lets Build a model for the Entire Dataset

rf1 <- randomForest(High~. , data = company_new, ntree = 100, type = "class") #ntree is the Number of trees
rf1 #Gives the Model Details
summary(rf1)#Gives the Summary of the Model
print(importance(rf1)) #returns the MeanDecreaseGini value of the features. The feature with Highest value is the best feature of the Model

#Lets Predict the Values for the Whole Data and see the Accuracy
pred1<- predict(rf1, company_new[,-11])
tab1 <- table(company_new[,11], pred1)
tab1
Acc1 <- sum(diag(tab1))/sum(tab1)
Acc1

#Lets Make a loop for Different number of Trees
trees1 <- c(100,500,700,1100,1500)

acc <- c()
precision1 <- c()
recall1 <- c()
f1score <- c()
auc <- c()

for(i in trees1) {
  print(i)
  
  forest1 <- randomForest(High~. , data = training, trControl = ctrl,method = "gbm", ntree = i)
  forest1
  pred <- predict(forest1,testing[,-11], type = "response")
  conf <- confusionMatrix(pred,testing$High, mode = "everything")
  conf$byClass
  areaundercurve <- roc(response = testing$High, predictor =factor(pred, ordered = TRUE), decreasing = TRUE)
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




#Lets Normalize the Data and see if the Accuracy of the Model Increases

normalise <- function(x) {
  return((x - max(x))/(max(x) - min(x)))
}

company_norm <- as.data.frame(lapply(company_new[,-c(6,9,10,11)], normalise))
company_norm_data <- cbind(company_norm, company_new[,c(6,9,10,11)])
summary(company_norm_data)

#Lets Divide the Data in train and test set
indata1 <- createDataPartition(company_norm_data$High, p=.70, list = F) #This will Hold 70% of the whole dataset
training1 <- company_new[indata1,]
testing1 <- company_new[-indata1,]

#Lets Build the Model
rf5 <- randomForest(High~. , data = training1, ntree = 500)
rf5
print(importance(rf5))
pred5 <- predict(rf5, testing1[,-11])
tab5 <- table(testing1[,11], pred5)
tab5
Acc5 <- sum(diag(tab5))/sum(tab5)
Acc5
#So here we observe that the Accuracy has decreased by almost 12%

acc1 <- c()
precision11 <- c()
recall11 <- c()
f1score1 <- c()
auc1 <- c()

for(i in trees1) {
  print(i)
  
  forest11 <- randomForest(High~. , data = training1, trControl = ctrl,method = "gbm", ntree = i)
  forest11
  pred1 <- predict(forest11,testing1[,-11], type = "response")
  conf1 <- confusionMatrix(pred1,testing$High, mode = "everything")
  conf1$byClass
  areaundercurve1 <- roc(response = testing1$High, predictor =factor(pred1, ordered = TRUE), decreasing = TRUE)
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

print(importance(rf1))

#So we Can Conclude from the model ,that the Variables that cause High sales are Advertising, ShelveLoc and Price.
