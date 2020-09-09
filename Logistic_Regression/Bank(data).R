#Logistic regression For bank

#Output variable -> y

#Lets Import the dataset


bank <- read.csv(file.choose(), sep = ";") #The data in the file is seperated using Semi-Colons
attach(bank)
summary(bank)

#Standard Deviation
sd(age)
sd(balance)
sd(duration)
sd(campaign)
sd(pdays)
sd(previous)

#Variance
var(age)
var(balance)
var(duration)
var(campaign)
var(pdays)
var(previous)

library(moments)
#sKEWNESS
skewness(age)
skewness(balance)
skewness(duration)
skewness(campaign)
skewness(pdays)
skewness(previous)

#KURTOSIS
kurtosis(age)
kurtosis(balance)
kurtosis(duration)
kurtosis(campaign)
kurtosis(pdays)
kurtosis(previous)

#We see that the categorical Variables are defined as Character. Lets convert it into the Categorical variable using Factor function

bank$job <- as.factor(bank$job)
bank$marital <- as.factor(bank$marital)
bank$education <- as.factor(bank$education)
bank$default <- as.factor(bank$default)
bank$housing <- as.factor(bank$housing)
bank$loan <- as.factor(bank$loan)
bank$contact <- as.factor(bank$contact)
bank$month <- as.factor(bank$month)
bank$poutcome <- as.factor(bank$poutcome)
bank$y <- as.factor(bank$y)

#Lets Build a generalized linear regression model

model <- glm(y~. , data = bank, family = "binomial")
summary(model)

# To calculate the odds ratio manually we going r going to take exp of coef(model)
exp(coef(model))

# Confusion matrix table 
#pred <- ifelse(pred >0.8,"yess","no")
pred <- predict(model,bank,type="response")
summary(pred)


# Confusion matrix and considering the threshold value as 0.8
confusion<-table(pred>0.8,bank$y)
confusion

#Lets compute the Accuracy of the Model
accuracy <- sum(diag(confusion))/sum(confusion)
accuracy
#89.27 % Accuracy

misclasification<- 1-sum(diag(confusion))/sum(confusion)
misclasification #10.723%



#ROC Curve
library(ROCR)

rocrpred<-prediction(pred,bank$y)
rocrperf<-performance(rocrpred,'tpr','fpr')


###Area Under Curve
auc <- performance(rocrpred ,"auc")
auc  <- unlist(slot(auc ,"y.values"))
auc #90.79% Auc:- area under curve


plot(rocrperf,colorize=T)
legend( .3, .4 , auc , title = "AUC" , cex = 1)

# More area under the ROC Curve better is the logistic regression model obtained

## Getting cutt off or threshold value along with true positive and false positive rates in a data frame 

rocr_cutoff <- data.frame(cut_off = rocrperf@alpha.values[[1]],fpr=rocrperf@x.values,tpr=rocrperf@y.values)
colnames(rocr_cutoff) <- c("cut_off","FPR","TPR")
View(rocr_cutoff)

library(dplyr)
rocr_cutoff$cut_off <- round(rocr_cutoff$cut_off,6)

# Sorting data frame with respect to tpr in decreasing order 
rocr_cutoff <- arrange(rocr_cutoff,desc(TPR))
View(rocr_cutoff)