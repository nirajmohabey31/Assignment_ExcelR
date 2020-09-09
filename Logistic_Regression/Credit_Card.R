#Logistic Regression FOR CREDIT CARD data

#Classify whether application accepted or not using Logistic regression

#Import the creditcard.csv file

creditcard <- read.csv("E:/Data science Excelr/Assigments/Logistic Regression/Credit card/creditcard.csv")
attach(creditcard)
summary(creditcard)
View(creditcard)

creditcard <- creditcard[,-1]
#as column 1 is not useful enough

#Standard Deviation
sd(reports)
sd(age)
sd(income)
sd(share)
sd(expenditure)
sd(dependents)
sd(months)
sd(majorcards)
sd(active)

#Variance
var(reports)
var(age)
var(income)
var(share)
var(expenditure)
var(dependents)
var(months)
var(majorcards)
var(active)

library(moments)
#Skewness
skewness(reports)
skewness(age)
skewness(income)
skewness(share)
skewness(expenditure)
skewness(dependents)
skewness(months)
skewness(majorcards)
skewness(active)

#kurtosis
kurtosis(reports)
kurtosis(age)
kurtosis(income)
kurtosis(share)
kurtosis(expenditure)
kurtosis(dependents)
kurtosis(months)
kurtosis(majorcards)
kurtosis(active)

str(creditcard)
#Lets convert the Categorical columns into Factors
creditcard$card <- as.factor(creditcard$card)
creditcard$owner <- as.factor(creditcard$owner)
creditcard$selfemp <- as.factor(creditcard$selfemp)

summary(creditcard)

#Lets build a Logistic Regression Model
creditcard_model <- glm(card~. , data = creditcard, family = "binomial")
summary(creditcard_model)

# Confusion matrix table 

pred <- predict(creditcard_model,creditcard,type="response")
summary(pred)


# Confusion matrix and considering the threshold value as 0.8
confusiontable<-table(pred>0.5,creditcard$card)
confusiontable

#Lets compute the Accuracy of the Model
accuracy <- sum(diag(confusiontable))/sum(confusiontable)
accuracy
#85.97 % Accuracy

misclasification<- 1-sum(diag(confusiontable))/sum(confusiontable)
misclasification #14.023%

#ROC Curve
library(ROCR)

rocrpred<-prediction(pred,creditcard$card)
rocrperf<-performance(rocrpred,'tpr','fpr')


###Area Under Curve
auc <- performance(rocrpred ,"auc")
auc  <- unlist(slot(auc ,"y.values"))
auc #69.79% Auc:- area under curve


plot(rocrperf,colorize=T)
abline(a=0,b=1)
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