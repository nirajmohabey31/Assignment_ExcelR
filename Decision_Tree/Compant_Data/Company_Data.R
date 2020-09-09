#Decision Tree

#A cloth manufacturing company is interested to know about the segment or attributes causes high sale. 

#A decision tree can be built with target variable "Sale"(we will convert it in categorical variable) & all other variable will be independent variable in the analysis.  

library(C50)
library(factoextra)
library(caret)
library(gmodels)

#Lets Import the Dataset
company <- read.csv("E:/Data science Excelr/Assigments/Decision Tree/Company/Company_Data.csv")
attach(company)
summary(company) #Gives the Summary of the Dataset

str(company) #Gives the Structure of the Dataset
#Here we see that there are 8 columns with numeric Variable and 3 columns with character Variable
#Lets Convert the Character Variables into factor 

names(company)#Gives the Names of the Columns in the Dataset

ShelveLoc <- as.factor(ShelveLoc)
Urban <- as.factor(Urban)
US <- as.factor(US)

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

#Correlation matrix
cor(company[,-c(7,10,11)]) #Categorical Variable are not used for Correlation. cor() accepts only the numeric variable

#Lets Plot the Data

ggplot(company) + geom_histogram(aes(Sales),binwidth = 0.5, col = "red",)

ggplot(company) + geom_histogram(aes(CompPrice),binwidth = 10, color = "cyan")

ggplot(company) + geom_histogram(aes(Income),binwidth = 1, color = "blue")

ggplot(company) + geom_histogram(aes(Advertising),binwidth = 0.4, color = "brown")

ggplot(company) + geom_histogram(aes(Population),binwidth = 20, color = "Black")

ggplot(company) + geom_histogram(aes(Price),binwidth = 9, color = "pink")

ggplot(company) + geom_histogram(aes(Age),binwidth = 0.7, color = "maroon")

ggplot(company) + geom_histogram(aes(Education),binwidth = 0.2, color = "turquoise")


#As we Know that we have to build a Decision Tree with the Dependent Variable as Sale
#But Sale is a Numeric Value so first Lets Convert the Sales Variable into a Categorical Variable

#When we got the Summary of the Dataset, we Saw that the Sales variable has a range of 0 to 16
#So here We will Split the Variable
#we will create a new variale "High"
#If the Sales is Greater than 9 then It is a Yes orelse No

High <- ifelse(Sales > 9,"Yes","No")
High <- as.factor(High)

#Lets Combine it With the Main Dataset
companynew <- cbind(company[,-1], High) #Here we Exclude the Sales Column as we have Derived a responsive Variable High using it

#Lets Create the Training and Testing sets
ind <- createDataPartition(companynew$High, p=.70, list = F) #This will Hold 70% of the whole dataset
training <- company_new[ind,]
testing <- company_new[-ind,]

#Lets Fit the Model for Entire Data Now
tree_model <- C5.0(High~. , data =companynew)
summary(tree_model) #Here we can See there was an Error of 6.5%
tree_model

#Lets Plot the Tree
plot(tree_model)

#Lets Build the Model for Training Set and Then Predict he Values for Testing set
t_model <- C5.0(High~. , data = training, method ="class")
summary(t_model) #Here we can see that there is an Error rate of 8.5%
t_model

pred <- predict.C5.0(t_model, newdata = testing)
pred

a<- table(testing$High, pred)
Accuracy <- sum(diag(a))/sum(a)
Accuracy  #78.9%

CrossTable(testing$High, pred)

#So here we can see that there are Misclassifications, we can improve the models using Bagging and Boosting techniques
#We use For loop for bagging in order to make multiple models

acc<- c()
for (i in 1:50) #This will create 500 different models
{
  print(i)
  
  #Build  a Model
  fittree <- C5.0(High~. , data = training,method = "class", trials = 10) #Trials is a Boosting Parameter
  
  
  pred2<- predict.C5.0(fittree,testing[,-11])
  ab<- table(testing$High, pred2)
  
  #To save the Accuracy of the models
  acc<- c(acc,sum(diag(ab))/sum(ab)) 
}

summary(acc) 

summary(fittree)

plot(fittree)
#According to fittree the Variables Income, Advertising, Price, ShelveLoc and age  cause High sales to the Company.