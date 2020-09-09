#####Hypothesis testing for Lab TAT ##

#Ho = Their is no difference in avergae TAT
#Ha = Their is a difference in average TAT

lab<-read.csv("E:/Data science Excelr/Assigments/Hypothesis Testing/Lab TAT/LabTAT.csv")
attach(lab)
summary(lab)
boxplot(lab)

#############Normality test###############

shapiro.test(Laboratory.1) ##p value > 0.05 that means normal distribution

shapiro.test(Laboratory.2) ##p value > 0.05 that means normal distribution

shapiro.test(Laboratory.3) ##p value > 0.05 that means normal distribution

shapiro.test(Laboratory.4) ##p value > 0.05 that means normal distribution

##############Variance Test######
var.test(Laboratory.1,Laboratory.2)
var.test(Laboratory.3,Laboratory.4)
###p value > 0.05 means Equal variances

#######ANOvA Test(we are using anova because the data set contains more than 2 values)
lab1<-stack(lab)  ##Assigning all the columns into a Stack
anova_lab<-aov(lab1$values~.,data = lab1)
anova_lab
summary(anova_lab)
# We saw here that the p value <0.05 so can conclude that we reject Ho and accept Ha
#i.e The Average TAT of different laboratories are different