#####Hypothesis Testing for Cutlets dataset#####

#Ho = The diameter of cutlet of both units are same
#Ha = The diameter of cutlet of both units are not same

##Loading the dataset
cutlet<-read.csv("E:/Data science Excelr/Assigments/Hypothesis Testing/cutlet data/Cutlets.csv")
attach(cutlet)
summary(cutlet)
boxplot(cutlet)

#############Normality test###############

shapiro.test(Unit.A) ##p value > 0.05 that means normal distribution

shapiro.test(Unit.B) ##p value > 0.05 that means normal distribution

##############Variance Test######
var.test(Unit.A,Unit.B)
###p value > 0.05 means Equal variances

##### we are using two sample test because data set is numeric####
t.test(Unit.A,Unit.B,alternative = "two.sided",var.equal = T,conf.level = 0.95)
##p value = 0.32 
####p value > 0.05 so we accept Ho and we can say that the diameter of cutlet of both units are same