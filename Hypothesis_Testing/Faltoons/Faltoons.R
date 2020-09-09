###Hypothesis Testing for Fantaloons data ###

#Ho = percentage of male and female are same
#Ha = percentage of male and female are not same

fantaloon<-read.csv("E:/Data science Excelr/Assigments/Hypothesis Testing/Faltoons/Faltoons.csv")
attach(fantaloon)
str(fantaloon)

###Coverting them as factor as they are in chr

fantaloon$Weekdays<-as.factor(fantaloon$Weekdays)
fantaloon$Weekend<-as.factor(fantaloon$Weekend)

summary(fantaloon) #summary to check values

fanta <- data.frame("Weekdays"=c(287,113), "Weekend" = c(233,167))
row.names(fanta) <- c("Female","Male") 

show(fanta)

prop.table(fanta)

###CHi Square test###

chisq.test(fanta)
##We can see the P value is less than 0.05 so we reject our null hypothesis and accept Ha 
#.i.e The percentage of male and female are not same