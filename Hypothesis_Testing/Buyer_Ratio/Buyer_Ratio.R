#######Hypothesis testing on Buyers data#####

#Ho = All Proportion of equal
#Ha = All proportion are not equal

buy <-read.csv("E:/Data science Excelr/Assigments/Hypothesis Testing/BuyerRatio/BuyerRatio.csv")
attach(buy)

buyer <-matrix(c(50,142,131,70,435,1523,1356,750),nrow = 2,byrow=T)
buyer
row.names(buyer)<-c("Males","Females")
colnames(buyer)<-c("East","West","North","south")

show(buyer)
prop.table(buyer,2)

barplot(prop.table(buyer,2),beside = T,legend.text = row.names(buyer))

###ChiSQuare Test

chisq.test(buyer)
##p value = 0.66 which is > 0.05 so here we can conclude that we are accepting our null hypothesis and rejecting the Ha
#that means Proportion of both male female over different regions are equal