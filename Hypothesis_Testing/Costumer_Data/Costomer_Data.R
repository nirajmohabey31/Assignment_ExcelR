#######Hypothesis testing on telecall data#####

# Ho = Defective % doesnt varies accross all  regions
# Ha = Defective % varies accross all regions

telecal <- read.csv(file.choose())
summary(telecal)
str(telecal)

#converting all the variables as factor
telecal$Phillippines<- as.factor(telecal$Phillippines)
telecal$Indonesia <- as.factor(telecal$Indonesia)
telecal$Malta <- as.factor(telecal$Malta)
telecal$India <- as.factor(telecal$India)
str(telecal)
summary(telecal)

##creating a data frame for the test
tele_table<-data.frame(Phillippines = c(29,271), Indonesia = c(33,267), Malta = c(31,269), India = c(20,280))
row.names(tele_table) <- c("Defective", "Error Free")
tele_table

prop.table(tele_table)

## Now lets perform the chi square test

chisq.test(tele_table)
##we found our output as X-squared = 3.859, p-value = 0.2771 where p value is >0.05
######so here then we can conclude that we are accepting our null hypothesis and rejecting the Ha alternative hypothesis
##i.e Defective % is same over all regions