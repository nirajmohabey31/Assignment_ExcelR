##ASSOCIATION RULE##
library(arules)
library(arulesViz)

#Loading the data

books<-read.csv("E:/Data science Excelr/Assigments/Association/book.csv")
databook<-data.frame(as.matrix(books)) #Coverting the dataset in dataframe
barplot(sapply(databook,sum),col = 1:2000) 

#RULES
options(max.print = 9999)
rules <- apriori(as.matrix(databook),parameter = list(support=0.005,confidence=0.9,minlen= 5))#907 rules done
arules::inspect(rules) # show the support, lift and confidence for all rules
plot(rules)


rules_conf <- sort (rules, by="confidence", decreasing=TRUE) # 'high-confidence' rules.
arules::inspect(rules_conf)
plot(rules_conf)# show the support, lift and confidence for all rules
#The rules with confidence of 1 imply that, whenever the LHS item was purchased,the RHS item was also purchased 100% of the time.

rules_lift <- sort (rules, by="lift", decreasing=TRUE) # 'high-lift' rules.
arules::inspect(rules_lift) # show the support, lift and confidence for all rules

#To get 'strong' rules,we increase the value of 'conf' parameter.
#To get 'longer' rules, increase 'maxlen'.

subsetRules <- which(colSums(is.subset(rules, rules)) > 1) # get subset rules in vector
length(subsetRules)  
rules <- rules[-subsetRules] # remove subset rules. 


########Rule 2##########
rules <- apriori (as.matrix(databook), parameter=list (supp=0.01,conf = 0.9,minlen=2), appearance = list (default="lhs",rhs="ChildBks"), control = list (verbose=T)) # get rules that lead to buying "childbks"
rules_conf <- sort (rules, by="confidence", decreasing=TRUE) # 'high-confidence' rules.
arules::inspect(rules_conf) 
rules_lift <- sort (rules, by="lift", decreasing=TRUE) # 'high-lift' rules.
arules::inspect(rules_lift)
#we came to know people who read youthbks,geogbks,Refbks,italsbks also read childbks

######Visualization######

library(arulesViz)
plot(rules,method = "scatterplot", jitter=0 )
plot(rules,method = "grouped")
plot(rules,method="paracoord",control=list(reorder=TRUE))
plot(rules,method = "graph")
#plot(rules,method = "mosaic")  #this plot dont work for all rules together it can visualize one rule at one time
plot(rules,method="two-key plot", jitter=0)
top4rules <- head(rules, n = 10, by = "confidence")
plot(top4rules, method = "graph",  engine = "htmlwidget") #an interactive plot


#######TRying random values of support and confidence
rules <- apriori(as.matrix(databook),parameter = list(support=0.01,confidence=0.8))#678 rules done
rules_conf <- sort (rules, by="confidence", decreasing=TRUE) # 'high-confidence' rules.
arules::inspect(rules_conf)# show the support, lift and confidence for all rules

rules <- apriori(as.matrix(databook),parameter = list(support=0.06,confidence=0.9)) #3 rules
rules_conf <- sort (rules, by="confidence", decreasing=TRUE) # 'high-confidence' rules.
arules::inspect(rules_conf)# show the support, lift and confidence for all rules


########Changing the minimum length in apriori algorithm

rules1 <- apriori (as.matrix(databook), parameter=list (supp=0.01,conf = 0.6,minlen=4,maxlen = 15), control = list (verbose=T)) #1026 Rules
rules_conf1 <- sort (rules1, by="confidence", decreasing=TRUE) # 'high-confidence' rules.
arules::inspect(rules_conf1)

plot(rules1,method = "scatterplot", jitter=0 )