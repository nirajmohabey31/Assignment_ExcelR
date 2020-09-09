library(arules)

groceries<-read.transactions("E:/Data science Excelr/Assigments/Association/groceries.csv",format="basket")
class(groceries)
arules::inspect(groceries[1:10]) #To view the transactions, use the inspect() function


size(head(groceries)) # number of items in each observation
LIST(head(groceries, 3)) # convert 'transactions' to a list, note the LIST in CAPS


options(max.print=999999) #without this function i got a warning that only 21 rows output was shown and my data set had more rows than it


frequentItem <- eclat (groceries, parameter = list(supp = 0.002, maxlen = 5)) # calculates support for frequent items
inspect(frequentItems)
#eclat() takes in a transactions object and gives the most frequent items in the data 
#maxlen defines the maximum number of items in each itemset of frequent items


itemFrequencyPlot(groceries, topN=10, type="absolute", main="Item Frequency")  #plot frequent items,itemFrequencyPlot can be applicable only for transaction data 


###RULE 1

rules <- apriori (groceries, parameter = list(supp = 0.001, conf = 0.5)) # Min Support as 0.001, confidence as 0.8.
rules_conf <- sort (rules, by="confidence", decreasing=TRUE) # 'high-confidence' rules.
arules::inspect(rules_conf)# show the support, lift and confidence for all rules
#The rules with confidence of 1 imply that, whenever the LHS item was purchased,the RHS item was also purchased 100% of the time.

rules_lift <- sort (rules, by="lift", decreasing=TRUE) # 'high-lift' rules if decreasing = F then low-lift rule 
arules::inspect(rules_lift) # it show the support, lift and confidence for all rules
#A rule with a lift of 182 imply that, the items in LHS and RHS are 182 times more likely to be purchased together compared to the purchases when they are assumed to be unrelated.

#To get 'strong' rules,we increase the value of 'conf' parameter.
#To get 'longer' rules and less rules, increase 'maxlen'.

subsetRules <- which(colSums(is.subset(rules, rules)) > 1) # get subset rules in vector
length(subsetRules)  
rules <- rules[-subsetRules] # remove subset rules. 

###RULE2

rules1 <- apriori (data=groceries, parameter=list (supp=0.001,conf = 0.08), appearance = list (default="lhs",rhs="bags"), control = list (verbose=F)) #verbose indicates that to show the info about the rules or not if "T" it shows if "F" it doesnt show
rules_conf1 <- sort (rules1, by="confidence", decreasing=TRUE) # 'high-confidence' rules.
arules::inspect(rules_conf1)
#we found out customers had purchased vegetables before buying 'bags'. This will help us understand the patterns of purchase of 'bags'

###RULE3

rules2 <- apriori (data=groceries, parameter=list (supp=0.001,conf = 0.08), appearance = list(default="rhs",lhs="vegetables"), control = list (verbose=F)) 
rules_conf2 <- sort (rules2, by="confidence", decreasing=TRUE) # 'high-confidence' rules.
arules::inspect(rules_conf2)

#we will check for cheese 

rules3 <- apriori (data=groceries, parameter=list (supp=0.001,conf = 0.08), appearance = list(default="lhs",rhs="cheese"), control = list (verbose=T)) # those who bought 'cheese' also bought..
rules_conf3 <- sort (rules3, by="confidence", decreasing=TRUE) # 'high-confidence' rules.
arules::inspect(rules_conf3)

######Visualization######

library(arulesViz)
plot(rules,method = "scatterplot", jitter=0 )
plot(rules,method = "grouped")
plot(rules,method="paracoord",control=list(reorder=TRUE))
plot(rules,method = "graph")
#plot(rules,method = "mosaic")  #this plot will only work for one rule at one time
plot(rules,method="two-key plot", jitter=0)
top4rules <- head(rules, n = 10, by = "confidence")
plot(top4rules, method = "graph",  engine = "htmlwidget") #an interactive plot


###Trying different values of support and confidence 

rules4 <- apriori (groceries, parameter = list(supp = 0.05, conf = 0.6))
rules_conf4 <- sort (rules4, by="confidence", decreasing=TRUE) # 'high-confidence' rules.
arules::inspect(rules_conf4)

rules5 <- apriori (groceries, parameter = list(supp = 0.06, conf = 1))
rules_conf5 <- sort (rules5, by="confidence", decreasing=TRUE) # 'high-confidence' rules.
arules::inspect(rules_conf5)

plot(rules4,method = "scatterplot", jitter=0 )
plot(rules5,method = "grouped")
plot(rules5,method="paracoord",control=list(reorder=TRUE))

####Changing the minimum length in apriori algorithm

rules6 <- apriori (data=groceries, parameter=list (supp=0.001,conf = 0.15,minlen=5,maxlen = 10), appearance = list(default="rhs",lhs="canned"), control = list (verbose=F)) # those who bought 'vegetables' also bought..
rules_conf6 <- sort (rules6, by="confidence", decreasing=TRUE) # 'high-confidence' rules.
arules::inspect(rules_conf6)

plot(rules6,method = "scatterplot", jitter=0 )
plot(rules6,method="paracoord",control=list(reorder=TRUE))