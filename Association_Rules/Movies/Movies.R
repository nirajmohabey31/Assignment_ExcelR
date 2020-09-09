##ASSOCIATION RULE FOR MOVIES


install.packages("rmarkdown")
library(rmarkdown)
library(arules)
library(arulesViz)

movies <- read.csv("E:/Data science Excelr/Assigments/Association/my_movies.csv")

rules <- apriori(as.matrix(movies[,6:15],parameter=list(support=0.2, confidence = 0.5,minlen=4)))#77 rules 

rules_conf <- sort (rules, by="confidence", decreasing=TRUE) # 'high-confidence' rules as decreasing=True if False then low confidence rule
arules::inspect(rules_conf) #Shows Confidence , support and lift 

rules_lift <- sort (rules, by="lift", decreasing=TRUE) # 'high-lift' rules rules as decreasing=True if False then low lift rule
arules::inspect(rules_lift) #Shows Confidence , support and lift 
head(quality(rules))

##Lets find for Harrypotter
rules1 <- apriori (as.matrix(movies[,6:15]), parameter=list (supp=0.006,conf = 0.08), appearance = list (default="lhs",rhs="Harry.Potter1"), control = list (verbose=T))#16 Rules #verbose indicates that to show the info about the rules or not if "T" it shows if "F" it doesnt show
rules_conf1 <- sort (rules1, by="confidence", decreasing=TRUE) # 'high-confidence' rules.
arules::inspect(rules_conf1)
plot(rules1,method = "grouped")
#WE came to know those who watch sixth sense, LOTR1 definately watch Harrypotter

#######VIZUALIZATION#####
plot(rules,method = "scatterplot")
plot(rules, method = "grouped")
# It looks ike most of them has wateched Lord of the rings movies along with gladiator and greenmiles
# Also most of them has watched Gladiator, Sixth sense along with Patrioit
# Patriot ,Braveheart and other three items along with Gladiator. 
plot(rules,method = "graph")
plot(rules,method="paracoord",control=list(reorder=TRUE))  #this takes a while
plot(rules,method="two-key plot", jitter=0)

toprules <- head(rules, n = 20, by = "confidence")
plot(toprules, method = "graph",  engine = "htmlwidget")


#changing values of support and confidence and minlen
rules2 <- apriori(as.matrix(movies[,6:15],parameter=list(support=0.5, confidence = 0.8,minlen=5)))
rules2 # 77 rules 
plot(rules2,method = "scatterplot")
plot(rules2,method = "grouped")
plot(rules2,method = "graph")


rules3 <- apriori(as.matrix(movies[,6:15],parameter=list(support=0.8, confidence = 1,minlen=4)))
rules3

plot(rules3,method = "scatterplot")
plot(rules3,method = "grouped")
plot(rules3,method = "graph")