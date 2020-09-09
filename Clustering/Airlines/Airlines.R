#Clustering Algorithm
#We will DO Heirarchical as well as K-Means Clustering

#Perform clustering (Both hierarchical and K means clustering) for the airlines data to obtain optimum number of clusters. 
#Draw the inferences from the clusters obtained.

#Data Description:

# The file EastWestAirlinescontains information on passengers who belong to an airline's frequent flier program. For each passenger the data include information on their mileage history and on different ways they accrued or spent miles in the last year. The goal is to try to identify clusters of passengers that have similar characteristics for the purpose of targeting different segments for different types of mileage offers

#Use the Below Library to import Excel Files 
install.packages("xlsx")
Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jre1.8.0_251') #if you get the pkg (rjava) error 
library(rJava)

library(xlsx)
airlines <- read.xlsx("E:\\Data science Excelr\\Assigments\\Clustering\\EastWestAirlines.xlsx",2)
summary(airlines)

###EDA##
#Standard Deviation
sd(airlines$Balance)
sd(airlines$Qual_miles)
sd(airlines$cc1_miles)
sd(airlines$cc2_miles)
sd(airlines$cc3_miles)
sd(airlines$Bonus_miles)
sd(airlines$Bonus_trans)
sd(airlines$Flight_miles_12mo)
sd(airlines$Flight_trans_12)
sd(airlines$Days_since_enroll)
sd(airlines$Award.)


#Variance
var(airlines$Balance)
var(airlines$Qual_miles)
var(airlines$cc1_miles)
var(airlines$cc2_miles)
var(airlines$cc3_miles)
var(airlines$Bonus_miles)
var(airlines$Bonus_trans)
var(airlines$Flight_miles_12mo)
var(airlines$Flight_trans_12)
var(airlines$Days_since_enroll)
var(airlines$Award.)

#correlation
cor(airlines[,-1])

airlines1 <- (scale(airlines[2:12])) #WE need to scale the data to normalize it so that its easy to calculate distance between them
range(airlines1)#Gives the range of our scaled data

d <- dist(airlines1, method = "euclidean") #Distance between data point is found through various methods(i used euclidean), distance contributes to the further process.
d
options(max.print=999999) #without this function i got a warning as  [ reached getOption("max.print") -- omitted 29 rows ] where only 21 rows output was shown and my data set had more rows than it

#Using Centroid method of hierarchical clustering
hierarchical_centroid <- hclust(d, method = "centroid") # Doing hierarchical clustering using centroid method which gives the distance between center points clusters
plot(hierarchical_centroid)
clusterGroup <- cutree(hierarchical_centroid,10)
rect.hclust(hierarchical_centroid,5,border = "blue")
hierarchical_centroid_Data <- data.frame(airlines[,1],clusterGroup) #dataframe about which airlineid is in which cluster number
plot(hierarchical_centroid_Data)

table(clusterGroup) #To see the Number of Customers in Each Group

#Using Average Linkage Method of hierarchical clustering
Hierarchical_Average <- hclust(d, method = "average") #average distance
plot(Hierarchical_Average)
clustergroup <- cutree(Hierarchical_Average, 10)
rect.hclust(Hierarchical_Average, 8,border = "Blue")
Hierarchical_Average_data <- data.frame(airlines[,1], clustergroup)#dataframe about which airlineid is in which cluster number
plot(Hierarchical_Average_data)

table(clustergroup)


#Using Complete Linkage Method of hierarchical clustering
Hierarchical_Complete <- hclust(d, method = "complete") #maximum distance
plot(Hierarchical_Complete)
clustergroup1<- cutree(Hierarchical_Complete, 10)
rect.hclust(Hierarchical_Complete, 10,border = "Blue")
Hierarchical_Complete_data <- data.frame(airlines[,1], clustergroup)#dataframe about which airlineid is in which cluster number
plot(Hierarchical_Complete_data)

#Using single Linkage Method of hierarchical clustering
Hierarchical_single <- hclust(d, method = "single") #minimum distance
plot(Hierarchical_single)
clustergroup2 <- cutree(Hierarchical_single, 10)
rect.hclust(Hierarchical_single, 5,border = "Blue")
Hierarchical_single_data <- data.frame(airlines[,1], clustergroup)#dataframe about which airlineid is in which cluster number
plot(Hierarchical_single_data)

#Now lets do K-Means Clustering on the data

wss <- c()
for (i in 2:15) wss[i]<- sum(kmeans(airlines1, centers = i)$withinss)
plot(1:15,wss,type = "b", xlab = "No of Clusters", ylab = "Avg Distance")

#Using the Elbow Plot we got the k value as 5

k_means_airline <- kmeans(d,5)

#If you want to See the Animated view of the Clusters then run the below Statements
install.packages("animation")
library(animation)
windows()
k_means_airline <- kmeans.ani(d,5)

k_means_airline_clusters <- data.frame(airlines[,1], k_means_airline$cluster)

############### Lets Perform Different Distance  Methods on the data########

d.manhat <- dist(airlines1, method = "manhattan")
d.manhat
library(ggplot2)
library(factoextra)

d.pearson <- get_dist(airlines1, method = "pearson")
d.pearson

d.kendall <- get_dist(airlines1, method = "kendall")
d.kendall

d.spearman <- get_dist(airlines1, method = "spearman")
d.spearman

##### Lets perform various Clustering using these distances ####

sing.clust <- hclust(d.manhat, method = "single") #Single Linkage Method
fviz_dend(sing.clust)
sing.clust.cuttree <- cutree(sing.clust, k=4)
sing.clust.data <- data.frame(airlines[,1],"cluster"=sing.clust.cuttree)
sing.clust.data

comp.clust <- hclust(d.manhat, method = "complete") #Complete Linkage Method
fviz_dend(comp.clust)
comp.cuttree <- cutree(comp.clust, k=5)
comp.clust.data <- data.frame(airlines[,1],"cluster"=comp.cuttree)
comp.clust.data

#For Density Based Clustering
install.packages("fpc")
install.packages("dbscan")
library(fpc)
library(dbscan)

#To determine the eps value: dbscan::kNNdistplot(df, k =  5)
#abline(h = 0.15, lty = 2)

dens.clust <- dbscan(d.pearson, minPts = 5, eps = 0.15)
fviz_cluster(dens.clust,data = airlines1, palette ="jco", geom = "point", ggtheme = theme_classic())
dens.clust.data <- data.frame(airlines[,1], "cluster"=dens.clust$cluster)
dens.clust.data
#Cluster 0 corresponds to Outliers

#Model Based Cluster

library(mclust)
model.based <- Mclust(d.pearson)
summary(model.based)

model.based$modelName #Returns the name of the model
model.based$G #Returns the total number of Clusters

fviz_mclust(model.based, "BIC",  palette = "jco")
fviz_mclust(model.based, "classification", geom = "point",palette="jco")
fviz_mclust(model.based,"uncertainty", palette = "jco")

#Fuzzy Clustering

library(cluster)

# fanny(x, k, metric = "euclidean", stand = FALSE)
# x: A data matrix or data frame or dissimilarity matrix
# k: The desired number of clusters to be generated
# metric: Metric for calculating dissimilarities between observations
# stand: If TRUE, variables are standardized before calculating the dissimilarities

fuz <- fanny(airlines1, 3) #Fuzzy Cluster
fuz$clustering #Returns the Cluster for each value
fuz$membership #Returns the membership Coefficient for each value
fviz_cluster(fuz, ellipse.type = "norm", repel = TRUE,palette = "jco", ggtheme = theme_minimal(),legend = "right")
fuz.data <- data.frame(airlines[,1], "cluster"=fuz$clustering)
fuz.data


#Partitioning around Medoids (PAM) Also Called K-Medoids Algorithm for Clustering

#library("cluster","factoextra")
pammodel <- pam(airlines1,3, metric = "manhattan",stand = FALSE)
pammodel$medoids
pammodel$clustering
fviz_cluster(pammodel, palette="jco", repel = TRUE, ggtheme = theme_classic(), legend = "right")
pammodel.data <- data.frame(airlines[,1], "Cluster"=pammodel$clustering)
pammodel.data