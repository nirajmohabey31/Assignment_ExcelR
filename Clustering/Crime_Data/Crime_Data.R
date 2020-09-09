#Clustering for Crime Data

#Perform Clustering for the crime data and identify the number of clusters formed and draw inferences.


#Lets Do Heirarchical Clustering
crime <- read.csv("E:/Data science Excelr/Assigments/Clustering/crime_data.csv") 
summary(crime)

#EDA
#Standard Deviation
sd(crime$Murder)
sd(crime$Assault)
sd(crime$UrbanPop)
sd(crime$Rape)

#Variance
var(crime$Murder)
var(crime$Assault)
var(crime$UrbanPop)
var(crime$Rape)

#correlation
cor(crime[,-1])

crime1<- scale(crime[,2:5]) #WE need to scale the data to normalize it so that its easy to calculate distance between them
range(crime1)#Gives the range of our scaled data

d<-dist(crim e1, method = "euclidean") #Distance between data point is found through various methods(i used euclidean), distance contributes to the further process. 
d

#Lets do it Using Centroid Linkage Method in hierarchical clustering
cluster1 <- hclust(d, method = "centroid") # Doing hierarchical clustering using centroid method which gives the distance between center points clusters
plot(cluster1) 

tree1 <- cutree(cluster1,k= 4)
tree1

rect.hclust(cluster1, k=4, border = 2:6) #to superimpose rectangular compartments for each cluster on the tree with the rect.hclust() function
abline(h = 4, col = 'red') #we visually want to see the clusters on the dendrogram you can use R's abline() function to draw the cut line

#suppressPackageStartupMessages(library(dendextend)) # we can also use the color_branches() function from the dendextend library to visualize your tree with different colored branches.
#complete_dend_cluster1 <- as.dendrogram(cluster1)
#complete_col_dend_cluster1 <- color_branches(cluster1, k = 4)
#plot(complete_col_dend_cluster1)

groups <- data.frame("City"=crime[,1],"Cluster Number"=tree1) #dataframe about which city is in which cluster number

#Lets Use Average Linkage Method

cluster2 <- hclust(d, method = "average")
plot(cluster2)

tree2 <- cutree(cluster2, k=4)

rect.hclust(cluster2,k=4, border="blue")

group2 <- data.frame("City"=crime[,1], "cluster number"= tree2)#dataframe about which city is in which cluster number
group2

#Lets Use Complete Linkage Method

cluster3 <- hclust(d, method = "complete")  #maximum distance
plot(cluster3)

tree3 <- cutree(cluster3, k=4)

rect.hclust(cluster3,k=4, border=2:6)

group3 <- data.frame("City"=crime[,1], "cluster number"= tree3)
group3

#Lets Use Single Linkage Method

cluster4 <- hclust(d, method = "single")  #minimum distance
plot(cluster4)

tree4 <- cutree(cluster4, k=5)

rect.hclust(cluster4,k=4, border=2:6)

group4 <- data.frame("City"=crime[,1], "cluster number"= tree4)
group4


#Lets Do the Clustering With KMeans
#Lets Find the K value With the help of Elbow Plot
wss <- c()
for (i in 2:15) wss[i]<- sum(kmeans(d, centers = i)$withinss)
plot(1:15,wss,type = "b", xlab = "No of Clusters", ylab = "Avg Distance")

#Lets take the Value of K as 3 as obtained from Elbow Plot

k_mean_cluster <- kmeans(d,3)
k_mean_cluster$centers
k_mean_cluster$cluster
print(k_mean_cluster)

aggregate(crime1, by=list(cluster=k_mean_cluster$cluster), mean) #we compute the mean of each variables by clusters using the original data
dd <- cbind(crime1, cluster5 = k_mean_cluster$cluster)   #add the point classifications to the original data
head(dd)
plot(dd)

k_mean_cluster$cluster  # Cluster number for each of the observations
#head(k_mean_cluster$cluster, 50)

k_mean_cluster$size  #cluster size

k_mean_cluster$centers  # Cluster means


final_Cluster_info <- data.frame("City"=crime[,1], "Cluster"=k_mean_cluster$cluster)


############### Lets Perform Different Distance  Methods on the data########

d.manhat <- dist(crime1, method = "manhattan")
d.manhat

install.packages("factoextra")
library(ggplot2)
library(factoextra)   #Provides some easy-to-use functions to extract and visualize the output of multivariate data analysis

d.pearson <- get_dist(crime1, method = "pearson") #get_dist is used here to find distance and is different than manhattan
d.pearson

d.kendall <- get_dist(crime1, method = "kendall")
d.kendall

d.spearman <- get_dist(crime1, method = "spearman")
d.spearman

##### Lets perform various Clustering methods using these distances ####

sing.clust <- hclust(d.manhat, method = "single") #Single Linkage Method
fviz_dend(sing.clust)  #Gives dendogram for cluster
sing.clust.cuttree <- cutree(sing.clust, k=4)
sing.clust.data <- data.frame(crime[,1],"cluster"=sing.clust.cuttree) #dataframe about which city is in which cluster number
sing.clust.data

comp.clust <- hclust(d.manhat, method = "complete") #Complete Linkage Method
fviz_dend(comp.clust)
comp.cuttree <- cutree(comp.clust, k=5) #This cut the tree into 5 clusters
comp.clust.data <- data.frame(crime[,1],"cluster"=comp.cuttree)
comp.clust.data

#For Density Based Clustering
install.packages("fpc")
install.packages("dbscan")
library(fpc) #fpc:Flexible Procedures for Clustering
library(dbscan)  #dbscan:density-based spatial clustering of applications with noise

#To determine the eps value: dbscan::kNNdistplot(df, k =  5)
#abline(h = 0.15, lty = 2)

dens.clust <- dbscan(d.pearson, minPts = 5, eps = 0.15) #eps=epsillon~error
fviz_cluster(dens.clust,data = crime1, palette ="jco", geom = "point", ggtheme = theme_classic())
dens.clust.data <- data.frame(crime[,1], "cluster"=dens.clust$cluster)
dens.clust.data
#Cluster 0 corresponds to Outliers

#Model Based Cluster# model cluster here automatically selects the best model from all
install.packages("mclust")
library(mclust)
model.based <- Mclust(d.pearson)
summary(model.based)

model.based$modelName #Returns the name of the model
model.based$G #Returns the total number of Clusters

fviz_mclust(model.based, "BIC",  palette = "jco") #It select the best model itself here it is #VII #BIC:Bayesian Information Criterion

fviz_mclust(model.based, "classification", geom = "point",palette="jco")
fviz_mclust(model.based,"uncertainty", palette = "jco")

#Fuzzy Clustering
install.packages(cluster)
library(cluster)

# fanny(x, k, metric = "euclidean", stand = FALSE)
# x: A data matrix or data frame or dissimilarity matrix
# k: The desired number of clusters to be generated
# metric: Metric for calculating dissimilarities between observations
# stand: If TRUE, variables are standardized before calculating the dissimilarities

fuz <- fanny(crime1, 3) #Fuzzy Cluster #3 indicate no of clusters to be formed
fuz$clustering #Returns the Cluster for each value
fuz$membership #Returns the membership Coefficient for each value
fviz_cluster(fuz, ellipse.type = "norm", repel = TRUE,palette = "jco", ggtheme = theme_minimal(),legend = "right")
fuz.data <- data.frame(crime[,1], "cluster"=fuz$clustering)
fuz.data


#Partitioning around Medoids (PAM) Also Called K-Medoids Algorithm for Clustering

#library("cluster","factoextra")
pammodel <- pam(crime1,3, metric = "manhattan",stand = FALSE)
pammodel$medoids
pammodel$clustering
fviz_cluster(pammodel, palette="jco", repel = TRUE, ggtheme = theme_classic(), legend = "right")  #Repel=T,helps us to view the points clearly without overlapping
pammodel.data <- data.frame(crime[,1], "Cluster"=pammodel$clustering)
pammodel.data