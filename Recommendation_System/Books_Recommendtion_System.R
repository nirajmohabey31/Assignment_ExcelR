library(recommenderlab)
library(data.table)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(DT)
library(reshape2)
library(grid)
library(hexbin)
library(gridExtra)
library(corrplot)
library(qgraph)
library(methods)
library(Matrix)



#Loading the dataset
bookratings_list <- fread("E:/Data science Excelr/Assigments/Recommendation System/book.csv")
attach(bookratings_list)
head(bookratings_list)

bookratings_list <- bookratings_list[,2:4]
head(bookratings_list)
dim(bookratings_list)


#Remove the duplicate ratings.
bookratings_list[, N := .N, .(User_ID,Book_Title )]
cat('Number of duplicate ratings: ', nrow(bookratings_list[N > 1]))
#Number of duplicate ratings : 14
bookratings_list <- bookratings_list[N == 1]

### keep users who rated more than 2 books
bookratings_list[, N := .N, .(User_ID)]
cat('Number of users who rated more than 2 books: ', uniqueN(bookratings_list[N <= 2, User_ID]))
## Number of users who rated more than 2 books: 1571
bookratings_list <- bookratings_list[N > 2]

#Data Exploration
## i) Distribution of ratings

bookratings_list %>% 
  ggplot(aes(x = Book_Rating, fill = factor(Book_Rating))) +
  ggtitle("Distribution of Ratings") + theme(plot.title = element_text(hjust = 0.5)) +
  geom_bar(color = "grey20") + scale_fill_brewer(palette = "Paired") + guides(fill = FALSE)

## ii) Number of ratings per user

bookratings_list %>% 
  group_by(User_ID) %>% 
  summarize(number_of_ratings_per_user = n()) %>% 
  ggplot(aes(number_of_ratings_per_user)) + 
  xlab("Ratings per User") + ylab("Rating count") +
  ggtitle("Number of ratings per user") + theme(plot.title = element_text(hjust = 0.5)) +
  geom_bar(fill = "pink2", color = "grey20") + coord_cartesian(c(5, 50))

### iii) Distribution of mean user rating

bookratings_list %>% 
  group_by(User_ID) %>% 
  summarize(mean_user_rating = mean(Book_Rating)) %>% 
  ggplot(aes(mean_user_rating)) +
  xlab("Mean of User ratings") + ylab("Rating count") +
  ggtitle("Distribution of mean user rating") + theme(plot.title = element_text(hjust = 0.5)) +
  geom_histogram(bins = 30, fill = "cyan3", color = "grey20")

## vii) Frequent raters

get_cor <- function(df){
  m <- cor(df$x,df$y, use="pairwise.complete.obs");
  eq <- substitute(italic(r) == cor, list(cor = format(m, digits = 2)))
  as.character(as.expression(eq));                 
}

tmp <- bookratings_list %>% 
  group_by(User_ID) %>% 
  summarize(mean_rating = mean(Book_Rating), number_of_rated_books = n())

tmp %>% filter(number_of_rated_books <= 100) %>% 
  ggplot(aes(number_of_rated_books, mean_rating)) + stat_bin_hex(bins = 50) + scale_fill_distiller(palette = "Spectral") + stat_smooth(method = "lm", color = "orchid", size = 2, se = FALSE) +
  annotate("text", x = 80, y = 1.9, label = get_cor(data.frame(x = tmp$number_of_rated_books, y = tmp$mean_rating)), color = "orchid", size = 7, parse = TRUE)

### Collaborative Filtering

dimension_names <- list(User_ID = sort(unique(bookratings_list$User_ID)), Book_Title = sort(unique(bookratings_list$Book_Title)))
ratingmat <- spread(select(bookratings_list, Book_Title, User_ID, Book_Rating), Book_Title, Book_Rating) %>% select(-User_ID)

ratingmat <- as.matrix(ratingmat)
dimnames(ratingmat) <- dimension_names
ratingmat[1:5, 1:5]
dim(ratingmat)

## Find similar users
# Below I do this for 2 users (user_ids: 507 and 2439 )

#507
current_user <- "507"
rated_items <- which(!is.na((as.data.frame(ratingmat[current_user, ]))))
selected_users <- names(which(apply(!is.na(ratingmat[ ,rated_items]), 1, sum) >= 1))
head(selected_users, 10)
user1 <- data.frame(item=colnames(ratingmat),rating=ratingmat["507",]) %>% filter(!is.na(rating))

#2439
current_user1 <- "2439"
rated_items1 <- which(!is.na((as.data.frame(ratingmat[current_user1, ]))))
selected_users1 <- names(which(apply(!is.na(ratingmat[ ,rated_items1]), 1, sum) >= 1))
head(selected_users1, 40)
user2 <- data.frame(item = colnames(ratingmat),rating=ratingmat["2439",]) %>% filter(!is.na(rating))

tmp <- merge(user1, user2, by="item")
tmp

# normalizing the user ratings
rmat <- ratingmat[selected_users, ]
user_mean_ratings <- rowMeans(rmat,na.rm=T)
rmat <- rmat - user_mean_ratings

similarities <- cor(t(rmat[rownames(rmat)!=current_user, ]), rmat[current_user, ], use = 'pairwise.complete.obs')
sim <- as.vector(similarities)
names(sim) <- rownames(similarities)
res <- sort(sim, decreasing = TRUE)

## Visualizing similarities

sim_mat <- cor(t(rmat), use = 'pairwise.complete.obs')
random_users <- selected_users[1:3]
qgraph(sim_mat[c(current_user, random_users), c(current_user, random_users)], layout = "spring", vsize = 5, theme = "TeamFortress", labels = c(current_user, random_users))

#USING RECOMMENDERLAB

#matrix in sparse format
ratingmat0 <- ratingmat
ratingmat0[is.na(ratingmat0)] <- 0
sparse_ratings <- as(ratingmat0, "sparseMatrix")
rm(ratingmat0)
gc()

#Recommenderlab uses as special variant of a sparse matrices, so we convert to this class first.

R <- as(sparse_ratings, "realRatingMatrix")
R

#Running an algorithm in Recommenderlab is really easy. All you have to do is call Recommender() and pass the data,
#select a method ("UBCF" - user-based collaborative filtering) 
#Used based Collaborative Filtering

model <- Recommender(R, method = "UBCF", param = list(method = "pearson", nn = 4))

## create n recommendations for a user based on his ratings
uid = "507"
books <- subset(bookratings_list, bookratings_list$User_ID==uid)
print("You have rated:")
books

#Prediction For User
print("Recommendations for you:")
prediction <- predict(model, R[uid], n=5) 
as(prediction, "list")

#So we created a recommendation system in which based on the rating to a particular book  we get the recommendation for other 5 books.

#Different techniques

#Popularity based 

model1 <- Recommender(R, method="POPULAR")

#Predictions for user
recommended_items1 <- predict(model1, R[uid], n=5)
as(recommended_items1, "list")

#SVD 

model2 <- Recommender(R, method="SVD")

#Predictions for user
recommended_items2 <- predict(model2, R[uid], n=5)
as(recommended_items2, "list")