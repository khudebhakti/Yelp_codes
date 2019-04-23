setwd("D:/bhakti/yelp/7th_Challenge/yelp_dataset_challenge_academic_dataset/Restaurant_Shopping")
library(RMySQL)
# sorting only Shopping and Restaurants data

mydb2 = dbConnect(MySQL(), user='root', password='root', host='localhost')
#dbSendQuery(mydb, "CREATE DATABASE food_shop;")
dbSendQuery(mydb2, "USE food_shop")
mydb2 = dbConnect(MySQL(), user='root', password='root', host='localhost', dbname="food_shop")

#two category examples
business <- dbGetQuery(mydb2,"select * from business_mod")       #business which are open
b <- dbGetQuery(mydb2,"select * from business_mod2")             #factors to numeric data
review <- dbGetQuery(mydb2,"select * from review_sort")
tip <- dbGetQuery(mydb2,"select * from tip_sort")
user <- dbGetQuery(mydb2,"select * from user_2")

#########################
a <- which(user$user_id %in% (review$user_id))
b <- which(user$user_id %in% (tip$user_id))
unique(c(a[1:10],b[1:10]))
#########################

#binding users' Friends data whos given review
a <- Sys.time()
review_user1 <- dbGetQuery(mydb2,"SELECT review_sort.*, user_2.friends, user_2.nFriends, user_2.nElite
                                  FROM review_sort
                                  INNER JOIN user_2
                                  ON review_sort.user_id = user_2.user_id") 

b <- Sys.time()
dbWriteTable(mydb2,"Review_With_User2",review_user1,row.names=F,overwrite=T)

b-a
#########################################

review_user <- dbGetQuery(mydb2,"select * from review_with_user")
#UF <- c()

# for(i in 1:nrow(review_user))
# {
#   f <- unlist(strsplit(review_user$friends[i],split = ","))
#   if(length(f1) != 0) #if user has no friends
#   {
#     
#   }
#   
# }
#################################################################################################
# http://stackoverflow.com/questions/13773770/split-comma-separated-column-into-separate-rows
#################################################################################################
s <- strsplit(as.character(review_user$friends), ', ')
UF <- data.frame(user=rep(review_user$user_id, sapply(s, FUN=length)), friends=unlist(s))
#colnames(UF) <- c("User","Friends")

all_user <- c(review_user$user_id,unlist(strsplit(review_user$friends,split=", ")))
all_user <- factor(all_user)


# y <- c(as.character(UF$user), as.character(UF$friends))
# y <- factor(y)
# y <- as.numeric(y)

#UF$user <- sapply(UF$user, function(x){which(levels(all_user)==x)})
#UF$friends <- sapply(UF$friends, function(x){which(levels(all_user)==x)})

library(igraph)
uf2 <- as.data.frame(UF)
uf2 <- graph.data.frame(uf2,directed = F)
review_user$degree <- rep(0,nrow(review_user))
a <- which(review_user$user_id %in% V(uf2)$name)
review_user$degree[a] <- degree(uf2)[a]


g <- betweenness(uf2)
review_user$betweenness <- rep(0,nrow(review_user))
review_user$betweenness[a] <- g[1:length(a)]
g1 <- closeness(uf2)
review_user$closeness <- rep(0,nrow(review_user))
review_user$closeness[a] <- g1[1:length(a)]
g2 <- evcent(uf2)
g2 <- g2$vector
review_user$evcent <- rep(0,nrow(review_user))
review_user$evcent[a] <- g2[1:length(a)]
dbWriteTable(mydb2,"review_with_user",review_user,row.names=F,overwrite=F)
###################################################

review_user <- dbGetQuery(mydb2,"select * from review_With_user")
