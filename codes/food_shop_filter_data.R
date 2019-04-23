setwd("D:/bhakti/yelp/7th_Challenge/yelp_dataset_challenge_academic_dataset/Restaurant_Shopping")
library(RMySQL)
# sorting only Shopping and Restaurants data

mydb = dbConnect(MySQL(), user='root', password='root', host='localhost')
#dbSendQuery(mydb2, "CREATE DATABASE yelp;")
dbSendQuery(mydb, "USE yelp")
mydb = dbConnect(MySQL(), user='root', password='root', host='localhost', dbname="yelp")

mydb2 = dbConnect(MySQL(), user='root', password='root', host='localhost')
#dbSendQuery(mydb, "CREATE DATABASE food_shop;")
dbSendQuery(mydb2, "USE food_shop")
mydb2 = dbConnect(MySQL(), user='root', password='root', host='localhost', dbname="food_shop")

#########################################################
business2 <- read.csv("Business2.csv",stringsAsFactors = F)
#or
business2 <- dbGetQuery(mydb,"SELECT * from yelp.business2")
row <- c()
bus.cat <- c("Shopping","Food")
for(i in 1:nrow(business2))
{
  a <- which(bus.cat %in% unlist(strsplit(business2$categories[i],split=", ")))
  if(length(a)>0){row <- c(row,i)}
}

business2 <- business2[row,]
#write.csv(business2,"business_2.csv",row.names = F)
dbWriteTable(mydb2,"business_2",business2,row.names=F,overwrite=T)

#review data filtering based on business_id
row <- which(t(dbGetQuery(mydb,"select business_id from yelp.review")) %in% 
               t(dbGetQuery(mydb2,"select business_id from food_shop.business_2")))

review_2 <- dbGetQuery(mydb,"select * from yelp.review")[row,]
dbWriteTable(mydb2,"review_2",review_2,row.names=F,overwrite=T)

row <- which(t(dbGetQuery(mydb,"select business_id from yelp.tip")) %in% 
               t(dbGetQuery(mydb2,"select business_id from food_shop.business_2")))
tip_2 <- dbGetQuery(mydb,"select * from yelp.tip")[row,]
dbWriteTable(mydb2,"tip_2",tip_2,row.names=F,overwrite=T)
write.csv(tip_2,"tip_2.csv",row.names=F)

row <- which(t(dbGetQuery(mydb,"select user_id from yelp.user2")) %in% 
               t(dbGetQuery(mydb2,"select user_id from food_shop.review_2")))
user_2 <- dbGetQuery(mydb,"select * from yelp.user2")[row,]
dbWriteTable(mydb2,"user_2",user_2,row.names=F,overwrite=T)
write.csv(user_2,"user_2.csv",row.names=F)

row <- which(t(dbGetQuery(mydb,"select business_id from yelp.checkin")) %in% 
               t(dbGetQuery(mydb2,"select business_id from food_shop.business_2")))
checkin_2 <- dbGetQuery(mydb,"select * from yelp.checkin")[row,]
dbWriteTable(mydb2,"checkin_2",checkin_2,row.names=F,overwrite=T)
write.csv(checkin_2,"checkin_2.csv",row.names=F)
######################################################

#eliminating other categories 
business <- dbGetQuery(mydb2,"select * from food_shop.business_2")
bus.cat <- c("Shopping","Food")
count <- 0
for(i in 1:nrow(business))
{
  a <- which(bus.cat %in% unlist(strsplit(business$categories[i],split=", ")))
    if(length(a)==2)
    {
      #cat(sprintf("i= %d, length(a)= %d\n",i,length(a)))
      count <- count +1
    }
  business$new_category[i] <- toString(bus.cat[c(a)])
}

#removing ambiguity examples (having both Shopping, Food as categories)
s <- sapply((business$new_category),function(x){length(unlist(strsplit(x,split=', ')))})
s <- which(s==2)
business1 <- business[-s,]
id <- business$business_id[s]   #extracting ids of double cat egs

#business   <- dbGetQuery(mydb2,"select * from food_shop.business_2")
review     <- dbGetQuery(mydb2,"select * from food_shop.review_2")
tip        <- dbGetQuery(mydb2,"select * from food_shop.tip_2")
user       <- dbGetQuery(mydb2,"select * from food_shop.user_2")
checkin    <- dbGetQuery(mydb2,"select * from food_shop.checkin_2")

checkin1 <- checkin[-(which(checkin$business_id %in% id)),]
tip1     <- tip[-(which(tip$business_id %in% id)),]
review1  <- review[-(which(review$business_id %in% id)),]
#user_id <- review$user_id[(which(review$business_id %in% id))]
########################################################################

#adding category to every file 'Business, Review, Tip, Checkin'

# for(i in 1:nrow(review1))
# {
#   print(i)
#   c <- which(business1$business_id %in% review1$business_id[i])
#   review1$category[i] <- business1$business_id[c]
# }

dbWriteTable(mydb2,"business_3",business1,row.names=F,overwrite=T)   #no 2 category examples
dbWriteTable(mydb2,"review_3",review1,row.names=F,overwrite=T)
dbWriteTable(mydb2,"tip_3",tip1,row.names=F,overwrite=T)
dbWriteTable(mydb2,"checkin_3",checkin1,row.names=F,overwrite=T)

dbSendQuery(mydb2,"CREATE TABLE review_3
                  select food_shop.review_2.*, food_shop.business_3.new_category
                  FROM food_shop.review_2
                  INNER JOIN food_shop.business_3
                  ON food_shop.review_2.business_id=food_shop.business_3.business_id;")

dbSendQuery(mydb2,"CREATE TABLE tip_3
                   select food_shop.tip_2.*, food_shop.business_3.new_category
                   FROM food_shop.tip_2
                   INNER JOIN food_shop.business_3
                   ON food_shop.tip_2.business_id=food_shop.business_3.business_id;")

c <- dbGetQuery(mydb2,"select food_shop.review_3.*, food_shop.business_3.new_category
                       FROM food_shop.review_3
                       INNER JOIN food_shop.business_3
                       ON food_shop.review_3.business_id=food_shop.business_3.business_id;")
dbWriteTable(mydb2,"review_3",c,overwrite=T,row.names=F)
################################################

review_3 <- dbGetQuery(mydb2,"select * from food_shop.review_3")
tip_3 <- dbGetQuery(mydb2,"select * from food_shop.tip_3")
business_3 <- dbGetQuery(mydb2,"select * from food_shop.business_3")
checkin_3 <- dbGetQuery(mydb2,"select * from food_shop.checkin_3")
user_3 <- dbGetQuery(mydb2,"select * from food_shop.user_2")   #just to maintain uniformity in naming

#missing value
missing.business <- sort(colMeans(is.na(business_3)) * 100)
missing.review   <- sort(colMeans(is.na(review_3)) * 100)
missing.tip      <- sort(colMeans(is.na(tip_3)) * 100)
missing.checkin  <- sort(colMeans(is.na(checkin_3)) * 100)
missing.user     <- sort(colMeans(is.na(user_3)) * 100)

business_3 <- business_3[,!( colMeans(is.na(business_3))*100 > 30 )]#remove columns having missing values > 70%
review_3   <- review_3[,!( colMeans(is.na(review_3))*100 > 30 )]
tip_3      <- tip_3[,!( colMeans(is.na(tip_3))*100 > 30 )]
checkin_3  <- checkin_3[,!( colMeans(is.na(checkin_3))*100 > 30 )]
user_3     <- user_3[,!( colMeans(is.na(user_3))*100 > 30 )]
