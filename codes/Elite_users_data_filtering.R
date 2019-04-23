setwd("D:/bhakti/yelp/7th_Challenge/yelp_dataset_challenge_academic_dataset/Restaurant_Shopping")
library(RMySQL)
mydb = dbConnect(MySQL(), user='root', password='root', host='localhost')
#dbSendQuery(mydb2, "CREATE DATABASE yelp;")
dbSendQuery(mydb, "USE yelp")
mydb = dbConnect(MySQL(), user='root', password='root', host='localhost', dbname="yelp")

mydb2 = dbConnect(MySQL(), user='root', password='root', host='localhost')
#dbSendQuery(mydb, "CREATE DATABASE food_shop;")
dbSendQuery(mydb2, "USE food_shop")
mydb2 = dbConnect(MySQL(), user='root', password='root', host='localhost', dbname="food_shop")

#two category examples
business <- dbGetQuery(mydb2,"select * from business_mod")       #business which are open
review <-  dbGetQuery(mydb2,"select * from review_mod")
tip <- dbGetQuery(mydb2,"select * from tip_mod")
user <- dbGetQuery(mydb,"select * from user2")   #all users. mydb2 doesnt have all users who gave tips

user = user[,c("user_id","nElite")]
review = merge(x = review, y = user, by = "user_id", all.x = TRUE)  #inner join
tip = merge(x = tip, y = user, by = "user_id", all.x = TRUE)  #inner join

#considering only 5 reviews and 5 tips per business
review_sort <- tip_sort <- sel_rev <- sel_tip <- c()
for (i in 1:nrow(business))
{
   print(i)
   r <- which(review$business_id == business$business_id[i])
   if(length(r)>5)
   {
      sel_rev <- review[r,]
      sel_rev <- sel_rev[order(sel_rev$nElite, decreasing = T),]
      sel_rev <- sel_rev[1:5,]
   }
   else
   {
     sel_rev <- review[r,]
   }
   review_sort <- rbind(review_sort,sel_rev)
   
  t <- which(tip$business_id == business$business_id[i])
  if(length(t)>5)
  {
    sel_tip <- tip[t,]
    sel_tip <- sel_tip[order(sel_tip$nElite, decreasing = T),]   #ordering as per Elite
    sel_tip <- sel_tip[1:5,]   #selecting top 5
  }
  else
  {
    sel_tip <- tip[t,]
  }
  tip_sort <- rbind(tip_sort,sel_tip)
}

dbWriteTable(mydb2,"review_sort_elite",review_sort, row.names=F,overwrite=T)
dbWriteTable(mydb2,"tip_sort_elite",tip_sort, row.names=F,overwrite=T)
write.csv(review_sort,"review_sort_elite.csv",row.names = F)
write.csv(tip_sort,"tip_sort_elite.csv",row.names = F)
