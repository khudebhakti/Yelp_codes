library(RMySQL)
library(jsonlite)
setwd("D:/bhakti/yelp/7th_Challenge/yelp_dataset_challenge_academic_dataset/Restaurant_Shopping")

json_file <- "yelp_academic_dataset_business.json"
business  <-  fromJSON(sprintf("[%s]", paste(readLines(json_file),collapse = ",")),flatten = T)
#business  <- business[,1:3]
business$categories    <- as.character(business$categories)     #dataframe which has list
business$neighborhoods <- as.character(business$neighborhoods)  #is not flattened

for(i in 1:ncol(business))
{
  if(class(business[,i])=="logical")
    business[,i] <- as.character(business[,i])
}


mydb = dbConnect(MySQL(), user='root', password='root', host='localhost')
#dbSendQuery(mydb, "CREATE DATABASE yelp;")
dbSendQuery(mydb, "USE yelp")
mydb = dbConnect(MySQL(), user='root', password='root', host='localhost', dbname="yelp")

dbWriteTable(mydb,"business",business,row.names=F)


json_file <- "yelp_academic_dataset_checkin.json"
checkin   <- fromJSON(sprintf("[%s]", paste(readLines(json_file),collapse = ",")), flatten = T)
dbWriteTable(mydb,"checkin",checkin,row.names=F)

json_file <- "yelp_academic_dataset_tip.json"
assign(strsplit(json_file, '[_|.]')[[1]][4], 
       fromJSON(sprintf("[%s]", paste(readLines(json_file),collapse = ",")),flatten = T))
dbWriteTable(mydb,"tip",tip,row.names=F)

json_file <- "yelp_academic_dataset_user.json"
assign(strsplit(json_file, '[_|.]')[[1]][4], 
       fromJSON(sprintf("[%s]", paste(readLines(json_file),collapse = ",")),flatten = T))
user$friends <- as.character(user$friends)
user$elite   <- as.character(user$elite)
dbWriteTable(mydb,"user",user,row.names=F)

rev1 <- read.csv("Review1.csv")
dbWriteTable(mydb, "review", rev1, row.names=F)

rev2 <- read.csv("Review2.csv")
dbWriteTable(mydb, "review", rev2, row.names=F, append=T)

rev3 <- read.csv("Review3.csv")
dbWriteTable(mydb, "review", rev3, row.names=F, append=T)
