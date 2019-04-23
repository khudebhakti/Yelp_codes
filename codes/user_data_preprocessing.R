setwd("D:/bhakti/yelp/7th_Challenge/yelp_dataset_challenge_academic_dataset")

library(jsonlite)
json_file <- "yelp_academic_dataset_user.json"
assign(strsplit(json_file, '[_|.]')[[1]][4], 
       fromJSON(sprintf("[%s]", paste(readLines(json_file,n=5),collapse = ",")),flatten = T))
for(i in 1: nrow(user))
{
  user$nfriends[i] <- length(unlist(user$friends[i]))   #counting the number of friends
  user$nelite[i]   <- length(unlist(user$elite[i]))
}
user$friends <- as.character(user$friends)
user$elite   <- as.character(user$elite)
 
user$friends <- user$elite <- NULL

apply(user,2,function(x){sum(is.na(x))/nrow(user)})     #counts the no. of missing values per column