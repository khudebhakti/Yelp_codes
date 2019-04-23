setwd("D:/bhakti/yelp/7th_Challenge/yelp_dataset_challenge_academic_dataset")

#converting JSON to CSV
library(jsonlite)


#Data Flattening

json_file <- "yelp_academic_dataset_checkin.json"
checkin   <- fromJSON(sprintf("[%s]", paste(readLines(json_file, n=5),collapse = ",")), flatten = T)

##############################################################

json_file <- "yelp_academic_dataset_business.json"
business  <-  fromJSON(sprintf("[%s]", paste(readLines(json_file),collapse = ",")),flatten = T)
#business$categories    <- as.character(business$categories)     #dataframe which has list
#business$neighborhoods <- as.character(business$neighborhoods)  #is not flattened
temp <- c()                    #dataframe which has list is not flattened
temp <- sapply(business$categories,function(x){rbind(temp,toString(unlist(x)))})
business$categories    <- temp    

temp <- c()
temp <- sapply(business$neighborhoods,function(x){rbind(temp,toString(unlist(x)))})
business$neighborhoods <-  temp 

for(i in 1:ncol(business))
{
  if(class(business[,i])=="logical")
    business[,i] <- as.character(business[,i])
}

##############################################################
json_file <- "yelp_academic_dataset_review.json"
assign(strsplit(json_file, '[_|.]')[[1]][4], 
       fromJSON(sprintf("[%s]", paste(readLines(json_file, n = 5),collapse = ",")), flatten = T))

json_file <- "yelp_academic_dataset_tip.json"
assign(strsplit(json_file, '[_|.]')[[1]][4], 
       fromJSON(sprintf("[%s]", paste(readLines(json_file, n = 5),collapse = ",")),flatten = T))

json_file <- "yelp_academic_dataset_user.json"
assign(strsplit(json_file, '[_|.]')[[1]][4], 
       fromJSON(sprintf("[%s]", paste(readLines(json_file),collapse = ",")),flatten = T))
#user$friends <- as.character(user$friends)
#user$elite   <- as.character(user$elite)
temp <- c()                    #dataframe which has list is not flattened
temp <- sapply(user$friends,function(x){rbind(temp,toString(unlist(x)))})
user$friends    <- temp 

temp <- c()                    #dataframe which has list is not flattened
temp <- sapply(user$elite,function(x){rbind(temp,toString(unlist(x)))})
user$elite    <- temp  

# s = strsplit(business$categories[5],split = '["\"]')
sapply(user,class)   #to findout class of each column
