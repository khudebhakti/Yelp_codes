setwd("/media/bhakti/New Volume/MTECH14/Project")
library(RMySQL)
# sorting only Shopping and Restaurants data

mydb2 = dbConnect(MySQL(), user='root', password='root', host='localhost')
#dbSendQuery(mydb, "CREATE DATABASE food_shop;")
dbSendQuery(mydb2, "USE food_shop")
mydb2 = dbConnect(MySQL(), user='root', password='root', host='localhost', dbname="food_shop")

business <- dbGetQuery(mydb2,"select * from business_mod")
checkin <- dbGetQuery(mydb2,"select * from checkin_2")

#binding business with checkin    not working
business_checkin <- dbGetQuery(mydb2,"SELECT business_mod.*, checkin_2.checkin_info_9-5
                                      FROM business_mod
                                      INNER JOIN checkin_2
                                      ON business_mod.business_id = checkin_2.business_id")  

############################################  Not to be used otherwise needed
name <- colnames(checkin)[3:ncol(checkin)]
name <- sub("[.]","_",name)           #checkin_info.1-4 --> checkin_info_1-4

n <- c()
for(i in 1:length(name))
{ n <- c(n,paste("checkin_2.",name[i],sep="")) }
m <- as.list(n)
l <- toString(unlist(m))
#####################################################

colnames(checkin)[3:ncol(checkin)] <- sub("[.]","_",colnames(checkin)[3:ncol(checkin)])
#q <- "checkin_info_23-3"
#strsplit(q,split = "_")     # "checkin_info_23-3" --> "checkin" "info"    "23-3"   

colnames(checkin)[3:ncol(checkin)] = sapply(colnames(checkin)[3:ncol(checkin)],
                                            function(x){strsplit(x,split = "_")[[1]][3]})

checkin <- checkin[which(checkin$business_id %in% business$business_id),]

# > colnames(checkin)
# [1] "row_names"   "type"        "business_id" "9-5"         "7-5"         "13-3"        "17-6"        "13-0"       
# [9] "17-3"        "10-0"        "18-4"        "14-6"        "16-2"        "22-5"        "9-4"         "15-1"       
# [17] "15-0"        "15-4"        "11-5"        "21-4"        "13-1"        "17-4"        "14-4"        "12-5"       
# [25] "10-4"        "10-5"        "12-1"        "23-1"        "13-4"        "15-2"        "15-5"        "18-3"   

#calculating checkins per day wise
s <- colnames(checkin)
days <- c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday")
for(i in 1:nrow(checkin))
{
  print(i)
    for(j in 1:7)
    {
      checkin[i,days[j]] <- sum(checkin[i,grep(paste("-",j-1,sep=""),s)],na.rm = T)
    }
}

#calculating checkins per hour wise
time <- c("0","1","2","3","4","5","6","7","8","9","10","11","12",
          "13","14","15","16","17","18","19","20","21","22","23")
for(i in 1:nrow(checkin))
{
  print(i)
  for(j in 1:24)
  {
    checkin[i,time[j]] <- sum(checkin[i,grep(paste(j-1,"-",sep=""),s)],na.rm = T)
  }
}

checkin <- checkin[,-grep("-",colnames(checkin))]    #removing old columns
dbWriteTable(mydb2,"checkin_3",checkin,row.names=F,overwrite=T)    #only those business where checkin info available

#####################################################################              
length(which(checkin$business_id %in% business$business_id))
# g <- c(days,time)
# g
# [1] "Sunday"    "Monday"    "Tuesday"   "Wednesday" "Thursday"  "Friday"    "Saturday"  "0"         "1"        
# [10] "2"         "3"         "4"         "5"         "6"         "7"         "8"         "9"         "10"       
# [19] "11"        "12"        "13"        "14"        "15"        "16"        "17"        "18"        "19"       
# [28] "20"        "21"        "22"        "23"       
# > mat <- matrix(0,nrow=nrow(business),ncol=length(g))
# > dim(mat)

rowSums(checkin[1:10,3:ncol(checkin)],na.rm = T)
#########################################################################

a <- Sys.time()
business_checkin <- dbGetQuery(mydb2,"SELECT business_mod.*, checkin_3.`0`, checkin_3.`1`, checkin_3.`2`, 
checkin_3.`3`, checkin_3.`4`, checkin_3.`5`, checkin_3.`6`, checkin_3.`7`, checkin_3.`8`, checkin_3.`9`, 
checkin_3.`10`, checkin_3.`11`, checkin_3.`12`, checkin_3.`13`, checkin_3.`14`, checkin_3.`15`, checkin_3.`16`, 
checkin_3.`17`, checkin_3.`18`, checkin_3.`19`, checkin_3.`20`, checkin_3.`21`, checkin_3.`22`, checkin_3.`23`,
checkin_3.Sunday, checkin_3.Monday, checkin_3.Tuesday, checkin_3.Wednesday, checkin_3.Thursday, checkin_3.Friday, 
checkin_3.Saturday
                                FROM business_mod
                                LEFT JOIN checkin_3 ON business_mod.business_id = checkin_3.business_id
                                UNION
                                SELECT business_mod.*, checkin_3.`0`, checkin_3.`1`, checkin_3.`2`, 
checkin_3.`3`, checkin_3.`4`, checkin_3.`5`, checkin_3.`6`, checkin_3.`7`, checkin_3.`8`, checkin_3.`9`, 
checkin_3.`10`, checkin_3.`11`, checkin_3.`12`, checkin_3.`13`, checkin_3.`14`, checkin_3.`15`, checkin_3.`16`, 
checkin_3.`17`, checkin_3.`18`, checkin_3.`19`, checkin_3.`20`, checkin_3.`21`, checkin_3.`22`, checkin_3.`23`,
checkin_3.Sunday, checkin_3.Monday, checkin_3.Tuesday, checkin_3.Wednesday, checkin_3.Thursday, checkin_3.Friday, 
checkin_3.Saturday
                                 FROM business_mod
                                 RIGHT JOIN checkin_3 ON business_mod.business_id = checkin_3.business_id")  

dbWriteTable(mydb2,"business_checkin",business_checkin, row.names=F, overwrite=T)
b <- Sys.time()
b-a


################################################
#inner join
a <- Sys.time()
business_checkin1 <- dbGetQuery(mydb2,"SELECT business_mod.*, checkin_3.`0`, checkin_3.`1`, checkin_3.`2`, 
checkin_3.`3`, checkin_3.`4`, checkin_3.`5`, checkin_3.`6`, checkin_3.`7`, checkin_3.`8`, checkin_3.`9`, 
                               checkin_3.`10`, checkin_3.`11`, checkin_3.`12`, checkin_3.`13`, checkin_3.`14`, checkin_3.`15`, checkin_3.`16`, 
                               checkin_3.`17`, checkin_3.`18`, checkin_3.`19`, checkin_3.`20`, checkin_3.`21`, checkin_3.`22`, checkin_3.`23`,
                               checkin_3.Sunday, checkin_3.Monday, checkin_3.Tuesday, checkin_3.Wednesday, checkin_3.Thursday, checkin_3.Friday, 
                               checkin_3.Saturday
                                        FROM business_mod
                                        INNER JOIN checkin_3
                                        ON business_mod.business_id = checkin_3.business_id") 
b <- Sys.time()
b-a
###########################################################
# SELECT * FROM t1
# LEFT JOIN t2 ON t1.id = t2.id
# UNION
# SELECT * FROM t1
# RIGHT JOIN t2 ON t1.id = t2.id
###########################################################

food <- business_checkin[which(business_checkin$Food==1 & business_checkin$Shop==0),colnames(business_checkin) %in% days]
shop <- business_checkin[which(business_checkin$Food==0 & business_checkin$Shop==1),colnames(business_checkin) %in% days]
food_shop <- business_checkin[which(business_checkin$Food==1 & business_checkin$Shop==1),colnames(business_checkin) %in% days]
barplot(colSums(shop,na.rm = T),main = "Shop")
barplot(colSums(food,na.rm = T),main = "Food")

#by time   columns 104:127
c2 <- business_checkin[which(business_checkin$Food==1 & business_checkin$Shopping==0),104:(104+23)]
c3 <- business_checkin[which(business_checkin$Food==0 & business_checkin$Shopping==1),104:(104+23)]
barplot(colSums(c2,na.rm = T),main="Food")
barplot(colSums(c3,na.rm = T),main="Shop")
colSums(c2,na.rm = T)
colSums(c3,na.rm = T)     #missing value problem 13571/16864   #should we form a regression problem
############################################################
business_checkin <- dbGetQuery(mydb2,"select * from business_checkin")
checkin <- business_checkin[,104:127]   #by hours
