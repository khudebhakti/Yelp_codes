setwd("D:/bhakti/yelp/7th_Challenge/yelp_dataset_challenge_academic_dataset/Restaurant_Shopping/")
library(jsonlite)
#json_file <- "yelp_academic_dataset_business.json"
#business  <-  fromJSON(sprintf("[%s]", paste(readLines(json_file),collapse = ",")),flatten = T)

#nCategories <- array(0,dim=nrow(business))
# for(i in 1:nrow(business))
# {
#   nCategories[i] <- length(unlist(business$categories[i]))   #counting the number of categories
# }
#business$nCategories <- as.numeric(nCategories)

business <- read.csv("Business.csv", stringsAsFactors = F)

days <- c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday")
#business1 <- business[1:5,]
nDaysOpen <- array(0,dim=nrow(business))
for(i in 1:nrow(business))
{
  for (j in 1:length(days))
  {
      day <- capture.output(cat("hours.",days[j],".open",sep=""))   
      #cat("hours.",days[j],".open\n",sep="")
      if(!is.na(business[i,day]))nDaysOpen[i] <- nDaysOpen[i] + 1 
  }
}

business$nDaysOpen <- as.numeric(nDaysOpen)                    #Added the number of days open to the dataset
#business <- business[ , -grep( "hours" , names( business )) ]  #Dropping the columns containing open close information grep command will 
                                                               #delete all columns containing hours word
# myVectorOfStrings <- c("attributes", "hours")
# matchExpression <- paste(myVectorOfStrings, collapse = "|")
# # [1] "foo|bar"
# b <- b %>% select(matches(matchExpression))

apply(business,2,function(x){(sum(is.na(x))/nrow(business))*100})                   #counts the no. of missing values per column


######################

Cat <- c()
#for(i in 1:10)
#   {
#     c <- unlist(business$categories[i])
#     Cat <- rbind(Cat, toString(c))
#}
#Option for this for-loop is below
Cat <- sapply(business$categories,function(x){rbind(Cat,toString(unlist(x)))}) #"Fast Food, Restaurants"

#####################
#To get the distribution of 22 categories with overlap

bus.cat <- c("Food", "Nightlife", "Restaurants","Shopping","Active Life","Arts & Entertainment",
             "Automotive", "Beauty & Spas", "Education", "Event Planning & Services", "Health & Medical",
             "Home Services", "Local Services", "Financial Services", "Hotels & Travel","Local Flavor",
             "Mass Media", "Professional Services", "Public Services & Government", "Real Estate",
             "Religious Organizations","Pets")

mat <- matrix(0, nrow=1,ncol=length(bus.cat))
colnames(mat) <- bus.cat

for(i in 1:nrow(business1))
{
  a <- which(bus.cat %in% unlist(strsplit(business1$categories[i],split=", ")))
  mat[,a] <- mat[,a] + 1
}

#barplot(mat,names.arg = bus.cat, horiz = T, border = NA, las=1, 
#         main = "Distribution",space=1, cex.axis = 0.5, cex.names = 0.5, axis.lty = 1)

freq <- as.numeric(mat)
ylim <- c(0, 1.1*max(freq))
xx <- barplot(freq, xaxt = 'n', xlab = '', width = 0.85, ylim = ylim,
              main = "Missing value in Business", ylab = "Frequency")
text(x = xx, y = freq, label = freq, pos = 3, cex = 1, col = "red")
axis(1, at=xx, labels=colnames(business_3), tick=FALSE, las=2, line=-0.5, cex.axis=0.8)
#########################################################


#converting to attributes columns to factor to numeric to factor 
business <- dbGetQuery(mydb2,"select * from business_mod")
b <- business
#myVectorOfStrings <- c("attributes", "hours")
#matchExpression <- paste(myVectorOfStrings, collapse = "|")  
# [1] "attributes|hours"
#b <- b %>% select(matches(matchExpression))    #selects columns having words attributes or hours

business[grep("attributes",names(business))]<- as.data.frame(lapply(business[grep("attributes",names(business))],factor))
business[grep("attributes",names(business))]<- as.data.frame(lapply(business[grep("attributes",names(business))],as.numeric))
business[grep("attributes",names(business))]<- as.data.frame(lapply(business[grep("attributes",names(business))],factor))
dbWriteTable(mydb2,"business_mod2",business,row.names=F)

################################################################


