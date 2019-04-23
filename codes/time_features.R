#Time features
#setwd("D:/bhakti/yelp/7th_Challenge/yelp_dataset_challenge_academic_dataset/Restaurant_Shopping")
library(RMySQL)
# sorting only Shopping and Restaurants data

mydb2 = dbConnect(MySQL(), user='root', password='root', host='localhost')
#dbSendQuery(mydb, "CREATE DATABASE food_shop;")
dbSendQuery(mydb2, "USE food_shop")
mydb2 = dbConnect(MySQL(), user='root', password='root', host='localhost', dbname="food_shop")

# #two category examples
 business <- dbGetQuery(mydb2,"select * from business_mod")       #business which are open

time <- business[,grep("hours",colnames(business))]
#time$Open  <- rep(NA,nrow(time))
#time$Close <- rep(NA,nrow(time))
for(i in 1:nrow(time))
{
  time$Open[i] <- format(min(strptime(time[i,grep("open",colnames(time))],"%H:%M"),na.rm = T),"%H:%M")   #Min == Open
  time$Close[i] <- format(max(strptime(time[i,grep("close",colnames(time))],"%H:%M"),na.rm = T),"%H:%M")  #Max == close
}

time$Morning  <- rep(NA,nrow(time))
time$Afternoon <- rep(NA,nrow(time))
time$Evening <- rep(NA,nrow(time))
time$Night <- rep(NA,nrow(time))
#Bucketing the time slots:  5-12 : Morning, 12-5 : Afternoon, 5-9 : Evening, 9-5 : Night
for(i in 1:nrow(time))
{
  if(isTRUE( strptime(time$Open[i],"%H:%M") == strptime(time$Close[i],"%H:%M") ) ) {time$Morning[i] <- time$Afternoon[i] <- time$Evening[i] <-  time$Night[i] <- 1; next()}
  
  if(isTRUE( ( strptime(time$Open[i],"%H:%M") >= strptime("05:00","%H:%M") ) & ( strptime(time$Open[i],"%H:%M") < strptime("12:00","%H:%M") ) ) )
  {
    if(i == 5752) {print("1")}
    if(strptime(time$Close[i],"%H:%M") <= strptime("12:00","%H:%M")) {time$Morning[i] <- 1; time$Afternoon[i] <- time$Evening[i] <- time$Night[i] <- 0}
    if(isTRUE( (strptime(time$Close[i],"%H:%M") <= strptime("17:00","%H:%M")) & (strptime(time$Close[i],"%H:%M") > strptime("12:00","%H:%M")) ) ) {time$Morning[i] <- time$Afternoon[i] <- 1; time$Evening[i] <- time$Night[i] <- 0}
    if(isTRUE( (strptime(time$Close[i],"%H:%M") <= strptime("21:00","%H:%M")) & (strptime(time$Close[i],"%H:%M") > strptime("17:00","%H:%M")) ) ) {time$Morning[i] <- time$Afternoon[i] <- time$Evening[i] <- 1; time$Night[i] <- 0}
    if(isTRUE( (strptime(time$Close[i],"%H:%M") <= strptime("05:59","%H:%M")) | (strptime(time$Close[i],"%H:%M") > strptime("21:00","%H:%M")) ) ) {time$Morning[i] <- time$Afternoon[i] <- time$Evening[i] <-  time$Night[i] <- 1}
  }
  
  if(isTRUE( ( strptime(time$Open[i],"%H:%M") >= strptime("12:00","%H:%M") ) & ( strptime(time$Open[i],"%H:%M") < strptime("17:00","%H:%M") ) ) )
  {
    if(i == 5752) {print("2")}
    if(strptime(time$Close[i],"%H:%M") <= strptime("17:00","%H:%M")) {time$Morning[i] <- 0; time$Afternoon[i] <- 1; time$Evening[i] <- time$Night[i] <- 0}
    if(isTRUE( (strptime(time$Close[i],"%H:%M") <= strptime("21:00","%H:%M")) & (strptime(time$Close[i],"%H:%M") > strptime("17:00","%H:%M")) ) ) {time$Morning[i] <- 0; time$Afternoon[i] <- time$Evening[i] <- 1; time$Night[i] <- 0}
    if(isTRUE( (strptime(time$Close[i],"%H:%M") <= strptime("05:00","%H:%M")) | (strptime(time$Close[i],"%H:%M") > strptime("21:00","%H:%M")) ) ) {time$Morning[i] <- 0; time$Afternoon[i] <- time$Evening[i] <-  time$Night[i] <- 1}
    if(isTRUE( (strptime(time$Close[i],"%H:%M") <= strptime("11:59","%H:%M")) & (strptime(time$Close[i],"%H:%M") > strptime("05:00","%H:%M")) ) ) {time$Morning[i] <- time$Afternoon[i] <- time$Evening[i] <- time$Night[i] <- 1}
  }
  
  if(isTRUE( ( strptime(time$Open[i],"%H:%M") >= strptime("17:00","%H:%M") ) & ( strptime(time$Open[i],"%H:%M") < strptime("21:00","%H:%M") ) ) )
  {
    if(i == 5752) {print("3")}
    if(strptime(time$Close[i],"%H:%M") <= strptime("21:00","%H:%M")) {time$Morning[i] <- time$Afternoon[i] <- 0; time$Evening[i] <- 1; time$Night[i] <- 0}
    if(isTRUE( (strptime(time$Close[i],"%H:%M") <= strptime("05:00","%H:%M")) | (strptime(time$Close[i],"%H:%M") > strptime("21:00","%H:%M")) ) ) {time$Morning[i] <- time$Afternoon[i] <- 0; time$Evening[i] <-  time$Night[i] <- 1}
    if(isTRUE( (strptime(time$Close[i],"%H:%M") <= strptime("12:00","%H:%M")) & (strptime(time$Close[i],"%H:%M") > strptime("05:00","%H:%M")) ) ) {time$Morning[i] <- 1; time$Afternoon[i] <-0;  time$Evening[i] <- time$Night[i] <- 1}
    if(isTRUE( (strptime(time$Close[i],"%H:%M") <= strptime("16:59","%H:%M")) & (strptime(time$Close[i],"%H:%M") > strptime("12:00","%H:%M")) ) ) {time$Morning[i] <- time$Afternoon[i] <- time$Evening[i] <- time$Night[i] <- 1}
  }
  
  #   if(isTRUE( ( strptime(time$Open[i],"%H:%M") >= strptime("21:00","%H:%M") ) | ( strptime(time$Open[i],"%H:%M") < strptime("05:00","%H:%M") ) ) )
  #   {
  #     if(i == 5752) {print("4")}
  #     if(strptime(time$Close[i],"%H:%M") <= strptime("05:00","%H:%M")) {time$Morning[i] <- time$Afternoon[i] <- time$Evening[i] <- 0;  time$Night[i] <- 1}
  #     if(isTRUE( (strptime(time$Close[i],"%H:%M") <= strptime("12:00","%H:%M")) & (strptime(time$Close[i],"%H:%M") > strptime("05:00","%H:%M")) ) ) {time$Morning[i] <- 1; time$Afternoon[i] <- time$Evening[i] <- 0; time$Night[i] <- 1}
  #     if(isTRUE( (strptime(time$Close[i],"%H:%M") <= strptime("17:00","%H:%M")) & (strptime(time$Close[i],"%H:%M") > strptime("12:00","%H:%M")) ) ) {time$Morning[i] <- time$Afternoon[i] <- 1; time$Evening[i] <- 0; time$Night[i] <- 1}
  #     if(isTRUE( (strptime(time$Close[i],"%H:%M") <= strptime("21:00","%H:%M")) & (strptime(time$Close[i],"%H:%M") > strptime("17:00","%H:%M")) ) ) {time$Morning[i] <- time$Afternoon[i] <- time$Evening[i] <-  time$Night[i] <- 1}
  #     if(isTRUE( (strptime(time$Close[i],"%H:%M") >  strptime("21:00","%H:%M")) | (strptime(time$Close[i],"%H:%M") <= strptime("05:00","%H:%M")) )) {time$Morning[i] <- time$Afternoon[i] <- time$Evening[i] <-  time$Night[i] <- 1}
  #   }
  
  
  if( isTRUE(strptime(time$Open[i],"%H:%M") >= strptime("21:00","%H:%M") ))
  {
    if(isTRUE( (strptime(time$Close[i],"%H:%M") > strptime("21:00","%H:%M")) | (strptime(time$Close[i],"%H:%M") <= strptime("05:00","%H:%M")) ) ) {time$Morning[i] <- time$Afternoon[i] <- time$Evening[i] <- 0;  time$Night[i] <- 1}
    if(isTRUE( (strptime(time$Close[i],"%H:%M") <= strptime("12:00","%H:%M")) & (strptime(time$Close[i],"%H:%M") > strptime("05:00","%H:%M")) ) ) {time$Morning[i] <- 1; time$Afternoon[i] <- time$Evening[i] <- 0; time$Night[i] <- 1}
    if(isTRUE( (strptime(time$Close[i],"%H:%M") <= strptime("17:00","%H:%M")) & (strptime(time$Close[i],"%H:%M") > strptime("12:00","%H:%M")) ) ) {time$Morning[i] <- time$Afternoon[i] <- 1; time$Evening[i] <- 0; time$Night[i] <- 1}
    if(isTRUE( (strptime(time$Close[i],"%H:%M") <= strptime("20:59","%H:%M")) & (strptime(time$Close[i],"%H:%M") > strptime("17:00","%H:%M")) ) ) {time$Morning[i] <- time$Afternoon[i] <- time$Evening[i] <-  time$Night[i] <- 1}
  }
  
  if(isTRUE(strptime(time$Open[i],"%H:%M") < strptime("05:00","%H:%M")))
  {
    if(isTRUE( (strptime(time$Close[i],"%H:%M") <= strptime("12:00","%H:%M")) & (strptime(time$Close[i],"%H:%M") > strptime("05:00","%H:%M")) ) ) {time$Morning[i] <- 1; time$Afternoon[i] <- time$Evening[i] <- 0; time$Night[i] <- 1}
    if(isTRUE( (strptime(time$Close[i],"%H:%M") <= strptime("17:00","%H:%M")) & (strptime(time$Close[i],"%H:%M") > strptime("12:00","%H:%M")) ) ) {time$Morning[i] <- time$Afternoon[i] <- 1; time$Evening[i] <- 0; time$Night[i] <- 1}
    if(isTRUE( (strptime(time$Close[i],"%H:%M") > strptime("17:00","%H:%M")) | (strptime(time$Close[i],"%H:%M") < strptime("04:59","%H:%M")) ) ) {time$Morning[i] <- time$Afternoon[i] <- time$Evening[i] <-  time$Night[i] <- 1}
  }
  
}

#time <- time[,-grep("hours",colnames(time))]
time <- time[,c("Open","Close","Morning","Afternoon","Evening","Night")]

#--------------------------------------------------------------


#finding the days when the shop is open
days <- c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday")
Day  <- as.data.frame(matrix(0,nrow=nrow(business),ncol=7))
colnames(Day) <- days
for(i in 1:nrow(business))
{
  for (j in 1:length(days))
  {
    day <- capture.output(cat("hours.",days[j],".open",sep=""))  
    if(!is.na(business[i,day])) { Day[i,days[j]] <- 1 }
    #cat("hours.",days[j],".open\n",sep="")
  }
}
Day[which(is.na(time$Open)),] <- NA     # Shops in which all time data is NA missing
# Even shops that are closed on any particular day are NA so they will remain as closed '0'
time$Open <- time$Close <- NULL
time <- cbind(time,Day)
