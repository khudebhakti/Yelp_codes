#continuation after test_class in food_shop_model2.R

data <- rbind(train,test)      #shuffled order data
data <- as.data.frame(data)
#data$review_count <- NULL   #we do not need review_count to impute missing values

attr <- c("attributes.Accepts Credit Cards","attributes.Price Range","attributes.Parking.garage",
          "attributes.Parking.street","attributes.Parking.validated","attributes.Parking.lot",
          "attributes.Parking.valet")
#attr <- as.data.frame(apply(attr,2,as.factor))  #convert all columns to factor

f <- which(colnames(b) %in% attr)
features <- b[,f]
colnames(features)[1:2] <- c("attributes.Accepts.CreditCards","attributes.PriceRange")
attr <- colnames(features)

for(f in colnames(features))
{
   print(f)
   levels <- length(levels(as.factor(features[,f])))
   if(levels==2)
   {
     print("in 2")
     features[which(factor(features[,f])==1),f] <- 0   #1 is false
     features[which(factor(features[,f])==2),f] <- 1    #2 is true
   }
#    if(levels > 2)               #make provision for missing value
#    {
#      #print("in 3")
#      dummyMat<- c()
#      dummyMat <- OneHotEncode(features[,f],f)
#      features[,f] <- NULL
#      features <- cbind(features,dummyMat)
#    }
  # print(features[1:10,f])
}
train_m <- features[train.ind,]
test_m <- features[-train.ind,]
features <- rbind(train_m,test_m)   #arranged in the shuffled order


#data <- cbind(business[,c("review_count","name","stars","nOpenDays","Food","Shopping")],features)

#Missing values imputed starting with features with lowest MV to feature with highes MV
for(a in colnames(features))
{
  print(a)
  data <- cbind(data,features[a])
  indx <- which(is.na(data[a]))
  trainM <- data[-indx,]
  testM  <- data[indx,]
  trainM[a] <- as.factor(trainM[,a])
  formula = as.formula(paste(a, "~."))
  library(C50)
  model = C5.0(formula, data=trainM)
  testM[a]  <- predict(model,testM)
  data[indx,a] <- as.numeric(as.character(testM[,a]))   #replacing the missing values in the original matrix
  #data[a] <- as.factor(data[,a])
}   #data with other attributes completed


# time missing value imputation

time <- rbind(train_t,test_t)  #data arranged in shuffled order
data <- cbind(data,time)
data <- as.data.frame(data)

#nOpenDays was initially 0 for missing value data. Converted back to NA
data$nOpenDays <- sapply(data$nOpenDays, function(x){ifelse(x==0,return(NA),x)})

names <- c("nOpenDays",colnames(time))

d <- data[,(colnames(data) %in% names)]
data <- data[,!(colnames(data) %in% names)]
data <- cbind(data,d)   #rearraging the columns

index <- which(is.na(data$nOpenDays))   #indices of the missing rows in main data 
d <- data[,(colnames(data) %in% names)]
data <- data[,!(colnames(data) %in% names)]

for(a in colnames(time))
{
  print(a)
  data <- cbind(data,d[a])
  indx <- which(is.na(data[a]))
  trainM <- data[-indx,]
  testM  <- data[indx,]
  trainM[a] <- as.factor(trainM[,a])
  formula = as.formula(paste(a, "~."))
  library(C50)
  model = C5.0(formula, data=trainM)
  testM[a]  <- predict(model,testM)
  data[indx,a] <- as.numeric(as.character(testM[,a]))   #replacing the missing values in the original matrix
  #data[a] <- as.factor(data[,a])
}   #data with other attributes completed


data$nOpenDays <- rowSums(data[,c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday")])


#checkin imputation
checkin <- rbind(train_c,test_c)  #data arranged in shuffled order
#data <- cbind(data,checkin)
data <- as.data.frame(data)

# for(a in colnames(checkin))  #does not work  (SVR)
# {
#   print(a)
#   data <- cbind(data,checkin[a])
#   indx <- which(is.na(data[a]))
#   trainM <- data[-indx,]
#   testM  <- data[indx,]
#   #trainM[a] <- as.factor(trainM[,a])
#   #formula = as.formula(paste(as.character(a), "~."))
#   #model<- svm(x=trainM[,1:(ncol(trainM)-1)],y=trainM[ncol(trainM)],type="eps-regression",kernel="radial")
#   #testM[a]  <- predict(model,testM)
#   data[indx,a] <- as.numeric(as.character(testM[,a]))   #replacing the missing values in the original matrix
#   #data[a] <- as.factor(data[,a])
# }   #data with other attributes completed


for(a in colnames(checkin))    #by taking mean of the business with the same STAR rating
{
  print(a)
  data <- cbind(data,checkin[a])
  trainM <- data[-indx,]
  testM  <- data[indx,]
  for (i in 1:nrow(testM))
  {
    testM[i,as.character(a)] <- round(mean(trainM[which(trainM$stars==testM$stars[i]),as.character(a)]))
  }
  data[indx,as.character(a)] <- testM[,as.character(a)]
}

for(a in colnames(checkin))  #keeping missing value as a third category
{
  print(a)
  data <- cbind(data,checkin[a])
  indx <- which(is.na(data[a]))
  data[indx,as.character(a)] <- -1
}


train <- data[1:nrow(train_business),]
test <- data[-(1:nrow(train_business)),]
class <- train_business[,c("Shopping","Food")]
labelNames <- colnames(class)
test_class <- test_business[,c("Shopping","Food")]
colnames(test_class) <- labelNames

##############################
#by KNN
a1 <- Sys.time()
for(i in 1:nrow(test))
{
  x <- rbind(test_m[i,!(colnames(data) %in% names)],train_m[,!(colnames(data) %in% names)])
  z<-as.matrix(dist(x,upper = T))  #
  z<-as.matrix(sort(z[,1]))   #sorting to get nearest neighbours
  row = row.names(z)[2]       #nearest neighbour
  test_m[i,(colnames(data) %in% names)] = train_m[row,(colnames(data) %in% names)] 
}
b1 <- Sys.time()
data[indx,(colnames(data) %in% names)] <- test[,(colnames(data) %in% names)] #filling in the original data

train <- data[1:nrow(train_business),]
test  <- data[-(1:nrow(train_business)),]

class <- train_business[,c("Shopping","Food")]
labelNames <- colnames(class)
test_class <- test_business[,c("Shopping","Food")]
colnames(test_class) <- labelNames
predictions <- matrix(0,nrow=nrow(test), ncol=length(labelNames))
predictions <- data.frame(predictions)
colnames(predictions) <- labelNames
##########################

#pass it to the classification algorithm


#One hot encoding for price range

OneHotEncode <- function(feature,name)
{
  feature <- as.factor(feature)
  dummyMat <- matrix(0,nrow=length(feature),ncol=length(levels(feature)))
  for(i in 1:nrow(dummyMat))
  {
    dummyMat[i,feature[i]] <- 1
  }
  colnames(dummyMat) <- paste(name,"_",levels(factor(feature)),sep = "")
  return(dummyMat)
}


dummyMat<- c()
dummyMat <- OneHotEncode(data[,"attributes.PriceRange"],"attributes.PriceRange")
data[,"attributes.PriceRange"] <- NULL
data <- cbind(data,dummyMat)

# paste("sdfsd_",as.numeric(levels(factor(data$attributes.PriceRange))),sep = "")

#convert factor back to numeric
indx <- sapply(data ,is.factor)
data[indx] <- lapply(data[indx], function(x) as.numeric(as.character(x)))

#data <- data[,!(names(data) %in% attr)]   removing multiple columns from a dataframe
#############
