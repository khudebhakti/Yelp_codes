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

#######################################################

set.seed(1)
train.ind <- sample(1:nrow(business),size = round(.7*nrow(business))) 
train_business <- business[train.ind,] 
test_business  <- business[-train.ind,]

#Get the time features
source("scripts/time_features.R")
train_t <- time[train.ind,]
test_t <- time[-train.ind,]

#Get the checkin features
# business_checkin <- dbGetQuery(mydb2,"select * from business_checkin")
# checkin <- business_checkin[,104:127]   #by hours
# #checkin <- rowSums(checkin)
# train_c <- checkin[train.ind,]
# test_c  <- checkin[-train.ind,]


#NGram tokenizer
NGram <- function(corpus,gram)
{
  options(mc.cores=1)
  Tokenizer <- function(x) NGramTokenizer(x, Weka_control(min = gram, max = gram))
  corpus <- tm_map(corpus,content_transformer(tolower))
  corpus <- tm_map(corpus,removePunctuation)
  corpus <- tm_map(corpus,removeNumbers)
  corpus <- tm_map(corpus,removeWords,stopwords("english"))
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, stemDocument, language = "english")
  dtm    <- DocumentTermMatrix(corpus, control=list(tokenize=Tokenizer,
                                                    weighting=function(x)weightTfIdf(x,normalize=T)))
  return(dtm)
}


library(e1071)
library(tm)
library(SnowballC)
library(RWeka)
library(C50)

combi <- rbind(train_business,test_business)
corpus <- Corpus(VectorSource(combi$name))
dtm1 <- NGram(corpus,1)
dtm2 <- NGram(corpus,2)
Dtm1 <- removeSparseTerms(dtm1,0.999)
Dtm2 <- removeSparseTerms(dtm2,0.999)
Dtm1 <- as.data.frame(as.matrix(Dtm1))
Dtm2 <- as.data.frame(as.matrix(Dtm2))
dtm  <- cbind(Dtm1, Dtm2)
dtm <- as.data.frame(as.matrix(dtm))

b1 <- b[,!(colMeans(is.na(b))*100>30)]
b1$Shopping<-b1$Food <-b1$business_id <- b1$name <- b1$open<- b1$review_count<- NULL
train_b <- b1[train.ind,]
test_b <- b1[-train.ind,]
dim(b1)

train <- dtm[1:nrow(train_business),]
test  <- dtm[-(1:nrow(train_business)),]

train <- as.data.frame(cbind(dtm[1:nrow(train_business),],train_business[,c("stars","nOpenDays")]))
test  <- as.data.frame(cbind(dtm[-(1:nrow(train_business)),],test_business[,c("stars","nOpenDays")]))

train <- as.data.frame(cbind(dtm[1:nrow(train_business),],train_b,train_t,train_c))
test  <- as.data.frame(cbind( dtm[-(1:nrow(train_business)),],test_b,test_t,test_c))

train <- as.data.frame(cbind(dtm[1:nrow(train_business),],train_b,train_t,"checkin"=as.vector(train_c)))
test  <- as.data.frame(cbind( dtm[-(1:nrow(train_business)),],test_b,test_t,"checkin"=as.vector(test_c)))

class <- train_business[,c("Shopping","Food")]
labelNames <- colnames(class)
#class$Shopping <- factor(class$Shopping)
#class$Food <- factor(class$Food)

test_class <- test_business[,c("Shopping","Food")]
colnames(test_class) <- labelNames

predictions <- matrix(0,nrow=nrow(test), ncol=length(labelNames))
predictions <- data.frame(predictions)
colnames(predictions) <- labelNames

library(deepnet)
a<- Sys.time()
set.seed(1)
nn <- nn.train(as.matrix(train),as.matrix(class),hidden = c(100,50),numepochs = 100)
               #hidden_dropout = 0.1, visible_dropout = 0.2)
predictions <- nn.predict(nn,test)
predictions <- round(predictions)
sum(test_class != predictions)/(nrow(test_class)*ncol(test_class))  #Hamming-Loss
b <- Sys.time()
b-a


sum(test_class != predictions)/(nrow(test_class)*ncol(test_class))  #Hamming-Loss
source("scripts/multi_label_F_measure.R")


#review
train.ind <- which(review$business_id %in% train_business$business_id) 
train_review <- review[train.ind,] 
test_review  <- review[-train.ind,]

combi <- rbind(train_review,test_review)
corpus <- Corpus(VectorSource(combi$text))
dtm1 <- NGram(corpus,1)
dtm2 <- NGram(corpus,2)
Dtm1 <- removeSparseTerms(dtm1,0.99)
Dtm2 <- removeSparseTerms(dtm2,0.995)
Dtm1 <- as.data.frame(as.matrix(Dtm1))
Dtm2 <- as.data.frame(as.matrix(Dtm2))
dtm  <- cbind(Dtm1, Dtm2)
dtm <- as.data.frame(as.matrix(dtm))
train <- as.data.frame(cbind(dtm[1:nrow(train_review),],
                             train_review[,c("stars","votes.funny","votes.useful","votes.cool")]))
test  <- as.data.frame(cbind(dtm[-(1:nrow(train_review)),],
                             test_review[,c("stars" ,"votes.funny","votes.useful","votes.cool")]))
class <- as.numeric(factor(train_review$new_category))
model <- svm(train,factor(class))
p <- predict(model,test)


class <- train_review[,c("Shopping","Food")]
#class$Shopping <- factor(class$Shopping)
#class$Food <- factor(class$Food)
labelNames <- colnames(class)
predictions <- matrix(0,nrow=nrow(test), ncol=length(labelNames))
predictions <- data.frame(predictions)
colnames(predictions) <- labelNames

test_class <- test_review[,c("Shopping","Food")]
colnames(test_class) <- labelNames

library(deepnet)
a<- Sys.time()
set.seed(1)
nn <- nn.train(as.matrix(train),as.matrix(class),hidden = c(100,50),numepochs = 100)
               #hidden_dropout = 0.1, visible_dropout = 0.2)
predictions <- nn.predict(nn,test)
predictions <- round(predictions)
sum(test_class != predictions)/(nrow(test_class)*ncol(test_class))  #Hamming-Loss
b <- Sys.time()
b-a


sum(test_class != predictions)/(nrow(test_class)*ncol(test_class))  #Hamming-Loss
source("scripts/multi_label_F_measure.R")

#tip
train.ind <- which(tip$business_id %in% train_business$business_id) 
train_tip <- tip[train.ind,] 
test_tip  <- tip[-train.ind,]

#tip
combi <- rbind(train_tip,test_tip)
corpus <- Corpus(VectorSource(combi$text))
dtm1 <- NGram(corpus,1)
dtm2 <- NGram(corpus,2)
Dtm1 <- removeSparseTerms(dtm1,0.995)
Dtm2 <- removeSparseTerms(dtm2,0.995)
Dtm1 <- as.data.frame(as.matrix(Dtm1))
Dtm2 <- as.data.frame(as.matrix(Dtm2))
dtm  <- cbind(Dtm1, Dtm2)
dtm <- as.data.frame(as.matrix(dtm))
train <- as.data.frame(cbind(train_tip["likes"],dtm[1:nrow(train_tip),]))
test  <- as.data.frame(cbind(test_tip["likes"], dtm[-(1:nrow(train_tip)),]))

class <- as.numeric(factor(train_tip$new_category))
model <- svm(train,factor(class))
p <- predict(model,test)


class <- train_tip[,c("Shopping","Food")]
class$Shopping <- factor(class$Shopping)
class$Food <- factor(class$Food)
labelNames <- colnames(class)
test_class <- test_tip[,c("Shopping","Food")]
colnames(test_class) <- labelNames
predictions <- matrix(0,nrow=nrow(test), ncol=length(labelNames))
predictions <- data.frame(predictions)
colnames(predictions) <- labelNames

library(deepnet)
a<- Sys.time()
set.seed(1)
nn <- nn.train(as.matrix(train),as.matrix(class),hidden = c(100,50),numepochs = 100)
               #hidden_dropout = 0.1, visible_dropout = 0.2)
predictions <- nn.predict(nn,test)
predictions <- round(predictions)
sum(test_class != predictions)/(nrow(test_class)*ncol(test_class))  #Hamming-Loss
b <- Sys.time()
b-a

test_class <- test_tip[,c("Shopping","Food")]
colnames(test_class) <- labelNames
sum(test_class != predictions)/(nrow(test_class)*ncol(test_class))  #Hamming-Loss
source("scripts/multi_label_F_measure.R")

