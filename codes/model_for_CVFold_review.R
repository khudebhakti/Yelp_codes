setwd("E:/Bhakti/Restaurant_Shopping")
source('E:/Bhakti/Restaurant_Shopping/scripts/CVFold.R')

#python cleaned data
business = read.csv("business_tfidf_missing_imputed_clean2.csv",stringsAsFactors=F)
review = read.csv("review_sort_clean2.csv",stringsAsFactors=F)
#tip = read.csv("tip_sort_clean2.csv",stringsAsFactors=F)


set.seed(1)
train.ind <- sample(1:nrow(business),size = round(.7*nrow(business))) 
train_business <- business[train.ind,] 
test_business  <- business[-train.ind,]


#NGram tokenizer
NGram <- function(corpus,gram)
{
  options(mc.cores=1)
  Tokenizer <- function(x) NGramTokenizer(x, Weka_control(min = gram, max = gram))
  dtm    <- DocumentTermMatrix(corpus, control=list(tokenize=Tokenizer,
                                                    weighting=function(x)weightTfIdf(x,normalize=T)))
  return(dtm)
}

library(tm)
library(SnowballC)
library(RWeka)
library(C50)

train.ind <- which(review$business_id %in% train_business$business_id) 
train_review <- review[train.ind,] 
test_review  <- review[-train.ind,]

combi <- rbind(train_review,test_review)
corpus <- Corpus(VectorSource(combi$new_text2))
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
print(dim(train))
class <- train_review[,c("Shopping","Food")]
labelNames <- colnames(class)
test_class <- test_review[,c("Shopping","Food")]
colnames(test_class) <- labelNames

memory.limit(50000)
data <- rbind(cbind(train,class),cbind(test,test_class))
set.seed(1)
data = data[sample(nrow(data)),]
k <- 5
step<-round(nrow(data)/k)    #gives the step size for every fold

#sVD
a1 <- Sys.time()
p1 <- CVFold(data,hidden_N = c(100,50),300)
b1 <- Sys.time()
b1-a1
print(p1)

train <- as.data.frame(cbind(dtm[1:nrow(train_review),],
                             train_review[,c("stars","votes.funny","votes.useful","votes.cool")]))
test  <- as.data.frame(cbind(dtm[-(1:nrow(train_review)),],
                             test_review[,c("stars" ,"votes.funny","votes.useful","votes.cool")]))
print(dim(train))
class <- train_review[,c("Shopping","Food")]
labelNames <- colnames(class)
test_class <- test_review[,c("Shopping","Food")]
colnames(test_class) <- labelNames

memory.limit(80000)
data <- rbind(cbind(train,class),cbind(test,test_class))
set.seed(1)
data = data[sample(nrow(data)),]
k <- 5
step<-round(nrow(data)/k)    #gives the step size for every fold
a1 <- Sys.time()
p2 <- CVFold2(data) #DT
b1 <- Sys.time()
b1-a1
print(p2)

