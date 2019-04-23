setwd("E:/Bhakti/Restaurant_Shopping")
source('E:/Bhakti/Restaurant_Shopping/scripts/CVFold.R')

#python cleaned data
business = read.csv("business_tfidf_missing_imputed_clean2.csv",stringsAsFactors=F)
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

combi <- rbind(train_business,test_business)
corpus <- Corpus(VectorSource(combi$new_name2))
dtm1 <- NGram(corpus,1)
dtm2 <- NGram(corpus,2)
Dtm1 <- removeSparseTerms(dtm1,0.999)
Dtm2 <- removeSparseTerms(dtm2,0.999)
Dtm1 <- as.data.frame(as.matrix(Dtm1))
Dtm2 <- as.data.frame(as.matrix(Dtm2))
dtm  <- cbind(Dtm1, Dtm2)
dtm <- as.data.frame(as.matrix(dtm))

train <- as.data.frame(cbind(dtm[1:nrow(train_business),],train_business[,9:ncol(train_business)],
                             "stars"=train_business$stars))
test <- as.data.frame(cbind(dtm[-(1:nrow(train_business)),],test_business[,9:ncol(test_business)],
                             "stars"=test_business$stars))

print(dim(train))
class <- train_business[,c("Shopping","Food")]
labelNames <- colnames(B_class)
test_class <- test_business[,c("Shopping","Food")]
colnames(test_class) <- labelNames


memory.limit(50000)
data <- rbind(cbind(train,class),cbind(test,test_class))
set.seed(1)
data = data[sample(nrow(data)),]
k <- 5
step<-round(nrow(data)/k)    #gives the step size for every fold

a1 <- Sys.time()
p1 <- CVFold(data,hidden_N = c(100,50),300)
b1 <- Sys.time()
b1-a1
print(p1)
a1 <- Sys.time()
p2 <- CVFold2(data) #DT
b1 <- Sys.time()
b1-a1
print(p2)

