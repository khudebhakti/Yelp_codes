#python cleaned data 
setwd("/media/bhakti/New Volume/MTECH14/Project")

#python cleaned data
business = read.csv("business_tfidf_imputed_2.csv",stringsAsFactors=F)
review = read.csv("review_sort_clean.csv",stringsAsFactors=F)
tip = read.csv("tip_sort_clean.csv",stringsAsFactors=F)


set.seed(1)
train.ind <- sample(1:nrow(business),size = round(.7*nrow(business))) 
train_business <- business[train.ind,] 
test_business  <- business[-train.ind,]


#NGram tokenizer
NGram <- function(corpus,gram)
{
  options(mc.cores=1)
  Tokenizer <- function(x) NGramTokenizer(x, Weka_control(min = gram, max = gram))
#   corpus <- tm_map(corpus,content_transformer(tolower))
#   corpus <- tm_map(corpus,removePunctuation)
#   corpus <- tm_map(corpus,removeNumbers)
#   corpus <- tm_map(corpus,removeWords,stopwords("english"))
#   corpus <- tm_map(corpus, stripWhitespace)
#   corpus <- tm_map(corpus, stemDocument, language = "english")
  dtm    <- DocumentTermMatrix(corpus, control=list(tokenize=Tokenizer,
                                                    weighting=function(x)weightTfIdf(x,normalize=T)))
  return(dtm)
}


library(tm)
library(SnowballC)
library(RWeka)
library(C50)

combi <- rbind(train_business,test_business)
corpus <- Corpus(VectorSource(combi$new_name))  #cleaned data
dtm1 <- NGram(corpus,1)
dtm2 <- NGram(corpus,2)
Dtm1 <- removeSparseTerms(dtm1,0.999)
Dtm2 <- removeSparseTerms(dtm2,0.999)
Dtm1 <- as.data.frame(as.matrix(Dtm1))
Dtm2 <- as.data.frame(as.matrix(Dtm2))
dtm  <- cbind(Dtm1, Dtm2)
dtm <- as.data.frame(as.matrix(dtm))

train  = cbind(dtm[1:nrow(train_business),],train_business[,9:ncol(train_business)],"stars"=train_business$stars)
test  =  cbind(dtm[-(1:nrow(train_business)),],test_business[,9:ncol(train_business)],"stars"=test_business$stars)

class <- train_business[,c("Shopping","Food")]
labelNames <- colnames(class)
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
