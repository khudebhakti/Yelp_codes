test_review <- read.csv("Test_review.csv",stringsAsFactors=F)
py_pred <- read.csv("python_Review_word2vec_pred.csv",stringsAsFactors=F)

test_business <- read.csv("Test_business.csv",stringsAsFactors=F)
test_class <- test_business[,c("Shopping","Food")]

#python catches "" as missing alongwith NA
missing_entry <- which(is.na(test_review$new_text2))
missing_name <- which(nchar(test_review$new_text2)==0) #name is "" becoz of cleaning
index <- c(missing_entry,missing_name)
index <- sort(index) #arrange it in proper order for filling

test_rough <-matrix(-100,nrow=nrow(test_review), ncol=2)  #deliberately -100 to see the changes
test_rough <- as.data.frame(test_rough)
colnames(test_rough) <- c("Shopping","Food")
test_rough[-index,] <- py_pred

#fill in the missing value directly
test_rough$Shopping[index] <- 1
test_rough$Food[index] <- 0
 
#check if any entries are 0 0 and replace them by maximum class for tht particular classifier
indx <- which(business$b_Shopping==0 &business$b_Food==0)
business$b_Shopping[indx] <- 1

indx <- which(review$Shopping==0 & review$Food==0)
review$Shopping[indx] <- 1

#no 0 0 for tips

indx <- which(image$i_Shopping==0 & image$i_Food==0)
image$i_Shopping[indx] <- 1

Mldr <- function(test_class,predictions)
{
  library(mldr)
  testclass <- mldr_from_dataframe(as.data.frame(test_class),labelIndices = c(match(c("Shopping","Food"),names(test_class))))   #trueclass
  predictions <- as.data.frame(predictions)
  indx <- sapply(predictions, is.factor)
  predictions[indx] <- lapply(predictions[indx], function(x) as.numeric(as.character(x)))
  pred  <- as.matrix(predictions)  #predicted class
  res<-mldr_evaluate(testclass, pred)
  l = list("H.Loss"=res$HammingLoss,"Acc"=res$Accuracy, "mAUC"=res$MicroAUC, "mFMeas"=res$MicroFMeasure,
           "mPrec"=res$MicroPrecision,"mRec"=res$MicroRecall,"subAcc"=res$SubsetAccuracy)
  return(l)
}

Mldr(test_class,test_rough)
colnames(test_rough) <- c("Shopping","Food")
write.csv(test_rough,"python_Review_word2vec_test_pred_Rcompleted_00_corrected.csv",row.names=F)
