business <- read.csv("python_Business_test_pred_Rcompleted_00_corrected.csv",stringsAsFactors=F)
review <- read.csv("python_Review_test_pred_Rcompleted_00_corrected.csv",stringsAsFactors=F)
#tip <- read.csv("python_Tip_0.995_test_pred_Rcompleted.csv",stringsAsFactors=F)
#image <- read.csv("python_Image_test_pred_Rcompleted_00_corrected.csv",stringsAsFactors=F)
image <- read.csv("normalize_image_test_prediction.csv",stringsAsFactors=F)
rev_word2vec <- read.csv("python_Review_word2vec_test_pred_Rcompleted_00_corrected.csv",stringsAsFactors=F)

test_business <- read.csv("Test_business.csv",stringsAsFactors=F) #for ground truth
test_class <- test_business[,c("Shopping","Food")]


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


labelNames <- c("Shopping","Food")
e <- matrix(-1,nrow=nrow(business),ncol=length(labelNames))
e <- as.data.frame(e)
colnames(e) <- labelNames
for(i in 1:nrow(business))
{
  e$Shopping[i] <- mean(c(business$b_Shopping[i],review$Shopping[i],image$i_Shopping[i],rev_word2vec$Shopping[i]))
  e$Food[i]     <- mean(c(business$b_Food[i],review$Food[i],image$i_Food[i],rev_word2vec$Food[i]))
}

e <- round(e)
Mldr(test_class = test_class,predictions = e )
