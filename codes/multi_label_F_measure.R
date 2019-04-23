
#code for precision-recall
#class <- mldr_from_dataframe(as.data.frame(test_class),labelIndices = c(match(c("Food","Shopping"),names(test_class))))   #trueclass
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
