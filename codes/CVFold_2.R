dp <- function(hidden_N,nepoch,train,class,test)
{
  a<- Sys.time()
  print(hidden_N)
  predictions <- matrix(0,nrow=nrow(test), ncol=length(labelNames))
  predictions <- data.frame(predictions)
  colnames(predictions) <- labelNames
  set.seed(1)
  library(deepnet)
  nn <- nn.train(as.matrix(train),as.matrix(class),hidden = hidden_N,numepochs = nepoch)
  predictions <- nn.predict(nn,test)
  predictions <- round(predictions)
  b<- Sys.time()
  print(b-a)
  return(predictions)
}

CVFold <- function(data,hidden_N=0,nepoch=0)
{
  count=1
  H.Loss <- c()
  Acc  <- c()
  AUC  <- c()
  Fmeas <- c()
  Prec <- c()
  Rec  <- c()
  mAUC  <- c()
  mFmeas <- c()
  mPrec <- c()
  mRec  <- c()
  
  #Acc2 <- c()
  for(i in 1:k)
  {
    print(i)
    if(i != k)
    {
      print("In 1")
      test <- data[(count:(step*i)),]
      test_class <- test[,c("Shopping","Food")]
      train <- data[-as.numeric(row.names(test)),]
      class <- train[,c("Shopping","Food")]
    }
    if(i == k)
    {
      print("In 2")
      test <- data[count:nrow(data),]
      test_class <- test[,c("Shopping","Food")]
      train <- data[-as.numeric(row.names(test)),]
      class <- train[,c("Shopping","Food")]
      
    }
    
    print(dim(test))
    #class$Shopping <- factor(class$Shopping)
    #class$Food <- factor(class$Food)
    test$Food <- test$Shopping <- train$Food <- train$Shopping <- NULL
    labelNames <- colnames(class)
    colnames(test_class) <- labelNames
    
    print("in 3")
    predictions <- matrix(0,nrow=nrow(test), ncol=length(labelNames))
    predictions <- as.data.frame(predictions)
    print(dim(predictions))
    colnames(predictions) <- labelNames
    
    #Run the model
    predictions <- dp(hidden_N,nepoch,train,class,test)
    
    #Evaluation 
    library(mldr)
    testclass <- mldr_from_dataframe(as.data.frame(test_class),labelIndices = c(match(c("Shopping","Food"),names(test_class))))   #trueclass
    predictions <- as.data.frame(predictions)
    indx <- sapply(predictions, is.factor)
    predictions[indx] <- lapply(predictions[indx], function(x) as.numeric(as.character(x)))
    pred  <- as.matrix(predictions)  #predicted class
    res<-mldr_evaluate(testclass, pred)
    #str(res)
    H.Loss[i]  <-  res$HammingLoss
    Acc[i]     <-  res$Accuracy
    AUC[i]     <-  res$AUC
    Fmeas[i]   <-  res$FMeasure
    Prec[i]    <-  res$Precision
    Rec[i]     <-  res$Recall
    mAUC[i]    <-  res$MicroAUC
    mFmeas[i]  <-  res$MicroFMeasure
    mPrec[i]   <-  res$MicroPrecision
    mRec[i]    <-  res$MicroRecall
    #Acc[i]<- sum(test_class != predictions)/(nrow(test_class)*ncol(test_class))  #Hamming-Loss
    #Acc2[i]<- sum(test_class != predictions1)/(nrow(test_class)*ncol(test_class))  #Hamming-Loss
    count=(count+step)
  }
  return(list('H.Loss'=H.Loss,'Acc'=Acc,'AUC'=AUC, 'FMeas'=Fmeas,'Prec'=Prec,'Rec'=Rec,
              'mAUC'=mAUC, 'mFMeas'=mFmeas,'mPrec'=mPrec,'mRec'=mRec))
}

