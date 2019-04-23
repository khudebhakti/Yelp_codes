# -*- coding: utf-8 -*-
"""
Created on Mon Jun  6 13:11:20 2016

@author: bhakti
"""

import os
path = "/media/bhakti/New Volume/MTECH14/Project/Ensemble"
os.chdir(path)

import pandas as pd
import numpy as np
train_review = pd.read_csv("train_review.csv",sep=",",header=0)
test_review = pd.read_csv("Test_review.csv",sep=",",header=0)

from sklearn.feature_extraction.text import TfidfVectorizer
def Tfidf(sparsity):
    tfv = TfidfVectorizer(max_features=None,  
                      strip_accents='unicode', analyzer='word',token_pattern=r'\w{2,}',\
                      ngram_range=(1, 2), use_idf=0,smooth_idf=0,sublinear_tf=0,\
                      stop_words = 'english',min_df=sparsity)
    return tfv

tfv = Tfidf(0.01)

#train_business = pd.read_csv("train_business.csv",sep=",",header=0)
test_review.isnull().sum()
train_nonull = train_review.dropna()
clean_train = train_nonull.new_text2
test_nonull = test_review.dropna()
clean_test = test_nonull.new_text2

train_data = tfv.fit_transform(clean_train)
train_data = train_data.todense()
train_data = pd.DataFrame(train_data) 
 
test_data = tfv.transform(clean_test)
test_data = test_data.todense()
test_data = pd.DataFrame(test_data)
 
#review
train = pd.concat([train_data.reset_index(drop=True),train_nonull[["stars","votes.cool","votes.useful","votes.funny"]].reset_index(drop=True)],axis=1)
test = pd.concat([test_data.reset_index(drop=True),test_nonull[["stars","votes.cool","votes.useful","votes.funny"]].reset_index(drop=True)],axis=1)
 
#tip
train = pd.concat([train_data.reset_index(drop=True),train_nonull[["likes"]].reset_index(drop=True)],axis=1)
test = pd.concat([test_data.reset_index(drop=True),test_nonull[["likes"]].reset_index(drop=True)],axis=1)

#business
train = pd.concat([train_data.reset_index(drop=True),train_nonull[["stars","Sunday","Monday","Tuesday",\
                   "Wednesday","Thursday","Friday","Saturday","nOpenDays","Morning","Afternoon","Evening","Night",\
                   "attributes.Accepts.CreditCards","attributes.PriceRange","attributes.Parking.garage",\
                   "attributes.Parking.street", "attributes.Parking.validated", "attributes.Parking.lot", "attributes.Parking.valet"]].reset_index(drop=True)],axis=1)

test = pd.concat([test_data.reset_index(drop=True),test_nonull[["stars","Sunday","Monday","Tuesday",\
                   "Wednesday","Thursday","Friday","Saturday","nOpenDays","Morning","Afternoon","Evening","Night",\
                   "attributes.Accepts.CreditCards","attributes.PriceRange","attributes.Parking.garage",\
                   "attributes.Parking.street", "attributes.Parking.validated", "attributes.Parking.lot", "attributes.Parking.valet"]].reset_index(drop=True)],axis=1)

def Rdeepnet(train,train_class,test,hidden_N,nepoch):
    import rpy2.robjects as robjects
    robjects.r('''
    dp <- function(hidden_N,nepoch,train,train_class,test) #train,class,test)
    {
        a<- Sys.time()
        print(hidden_N)
        labelNames = c("Shopping","Food")
        predictions <- matrix(0,nrow=nrow(test), ncol=length(labelNames))
        predictions <- data.frame(predictions)
        #colnames(predictions) <- labelNames
        set.seed(1)
        library(deepnet)
        nn <- nn.train(as.matrix(train),as.matrix(train_class),hidden = hidden_N,numepochs = nepoch)
        predictions <- nn.predict(nn,test)
        predictions <- round(predictions)
        b<- Sys.time()
        print(b-a)
        return(predictions)
    }
    ''')
    r_f = robjects.globalenv['dp']
    #print(r_f.r_repr())
    r_f = robjects.r['dp']
    from rpy2.robjects import pandas2ri
    pandas2ri.activate()
    train = pandas2ri.py2ri(train)   #converts pandas df to R dataframe
    test = pandas2ri.py2ri(test)
    train_class = pandas2ri.py2ri(train_class)
    from rpy2.robjects import numpy2ri        #converts list in python to R vectors
    numpy2ri.activate()
    hidden_N = np.array(hidden_N)
    nepoch = nepoch
    hidden_N = numpy2ri.py2ri(hidden_N)
    predictions =  pandas2ri.ri2py_dataframe(r_f(hidden_N,nepoch,train,train_class,test))
    return predictions
    
def Mldr_evaluation(test_class,predictions):
    import rpy2.robjects as robjects
    robjects.r('''
    f <- function(test_class,predictions)
    {
        library(mldr)
        testclass <- mldr_from_dataframe(as.data.frame(test_class),labelIndices = c(match(c("Shopping","Food"),names(test_class))))   #trueclass
        predictions <- as.data.frame(predictions)
        indx <- sapply(predictions, is.factor)
        predictions[indx] <- lapply(predictions[indx], function(x) as.numeric(as.character(x)))
        pred  <- as.matrix(predictions)  #predicted class
        res<-mldr_evaluate(testclass, pred)
        return(list("H.Loss"=res$HammingLoss,"Acc"=res$Accuracy, "mAUC"=res$MicroAUC, "mFMeas"=res$MicroFMeasure,
           "mPrec"=res$MicroPrecision,"mRec"=res$MicroRecall,"subAcc"=res$SubsetAccuracy))
    }
    ''')
    r_f = robjects.globalenv['f']
    print(r_f.r_repr())
    r_f = robjects.r['f']
    from rpy2.robjects import pandas2ri
    pandas2ri.activate()
    r_test = pandas2ri.py2ri(test_class)   #converts pandas df to R dataframe
    r_pred = pandas2ri.py2ri(predictions)
    res = r_f(r_test,r_pred)
    res = dict(zip(res.names, map(list,list(res))))
    return(res)

train_class = train_nonull[['Shopping','Food']]
test_class = test_nonull[['Shopping','Food']]

predictions = Rdeepnet(train,train_class,train,hidden_N=[100,50],nepoch=300)  
predictions.rename(columns = {0:'Shopping'}, inplace = True)
predictions.rename(columns = {1:'Food'}, inplace = True)
predictions.to_csv("python_Review_train_pred.csv",index=False)

