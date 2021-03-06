# -*- coding: utf-8 -*-
"""
Created on Thu Jun  2 08:29:12 2016

@author: bhakti
"""

path = "/media/bhakti/New Volume/MTECH14/Project/Ensemble"
import os
os.chdir(path)

import pandas as pd
import numpy as np
import nltk
from bs4 import BeautifulSoup
import re
import rpy2
train_business = pd.read_csv("train_business.csv",sep=",",header=0)
train_review = pd.read_csv("train_review.csv",sep=",",header=0)
test_business = pd.read_csv("Test_business.csv",sep=",",header=0)
test_review = pd.read_csv("Test_review.csv",sep=",",header=0)
#train_tip = pd.read_csv("train_tip.csv",sep=",",header=0)

def review_to_wordlist( review ):
    #review = BeautifulSoup(review).get_text()
    #review_text = re.sub("[^a-zA-Z]"," ", review_text)
    words = review.split()
    return(words)
    
tokenizer = nltk.data.load('tokenizers/punkt/english.pickle')

# Define a function to split a review into parsed sentences
def review_to_sentences( review , tokenizer):

    raw_sentences = tokenizer.tokenize(review.strip())
    # 2. Loop over each sentence
    sentences = []
    for raw_sentence in raw_sentences:
        # If a sentence is empty, skip it
        if len(raw_sentence) > 0:
            # Otherwise, call review_to_wordlist to get a list of words
            sentences.append( review_to_wordlist( raw_sentence ))
    # Return the list of sentences (each sentence is a list of words,
    # so this returns a list of lists
    return sentences
    
test_review.isnull().sum()
train_nonull = train_review.dropna()
clean_train = train_nonull.new_text2
test_nonull = test_review.dropna()
clean_test = test_nonull.new_text2
    
clean_train = clean_train.tolist()
clean_test = clean_test.tolist()
clean_train = [unicode(s) for s in clean_train]
clean_test  = [unicode(s) for s in clean_test]

print "Parsing sentences from training set"    
i=0    
review_words = []
for text in clean_train:
    print i
    review_words += review_to_sentences(str(text), tokenizer)
    i = i+1
sentences = review_words


# Import the built-in logging module and configure it so that Word2Vec 
# creates nice output messages
import logging
logging.basicConfig(format='%(asctime)s : %(levelname)s : %(message)s',level=logging.INFO)

# Set values for various parameters
num_features = 100  # Word vector dimensionality                      
min_word_count = 1   # Minimum word count                    
num_workers = 4     # Number of threads to run in parallel
context = 10           # Context window size                                                                                   
batch_words = 1000

# Initialize and train the model (this will take some time)
from gensim.models import word2vec
print "Training model..."
model = word2vec.Word2Vec(sentences, workers=num_workers, sg=1,hs=1, \
            size=num_features, min_count = min_word_count, \
            window = context, batch_words = batch_words)

# If you don't plan to train the model any further, calling 
# init_sims will make the model much more memory-efficient.
model.init_sims(replace=True)

# It can be helpful to create a meaningful model name and 
# save the model for later use. You can load it later using Word2Vec.load()
model_name = "Review_100features_1minwords_10context_1000batchwords"
model.save(model_name)

#####
from gensim.models import Word2Vec
model = Word2Vec.load("Review_100features_1minwords_10context_1000batchwords")

import numpy as np  # Make sure that numpy is imported

def makeFeatureVec(words, model, num_features):
    # Function to average all of the word vectors in a given paragraph
    # Pre-initialize an empty numpy array (for speed)
    featureVec = np.zeros((num_features,),dtype="float32")
    nwords = 0.
    # Index2word is a list that contains the names of the words in 
    # the model's vocabulary. Convert it to a set, for speed 
    index2word_set = set(model.index2word)
    # Loop over each word in the review and, if it is in the model's
    # vocaublary, add its feature vector to the total
    for word in words:
        if word in index2word_set: 
            nwords = nwords + 1.
            featureVec = np.add(featureVec,model[word])
    # Divide the result by the number of words to get the average
    featureVec = np.divide(featureVec,(nwords+1))
    return featureVec


def getAvgFeatureVecs(reviews, model, num_features):
    # Given a set of reviews (each one a list of words), calculate 
    # the average feature vector for each one and return a 2D numpy array 
    # Initialize a counter
    counter = 0.
    # Preallocate a 2D numpy array, for speed
    reviewFeatureVecs = np.zeros((len(reviews),num_features),dtype="float32")
    # Loop through the reviews
    for review in reviews:
       # Print a status message every 1000th review
       if counter%1000. == 0.:
           print "Review %d of %d" % (counter, len(reviews))
       # Call the function (defined above) that makes average feature vectors
       reviewFeatureVecs[counter] = makeFeatureVec(review, model, num_features)
       # Increment the counter
       counter = counter + 1.
    return reviewFeatureVecs
    
    # ****************************************************************
# Calculate average feature vectors for training and testing sets,
# using the functions we defined above. Notice that we now use stop word
# removal.

clean_train_reviews = []
for review in clean_train:
    clean_train_reviews.append( review_to_wordlist( review ))

trainDataVecs = getAvgFeatureVecs( clean_train_reviews, model, num_features )

print "Creating average feature vecs for test reviews"
clean_test_reviews = []
for review in clean_test:
    clean_test_reviews.append( review_to_wordlist( review ))

testDataVecs = getAvgFeatureVecs( clean_test_reviews, model, num_features )

train_class = train_nonull[['Shopping','Food']]
test_class = test_nonull[['Shopping','Food']]


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
        return(list('HLoss'=res$HammingLoss,'Acc'=res$Accuracy,'AUC'=res$AUC,'F1'=res$FMeasure,'Prec'=res$Precision,'Rec'=res$Recall,
                    'mAUC'=res$MicroAUC, 'mF1'=res$MicroFMeasure, 'mPrec'=res$MicroPrecision, 'mRec'=res$MicroRecall,'subAcc'=res$SubsetAccuracy))
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

from sklearn.ensemble import RandomForestClassifier
forest = RandomForestClassifier( n_estimators = 500 )
forest = forest.fit( trainDataVecs, train_class )
predictions = pd.DataFrame(forest.predict( testDataVecs ))
Mldr_evaluation(test_class,predictions)

# generate csv of predictions
predictions.rename(columns = {0:'Shopping'}, inplace = True)
predictions.rename(columns = {1:'Food'}, inplace = True)
predictions.to_csv("python_Review_word2vec_pred.csv",index=False)
