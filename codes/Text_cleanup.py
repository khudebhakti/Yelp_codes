# -*- coding: utf-8 -*-
"""
Created on Fri May 20 13:44:18 2016

@author: bhakti khude
"""

path = "/media/sf_D_DRIVE/bhakti/yelp/7th_Challenge/yelp_dataset_challenge_academic_dataset/Restaurant_Shopping"
import os
os.chdir(path)

import pandas as pd

import numpy as np
import nltk
import re
from nltk.corpus import stopwords            # Import the stop word list
from nltk.stem.porter import *
from nltk.stem.wordnet import WordNetLemmatizer
stemmer = PorterStemmer()
lmtzr = WordNetLemmatizer()


business = pd.read_csv("business_mod.csv",sep=",",header=0)

#---------------------------------------Spell check-------------
import re, collections
def words(text): return re.findall('[a-z]+', text.lower()) 

def train(features):
    model = collections.defaultdict(lambda: 1)
    for f in features:
        model[f] += 1
    return model

NWORDS = train(words(file('big.txt').read()))

alphabet = 'abcdefghijklmnopqrstuvwxyz'

def edits1(word):
   splits     = [(word[:i], word[i:]) for i in range(len(word) + 1)]
   deletes    = [a + b[1:] for a, b in splits if b]
   transposes = [a + b[1] + b[0] + b[2:] for a, b in splits if len(b)>1]
   replaces   = [a + c + b[1:] for a, b in splits for c in alphabet if b]
   inserts    = [a + c + b     for a, b in splits for c in alphabet]
   return set(deletes + transposes + replaces + inserts)

def known_edits2(word):
    return set(e2 for e1 in edits1(word) for e2 in edits1(e1) if e2 in NWORDS)

def known(words): return set(w for w in words if w in NWORDS)

def correct(word):
    candidates = known([word]) or known(edits1(word)) or known_edits2(word) or [word]
    return max(candidates, key=NWORDS.get)
  

def Correct(word):
    if d.check(word)==False:
        word = correct(word)
        if d.check(word)==False:
            word = d.suggest(word)[0]
        else:
            word = word
        #word = d.suggest(word)[0]
    else:
        word = word
    return word.lower()
    
import langid
import enchant   #library for spellcheck
d = enchant.Dict("en_US")   #englist us dictionary object
def LangCheck(text):
    for word in text.split():
        if langid.classify(word)[0] == "en":
            return word
        if (langid.classify(word)[0] == "fr" and langid.classify(word)[1] == -2.3902664184570312 ):  #café
            return "cafe"
    else:
        return ""
#------------------------------------------------------------------------------
    
def Lem(word):
    w = lmtzr.lemmatize(word,'n')
    if w == word :    # word has not been changed
       #print "2"       
       w = lmtzr.lemmatize(word,'v')
    return w
    

    
def CleanText(text):
    # 1. Remove non-letters  
        letters_only = re.sub("[^a-zA-Z]", " ", text) 
    # 2. Convert to lower case, split into individual words
        letters_only = letters_only.lower()
        #words = letters_only.split(" ")  
       # letters_only = (" ").join([LangCheck(z) for z in words])  #english checking
        words = nltk.word_tokenize(letters_only)                           
    # 3. In Python, searching a set is much faster than searching
    #   a list, so convert the stop words to a set
        stops = set(stopwords.words("english"))                  
     # 4. Remove stop words
        s = (" ").join([w for w in words if not w in stops])   
     # 5. Perform  stemming 
        if(len(s) > 2):
          s = (" ").join([Correct(z) for z in s.split(" ")])
        s = (" ").join([Lem(z) for z in s.split(" ")]) 
        #s = (" ").join([stemmer.stem(z) for z in s.split(" ")])
     #correct the stemmed word
       # if(len(s) > 1):
        #  s = (" ").join([Correct(z) for z in s.split(" ")])
        #Perform lemmatization
      #  s = (" ").join([Lem(z) for z in s.split(" ")]) #lemmatize for verb form
     # 6. Join the words back into one string separated by spac
        return s 

    

def POSTagging(text):
    #split the review of multiple sentences in single sentence.
    #tag the sentence
    #take the selected tags
    #return the entire review
    tokenizer = nltk.data.load('tokenizers/punkt/english.pickle')
    sentences = tokenizer.tokenize(text.strip())
    clean_sentences = []
    for sent in sentences:    #clean and tag each sentence 
        sent = CleanText(sent)      #clean up
        pos_result = nltk.pos_tag(nltk.word_tokenize(sent))  #pos taggs
        mytags = ['JJ', 'JJR', 'JJS', 'NN', 'NNP', 'NNPS',  'NNS', 'VB','VBD','VBG', 'VBN', \
        'VBP', 'VBZ']
        for tag in pos_result:
            if any(word in tag[1] for word in mytags):
                clean_sentences.append(tag[0])
        sent =(" ").join([stemmer.stem(z) for z in sent.split(" ")])
    return (" ").join(clean_sentences)    #converts to a string
    
    
def ToWords(raw_text):

    clean_data = []
    for i in xrange(0, len(raw_text)):
        if i % 500 == 0:
            print i
        clean_data.append(POSTagging(raw_text.iloc[i]))

    return clean_data




clean_text = ToWords(business.name)
cl = pd.DataFrame(clean_text)
b = pd.concat([cl.reset_index(drop=True),business.reset_index(drop=True)],axis=1)
b=b.rename(columns = {0:'new_name2'})
b.to_csv("business_tfidf_imputed_clean2.csv",index=False)


tip =pd.read_csv("tip_sort.csv",sep=",",header=0)
clean_text = ToWords(tip.text)


tip2 = pd.read_csv("tip_sort_elite.csv",sep=",",header=0)
clean_text2 = ToWords(tip2.text)

cl = pd.DataFrame(clean_text)
b = pd.concat([cl.reset_index(drop=True),tip.reset_index(drop=True)],axis=1)
b=b.rename(columns = {0:'new_text2'})
b.to_csv("tip_sort_clean_onlyEnglish.csv",index=False)

cl2 = pd.DataFrame(clean_text2)
b1 = pd.concat([cl2.reset_index(drop=True),tip2.reset_index(drop=True)],axis=1)
b1=b1.rename(columns = {0:'new_text2'})
b1.to_csv("tip_sort_elite_clean2.csv",index=False)


review =pd.read_csv("review_sort.csv",sep=",",header=0)
clean_text = ToWords(review.text)  #removing off english reviews


review2 = pd.read_csv("review_sort_elite.csv",sep=",",header=0)
clean_text2 = ToWords(review2.text)


cl2 = pd.DataFrame(clean_text2)
b1 = pd.concat([cl2.reset_index(drop=True),review2.reset_index(drop=True)],axis=1)
b1=b1.rename(columns = {0:'new_text2'})
b1.to_csv("review_sort_elite_clean2.csv",index=False)
