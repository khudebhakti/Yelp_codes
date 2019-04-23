# -*- coding: utf-8 -*-
"""
Created on Sun May 22 18:06:13 2016

@author: bhakti
"""

import os
#os.chdir("/media/sf_D_DRIVE/bhakti/yelp/7th_Challenge/yelp_dataset_challenge_academic_dataset/Restaurant_Shopping/photo_sort")
#path = "/media/bhakti/New Volume/MTECH14/Project/image_processing"
path ='/media/bhakti/New Volume/MTECH14/Project/Ensemble/'
os.chdir(path)
import cv2
import numpy as np
import pandas as pd
l1 = pd.read_csv("train_photo.txt",header=None)
l2 = pd.read_csv("test_photo.txt",header=None)
path ='/media/bhakti/New Volume/MTECH14/Project/Ensemble/photo_sort'
os.chdir(path)

#Normalization    
#resize training images
count=0
train_images = []
for name in l1[0]:
    print count
    image = cv2.resize(cv2.imread(str(name)), (224, 224)).astype(np.float32)
    #image = cv2.resize(image,(224,224))
    image[:,:,0] -= 103.939
    image[:,:,1] -= 116.779
    image[:,:,2] -= 123.68
    image = image.transpose((2,0,1))
    train_images.append(image)
    count = count+1
    
train_images = np.array(train_images)

#resize testing images
count=0
test_images = []
for name in l2[0]:
    print count
    image = cv2.resize(cv2.imread(str(name)), (224, 224)).astype(np.float32)
    #image = cv2.resize(image,(224,224))
    image[:,:,0] -= 103.939
    image[:,:,1] -= 116.779
    image[:,:,2] -= 123.68
    #image = cv2.resize(image,(224,224))
    image = image.transpose((2,0,1))
    test_images.append(image)
    count = count+1
    
test_images = np.array(test_images)

path ='/media/bhakti/New Volume/MTECH14/Project/Ensemble/'
os.chdir(path)
np.save("train_224_resize_normalize.npy",train_images)
np.save("test_224_resize_normalize.npy",test_images)
