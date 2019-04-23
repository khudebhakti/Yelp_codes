NOTE: Initially all the coding was done in R. Later on, a switch to Python was made. Respective csv's were generated and used in both the software tools.

database_connection.R  : Making connection to MySQL through R and reading JSON objects

jason_flattening.R  : script to extract information from JSON and store in MySQL database

food_shop_filter_data.R     :  filtering data to include only "Food" and "Shopping" category examples. It separates the businesses, reviews, tips, checkins

time_features.R  : Generation of features from time data

Missing_Value_Imputation-3.R : Missing value imputation

food_shop_model3.R : Initial Model building in R

food_shop_pythonClean_model_1.R : Model Building on Python processed data

multi_label_F_measure.R : Evaluation measures 

CVFold_2.R : k-Fold Cross-validation method

Text_cleanup.py : Text cleaning in Python

word2vec_2.py : Word2vec model

Photo_sort.R : Sorting photos as per selected businesses (from JSON object)

photos.sh : Shell script to separate the relevant photos from all photos

photo_train_test_split.R : Making train and test set of images

photo_resize.py : Resizing and normalizing the images

feature_extract.py : Feature extraction using VGG16 model

model.py : Neural network implementation on Images

Python_ensemble.py : Used for final model building and predictions

Python_results_postprocessing.R : Post-processing of results obtained from Python in R (Directly assigning class to empty class, all negative prediction examples) 

Final_results_ensembling.R : Ensembling of predictions from all models



