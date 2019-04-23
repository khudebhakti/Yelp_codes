setwd("/media/bhakti/New Volume/MTECH14/Project/Ensemble")

business <- read.csv("business_tfidf_imputed_clean2.csv",stringsAsFactors =F)
business_with_photo = read.csv("photo_sort.csv",stringsAsFactors =F)

b <- business[,c("business_id","Shopping","Food")]
business_with_photo <- merge(x=business_with_photo,y=b,by="business_id",all.x=T,all.Y=F) #bind the class
business_with_photo = as.data.frame(business_with_photo)

train_business = business$business_id[1:13500]
test_business = business$business_id[13501:nrow(business)]


train.ind <- which(business_with_photo$business_id %in% train_business) 
train_photo <- business_with_photo[train.ind,] 
test_photo  <- business_with_photo[-train.ind,]

write.csv(train_photo,"train_photo.csv",row.names=F)
write.csv(test_photo,"test_photo.csv",row.names=F)
#Arranging the names
jpg <- paste(train_photo$photo_id,".jpg",sep="")
write.table(jpg,"train_photo.txt",row.names = F,col.names = F)
jpg <- paste(test_photo$photo_id,".jpg",sep="")
write.table(jpg,"test_photo.txt",row.names = F,col.names = F)

#writing only classes
write.csv(train_photo[,c("Shopping","Food")],"train_photo_class.csv",row.names=F)
write.csv(test_photo[,c("Shopping","Food")],"test_photo_class.csv",row.names=F)

y_train <- read.csv("train_photo.csv",stringsAsFactors=F)
y_test <- read.csv("test_photo.csv",stringsAsFactors=F)
y_train <- y_train[,c("Shopping","Food")]
y_test <- y_test[,c("Shopping","Food")]
write.csv(y_train,"y_train.csv",row.names=F)
write.csv(y_test,"y_test.csv",row.names=F)
