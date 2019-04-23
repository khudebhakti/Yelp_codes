#photo_id = read.csv("photo_id_to_business_id")  #200000
#business_with_photo = photo_id[(photo_id$business_id %in% business$business_id),]   #choosing the photo having the same business id
business <- read.csv("business_tfidf_imputed_clean2.csv",stringsAsFactors =F)
business_with_photo = read.csv("business_with_photo.csv")
#37921
#9026 unique business id have images out of 16864 
#many of these photos do not have labels so choosing photos with labels
#4642 for Shopping
#4707 for Food
#323  for Shopping and Food
#caption = business_with_photo[(business_with_photo$caption != ""),]    #5848 images have captions

jpg <- paste(business_with_photo$photo_id,".jpg",sep="")


photo_sort <- c()   #Taking only one image per business id
for(i in 1:nrow(business))
{
   print(i)
   r = which(business_with_photo$business_id == business$business_id[i])
   if(length(r)>0)
     {
       if(length(r)>1)
       {
         set.seed(1)
         r = sample(r,size = 1)
       }
       photo_sort = rbind(photo_sort,business_with_photo[r,])
     }
}

write.csv(photo_sort,"photo_sort.csv",row.names=F)
jpg <- paste(photo_sort$photo_id,".jpg",sep="")
write.table(jpg,"photo_sort.txt",row.names=F)

for(i in 1:nrow(business))
{
  print(i)
  r <- which(review$business_id == business$business_id[i])
  if(length(r)>5){
    r <- sample(r, size=5)
  }
  review_sort <- rbind(review_sort,review[r,])