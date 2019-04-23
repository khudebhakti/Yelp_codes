#!/bin/sh
echo "Please enter name of the file  containing image names"
read names
echo "Please enter name of the folder containing all images"
read source
echo "Please enter name of the folder where you want to copy your files"
read dest
a=`wc -l /media/bhakti/New\ Volume/MTECH14/Project/Ensemble/$names | awk '{print $1}'`
for i in `seq 1 $a`; 
do 
var=$(sed "${i}q;d" /media/bhakti/New\ Volume/MTECH14/Project/Ensemble/$names | sed 's/"//g')
echo $var
cp /media/bhakti/New\ Volume/MTECH14/Project/Ensemble/$source/$var /media/bhakti/New\ Volume/MTECH14/Project/Ensemble/$dest/ 
done
