
library("reshape")
library("dplyr")


getData = function (){

  #read data
  testData = read.table("test/X_test.txt")
  trainData = read.table("train/X_train.txt")
  
  #read activity id
  testActivity = read.table("test/y_test.txt")
  trainActivity = read.table("train/y_train.txt")
  
  #read activity labels
  activityLabels = read.table("activity_labels.txt")
  
  
  #read subject labels
  testSubject = read.table("test/subject_test.txt")
  trainSubject = read.table("train/subject_train.txt")
  
  #add subject, activity and activity lables to data to data
  testData = cbind(testSubject,  activityLabels$V2[testActivity[,1]],   testData)
  trainData = cbind(trainSubject,  activityLabels$V2[trainActivity[,1]], trainData)
  
  #assign colnames
  featureLabels = as.vector(read.table("features.txt")[,2])
  
  colnames(testData) = c ("subject", "activity", featureLabels)
  colnames(trainData) = c ("subject", "activity", featureLabels)
  
  
  #combine all data
  allData = rbind(testData, trainData)
  
  #keep only mean and std (not meanfreq)
  colIDs = (grepl("mean", featureLabels) | grepl("std", featureLabels)) & (!grepl("meanFreq", featureLabels))
  #keep first 2(subject, activityl) as well
  colIDs =c (TRUE, TRUE, colIDs)
  allData = allData[, colIDs]
  
  #make colnames nicer
  newColNames = gsub("-","_", tolower(colnames(allData)))
  newColNames = gsub("\\(", "", newColNames)
  newColNames = gsub("\\)", "", newColNames)
  colnames(allData) = newColNames
  
  
  #sort by subject, activity
  allData = arrange(allData, subject, activity)
  
  #summary by subject/activity
  melted = melt(allData, id=c("subject", "activity"))
  avgData = aggregate(melted, list(melted$subject, melted$activity, melted$variable) , mean)
  avgData = avgData[,c(1,2,3,7)]
  colnames(avgData) = c("subject", "activity", "variable", "average")
  return (avgData)
}
  
  
