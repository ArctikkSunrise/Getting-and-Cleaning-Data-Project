run_analysis.R
Sun Apr 26 00:36:56 2015
##Data Specialization 03: Getting and Cleaning Data - Project

rm(list=ls())
library(dplyr)
## 
## Attaching package: 'dplyr'
## 
## The following object is masked from 'package:stats':
## 
##     filter
## 
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
library(plyr)
## -------------------------------------------------------------------------
## You have loaded plyr after dplyr - this is likely to cause problems.
## If you need functions from both plyr and dplyr, please load plyr first, then dplyr:
## library(plyr); library(dplyr)
## -------------------------------------------------------------------------
## 
## Attaching package: 'plyr'
## 
## The following objects are masked from 'package:dplyr':
## 
##     arrange, count, desc, failwith, id, mutate, rename, summarise,
##     summarize
library(utils)
WD <- "C:/Users/KK/Documents/Outside Learning/Data Science Specialization/03_Getting and Cleaning Data/project/Project-Data/UCI HAR Dataset"
setwd(WD)

#Read data from test and train folder
subjecttrain <- read.table("./train/subject_train.txt")
xtrain       <- read.table("./train/X_train.txt")
ytrain       <- read.table("./train/Y_train.txt")
subjecttest  <- read.table("./test/subject_test.txt")
xtest        <- read.table("./test/X_test.txt")
ytest        <- read.table("./test/Y_test.txt")

#step1 - Merges the training and the test sets to create one data set
datatrain    <- cbind(subjecttrain,xtrain,ytrain)
datatest     <- cbind(subjecttest,xtest,ytest)
Data         <- rbind(datatrain,datatest)

#step4 - Appropriately labels the data set with descriptive variable names
feature      <- read.table("features.txt",colClasses="character")
colnames(Data) <- c("subject",feature[,2],"activity")

#step2 - Extracts only the measurements on the mean and standard deviation for each measurement.
meandata     <- grep("[Mm][Ee][Aa][Nn]",feature[,2])#measurements on mean
stddata      <- grep("[Ss][Tt][Dd]",feature[,2])#measurements on standard deviation
meanstd      <- sort(c(meandata,stddata))#row of mean and SD measurements
ExtractData  <- Data[,c(1,(meanstd+1),ncol(Data))]#extracts only measurements on mean and SD

#step3 - Uses descriptive activity names to name the activities in the data set
ExtractData  <- ExtractData[order(ExtractData$subject,ExtractData$activity),]#sort data by subject, then activity
ExtractData[,ncol(ExtractData)] <- as.character(ExtractData[,ncol(ExtractData)])
ExtractData[,ncol(ExtractData)] <- gsub("1","WALKING",ExtractData[,ncol(ExtractData)])
ExtractData[,ncol(ExtractData)] <- gsub("2","WALKING_UPSTAIRS",ExtractData[,ncol(ExtractData)])
ExtractData[,ncol(ExtractData)] <- gsub("3","WALKING_DOWNSTAIRS",ExtractData[,ncol(ExtractData)])
ExtractData[,ncol(ExtractData)] <- gsub("4","SITTING",ExtractData[,ncol(ExtractData)])
ExtractData[,ncol(ExtractData)] <- gsub("5","STANDING",ExtractData[,ncol(ExtractData)])
ExtractData[,ncol(ExtractData)] <- gsub("6","LAYING",ExtractData[,ncol(ExtractData)])

#step5 - From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject
#Find row of each subject
subjectrow <- data.frame()
for(s in 1:30){
s1  <- which(ExtractData[,1] == s)
subjectrow[(1:length(s1)),s] <- s1
}

#find the average of each variable for each activity and each subject and combine into 1 dataframe
FinalData <- data.frame()
for(sub in 1:30){
    rowlength<- (sum(!is.na(subjectrow[,sub])))
    datatable<- ExtractData[(subjectrow[1,sub]:(subjectrow[1,sub]+rowlength-1)),]
    DataMean <- ddply(datatable,.(activity),summarise,mean = colMeans(datatable[,2:87]))
    dataname1<- rep(sub,516)
    dataname2<- rep(feature[meanstd,2],6)
    DataMean <- cbind(dataname1,dataname2,DataMean)
    FinalData<- rbind(FinalData,DataMean)
}
colnames(FinalData) <- c("subject","feature","activity","mean")
write.table(FinalData,"C:/Users/KK/Documents/Outside Learning/Data Science Specialization/03_Getting and Cleaning Data/project/FinalData.txt",row.names=FALSE)#export data
