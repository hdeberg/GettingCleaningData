# This code reads in the data from the Samsung dataset, extracts variables which
#describe the mean or standard deviation of measurements and then computes the mean of 
#each extracted variable

#Read in all of the necessary files from the dataset

labels <-read.table("./UCI HAR Dataset/activity_labels.txt")
features <-read.table("./UCI HAR Dataset/features.txt")

Y_test <-read.table("./UCI HAR Dataset/test/y_test.txt")
X_test <-read.table("./UCI HAR Dataset/test/X_test.txt")
Sub_test <-read.table("./UCI HAR Dataset/test/subject_test.txt")

Y_train <-read.table("./UCI HAR Dataset/train/y_train.txt")
X_train <-read.table("./UCI HAR Dataset/train/X_train.txt")
Sub_train <-read.table("./UCI HAR Dataset/train/subject_train.txt")

#Merge into one dataset, first apply rbind to combine the test and training sets
#in theier current format

X_all <-rbind(X_test, X_train)
Y_all <-rbind(Y_test, Y_train)
Sub_all <-rbind(Sub_test, Sub_train)

#Rename the variable in Sub_all and Y_all
colnames(Sub_all)[1] = "Subject"
colnames(Y_all)[1] = "ActivityNum"

#Search for features that correspond to means and standard deviations

std_id <- grep("std", features$V2)
mean_id <-grep("mean", features$V2)

#Each feature corresponds a variable in X_all. Only these features are extracted
X_subset <- X_all[,sort(c(std_id,mean_id))]

#Add the subject and activity data to the dataset
Data <- cbind(X_subset, Sub_all, Y_all)

#Use merge to add descriptive activity names to the data set
#This will reorder the rows, but since the x data, activities and subject
#have all been combned, this is okay V2.y will be the descriptive variable 
#name of the activity
colnames(labels)[1] = "ActivityNum"
colnames(labels)[2] = "Activity"
Data <- merge (Data, labels, by.x="ActivityNum",by.y="ActivityNum")
DataNames <- names(Data)

#rename variables looping over Data
#Match values in features$V1 to variable names in Data
for (i in 1:length(DataNames) ) {
  if(grepl("[0-9]", DataNames[i])){
    Idx <- as.numeric(gsub("[^0-9]","",DataNames[i]))
    LongName <- features$V2[Idx]
    names(Data)[i]<-as.character(LongName)
  }
}

#Calculates means
library(dplyr)
SummarizedData <- Data %>% group_by(Subject,Activity) %>% summarise_each(funs(mean))

#Remove the ActivityNum variable from Summarized Data as it is unnecessary with the
#Full activity names present in the Activity Variable
SummarizedData$ActivityNum <- NULL

#Write summarized tidy data to a table
write.table(SummarizedData,"CourseProject_TidyData.txt",row.name=FALSE, sep="\t")

