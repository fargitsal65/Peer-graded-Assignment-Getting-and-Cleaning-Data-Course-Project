library(dplyr)

#Downloading and unzipping dataset

if(!file.exists("./data")){dir.create("./data")}
fileUrl="https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileUrl,destfile="./data/Dataset.zip")
unzip(zipfile="./data/Dataset.zip",exdir="./data")

#Reading trainings tables:

x_train=read.table("./data/UCI HAR Dataset/train/X_train.txt")
y_train=read.table("./data/UCI HAR Dataset/train/y_train.txt")
subject_train=read.table("./data/UCI HAR Dataset/train/subject_train.txt")

#Reading testing tables:

x_test=read.table("./data/UCI HAR Dataset/test/X_test.txt")
y_test=read.table("./data/UCI HAR Dataset/test/y_test.txt")
subject_test=read.table("./data/UCI HAR Dataset/test/subject_test.txt")

#Reading feature vector:

features=read.table('./data/UCI HAR Dataset/features.txt')

#Reading activity labels:

activityLabels=read.table('./data/UCI HAR Dataset/activity_labels.txt')

#Assigning column names:

colnames(x_train)=features[,2]
colnames(y_train)="activityId"
colnames(subject_train)="subjectId"
colnames(x_test)=features[,2] 
colnames(y_test)="activityId"
colnames(subject_test)="subjectId"
colnames(activityLabels)=c('activityId','activityType')

#1. Merges the training and the test sets to create one data set.

merge_train=cbind(y_train, subject_train, x_train)
dim(merge_train)
merge_test=cbind(y_test, subject_test, x_test)
dim(merge_test)
merge_data=rbind(merge_train, merge_test)
dim(merge_data)

#2. Extracts only the measurements on the mean and standard deviation for each measurement.

mean_and_std <- (grepl("activityId" , colnames(merge_data)) | 
                   grepl("subjectId" , colnames(merge_data)) | 
                   grepl("mean" , colnames(merge_data)) | 
                   grepl("std" , colnames(merge_data)) 
)
extractMeanAndStd <- merge_data[ , mean_and_std == TRUE]
dim(extractMeanAndStd)
#3. Uses descriptive activity names to name the activities in the data set.

setWithActivityNames=merge(extractMeanAndStd, activityLabels,
                           by='activityId',
                           all.x=TRUE)
dim(setWithActivityNames)
#4. Appropriately labels the data set with descriptive variable names.

names(setWithActivityNames)<-gsub("Acc", "Accelerometer", names(setWithActivityNames))
names(setWithActivityNames)<-gsub("Gyro", "Gyroscope", names(setWithActivityNames))
names(setWithActivityNames)<-gsub("BodyBody", "Body", names(setWithActivityNames))
names(setWithActivityNames)<-gsub("Mag", "Magnitude", names(setWithActivityNames))
names(setWithActivityNames)<-gsub("^t", "Time", names(setWithActivityNames))
names(setWithActivityNames)<-gsub("^f", "Frequency", names(setWithActivityNames))
names(setWithActivityNames)<-gsub("tBody", "TimeBody", names(setWithActivityNames))
names(setWithActivityNames)<-gsub("-mean()", "Mean", names(setWithActivityNames),ignore.case = TRUE)
names(setWithActivityNames)<-gsub("-std()", "STD", names(setWithActivityNames),ignore.case = TRUE)
names(setWithActivityNames)<-gsub("-freq()", "Frequency", names(setWithActivityNames),ignore.case = TRUE)
names(setWithActivityNames)<-gsub("angle", "Angle", names(setWithActivityNames))
names(setWithActivityNames)<-gsub("gravity", "Gravity", names(setWithActivityNames))

#5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

tidyData <- setWithActivityNames %>%
  group_by(subjectId, activityId) %>%
  summarise_all(mean)
dim(tidyData)
write.table(tidyData, "tidyData.txt", row.name=FALSE)
str(tidyData)