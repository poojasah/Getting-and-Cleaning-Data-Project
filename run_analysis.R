###Getting and Cleaning Data Course Project###

#Checking and creating finalproject directory
if(!file.exists("./finalproject")) {dir.create("./finalproject")}

#Downloading the zip file
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(url,destfile = "./finalproject.zip")

#unzip the downloaded zip file
unzip("./finalproject.zip", exdir="./finalproject")

#Load required packages
library(dplyr)
library(data.table)
library(tidyr)

#Reading the subject 
subjectTrainData <- tbl_df(read.table("./finalproject/UCI HAR Dataset/train/subject_train.txt"))
subjectTestData <- tbl_df(read.table("./finalproject/UCI HAR Dataset/test/subject_test.txt"))

#Reading the data files 
XTrainData <- tbl_df(read.table("./finalproject/UCI HAR Dataset/train/X_train.txt"))
XTestData <- tbl_df(read.table("./finalproject/UCI HAR Dataset/test/X_test.txt"))

#Reading the label files
YLabelTrainData <- tbl_df(read.table("./finalproject/UCI HAR Dataset/train/Y_train.txt"))
YLabelTestData <- tbl_df(read.table("./finalproject/UCI HAR Dataset/test/Y_test.txt"))


###1. MERGE THE TRAINING AND TEST SETS TO CREATE ONE DATA SET

allSubjectData <- rbind(subjectTrainData,subjectTestData)
setnames(allSubjectData, "V1", "subject")

allYLabel <- rbind(YLabelTrainData,YLabelTestData)
setnames(allYLabel, "V1", "YLabel")

allXData <- rbind(XTrainData,XTestData)

# naming column names of the 'allXData' according to feature
dataFeatures <- tbl_df(read.table("./finalproject/UCI HAR Dataset/features.txt"))
setnames(dataFeatures, names(dataFeatures), c("featureNum", "featureName"))
colnames(allXData) <- dataFeatures$featureName

#column names for activity labels
activityLabels<- tbl_df(read.table("./finalproject/UCI HAR Dataset/activity_labels.txt"))
setnames(activityLabels, names(activityLabels), c("activityNum","activityName"))

#merge Columns
allSubjectData <- cbind(allSubjectData, allYLabel)
allData <- cbind(allSubjectData, allXData)

###2.Extract only the measurements on the mean and standard deviation for each measurement.
# Reading "features.txt" and extracting only the mean and standard deviation
dataFeaturesMeanStd <- grep("mean\\(\\)|std\\(\\)",dataFeatures$featureName,value=TRUE)
dataFeaturesMeanStd <- union(c("subject","YLabel"), dataFeaturesMeanStd)
allData <- subset(allData, select = dataFeaturesMeanStd)

###3. Uses descriptive activity names to name the activities in the data set
setnames(allData, "YLabel", "activityNum")
allData <- merge(activityLabels, allData, by="activityNum", all.x = TRUE)
allData$activityName <- as.character(allData$activityName)
dataAggr<- aggregate(. ~ subject - activityName, data = allData, mean) 
allData<- tbl_df(arrange(dataAggr,subject,activityName))

###4.Appropriately label the data set with descriptive variable names
names(allData)<-gsub("^t", "time", names(allData))
names(allData)<-gsub("^f", "frequency", names(allData))
names(allData)<-gsub("Acc", "Accelerometer", names(allData))
names(allData)<-gsub("Gyro", "Gyroscope", names(allData))
names(allData)<-gsub("Mag", "Magnitude", names(allData))
names(allData)<-gsub("BodyBody","Body", names(allData))

###5.From the data set in step 4, creates a second, 
### independent tidy data set with the average of each variable for each activity and each subject.
write.table(allData, "TidyData.txt", row.name=FALSE)