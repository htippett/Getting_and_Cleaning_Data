#Coursera Getting and Cleaning Data Course, Week4 Peer-graded Assignment
#Project Description: 
# The purpose of this project is to demonstrate your ability to collect, work with, and clean a data set. 
#The goal is to prepare tidy data that can be used for later analysis. You will be graded by your peers on a series 
#of yes/no questions related to the project. You will be required to submit: 
#1) a tidy data set as described below, 
#2) a link to a Github repository with your script for performing the analysis, 
#3) a code book that describes the variables, the data, and any transformations or work that you performed to clean 
#up the data called CodeBook.md. You should also include a README.md in the repo with your scripts. This repo explains 
#how all of the scripts work and how they are connected.
# 
# One of the most exciting areas in all of data science right now is wearable computing - see for example this article . Companies like Fitbit, Nike, and Jawbone Up are racing to develop the most advanced algorithms to attract new users. The data linked to from the course website represent data collected from the accelerometers from the Samsung Galaxy S smartphone. A full description is available at the site where the data was obtained:
#   
#   http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones
# 
# Here are the data for the project:
#   
#   https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
# 
# You should create one R script called run_analysis.R that does the following.
# 
# a) Merges the training and the test sets to create one data set.
# b) Extracts only the measurements on the mean and standard deviation for each measurement.
# c) Uses descriptive activity names to name the activities in the data set
# d) Appropriately labels the data set with descriptive variable names.
# e) From the data set in step d, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
# Good luck!
#-------------------------------------------------------------------------------------------------------------------

#download needed packages
if(!require("data.table")) {
  install.packages("data.table")
}

if(!require("reshape2")) {
  install.packages("reshape2")
}

require("data.table")
require("reshape2")

#Set current working directory

setwd("C:/Users/clicq573/Documents/datasciencecoursera/ProgAssignment_GettingAndCleaning/")

#1) Create a tidy data set as described above
#a) Merge the training and the test sets to create one data set
##reading the data in
features <- read.table("./features.txt",header=FALSE)
activity_labels <- read.table("./activity_labels.txt",header=FALSE)
subjectTrain <-read.table("./train/subject_train.txt",header=FALSE)
x_Train <-read.table("./train/X_train.txt",header=FALSE)
y_Train <-read.table("./train/y_train.txt",header=FALSE)
subjectTest <-read.table("./test/subject_test.txt",header=FALSE)
x_Test <-read.table("./test/X_test.txt",header=FALSE)
y_Test <-read.table("./test/y_test.txt",header=FALSE)

##assigning column names
colnames(activity_labels) <- c("activityID","activityType")
colnames(subjectTrain) <- "subjectID"
colnames(x_Train) <- features[,2]
colnames(y_Train) <- "activityID"
colnames(subjectTest) <- "subjectID"
colnames(x_Test) <- features[,2]
colnames(y_Test) <- "activityID"

##merging the data
trainData <- cbind(y_Train, subjectTrain, x_Train)
testData <- cbind(y_Test,subjectTest,x_Test)
MergedData <- rbind(trainData,testData)

#b) Extract only the measurements on the mean and standard deviation for each measurement.
data_extract <- grep1("mean\\(\\)", names(MergedData)) | grep1("std\\(\\)", names(MergedData))
data_extract[1:2] <- TRUE
MergedData <- MergedData[, data_extract]

#c) Use descriptive activity names to name the activities in the data set
MergedData$activityID <- factor(MergedData$activityID, labels=c("Walking","Walking_Upstairs","walking_Downstairs",
                                                                "Sitting","Standing","Laying"))

#d) Appropriately labels the data set with descriptive variable names.
##tidy feature names by removing "()" at the end

TidyNames <- names(MergedData)
TidyNames <- gsub("[(][)]","",TidyNames)
TidyNames <- gsub("^t","Time_", TidyNames)
TidyNames <- gsub("^f","Frequency_",TidyNames)
TidyNames <- gsub("Acc","Accelerometer", TidyNames)
TidyNames <- gsub("Gyro","Gyroscope", TidyNames)
TidyNames <- gsub("Mag","Magnitude", TidyNames)
TidyNames <- gsub("-mean-","_Mean_", TidyNames)
TidyNames <- gsub("-std-","_StandardDeviation_", TidyNames)
TidyNames <- gsub("-","_", TidyNames)
names(MergedData) <- TidyNames

# e) From the data set in step d, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
melted <- melt(MergedData, id=c("subjectID","activityID"))
tidy <- dcast(melted, subjectID+activityID ~ variable, mean)
write.csv(tidy, "tidy.csv", row.names=FALSE)







