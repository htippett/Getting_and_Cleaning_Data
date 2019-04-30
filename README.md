# Getting_and_Cleaning_Data
Wk4_PeerGradedAssignment

## Course Project
You should create one R script called run_analysis.R that does the following.

Merges the training and the test sets to create one data set.
Extracts only the measurements on the mean and standard deviation for each measurement.
Uses descriptive activity names to name the activities in the data set
Appropriately labels the data set with descriptive variable names.
From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

## How to Work on this Project
1.Download the data source and put into a folder on your local drive. You'll have a UCI HAR Dataset folder.
2.Put run_analysis.R in the parent folder of UCI HAR Dataset, then set it as your working directory using setwd() function in RStudio.
3.Run source("run_analysis.R"), then it will generate a new file tiny_data.txt in your working directory.

## Dependencies
run_analysis.R file will help you to install the dependencies automatically. It depends on reshape2 and data.table. 
