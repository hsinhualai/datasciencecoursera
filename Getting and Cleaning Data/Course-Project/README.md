# Course Project for Getting and Cleaning Data
## The Goal of the Project
You should create one R script called run_analysis.R that does the following. 

1. Merges the training and the test sets to create one data set.

2. Extracts only the measurements on the mean and standard deviation for each measurement. 

3. Uses descriptive activity names to name the activities in the data set

4. Appropriately labels the data set with descriptive variable names. 

5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

## How to run run_analysis.R
1. Download the data from the source link https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip.

2. Extract the zip file to your working directory and you will obtain a folder named 'UCI HAR Dataset'.

3. For running run_analysis.R, you need to change the default setting of the working directory using setwd().

4. Inputting 'source("run_analysis.R") in your working directory containing the 'run_analysis.R' and the folder 'UCI HAR Dataset will generate a file named tidy_data.txt, which is the tidy data we need.