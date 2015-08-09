# CODEBOOK FOR RUN.ANALYSIS.R PROGRAM

This code book describeds the variables, the data, and any transformations or working that you performed to clean up the data
  
## The data source for the project
The data source is from the link https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 

Extracting the zip file gives the data folder named "UCI HAR Dataset""

The UCI HAR Dataset contains the raw data which we are asked to clean up

The UCI HAR Dataset folder includes the following files:

- 'README.txt'

- 'features_info.txt': Shows information about the variables used on the feature vector.

- 'features.txt': List of all features.

- 'activity_labels.txt': Links the class labels with their activity name.

- 'train/X_train.txt': Training set.

- 'train/y_train.txt': Training labels.

- 'test/X_test.txt': Test set.

- 'test/y_test.txt': Test labels.

   The following files are available for the train and test data. Their descriptions are equivalent. 

- 'train/subject_train.txt': Each row identifies the subject who performed the activity for each window sample. Its range is from 1 to 30.

- 'train/Inertial Signals/total_acc_x_train.txt': The acceleration signal from the smartphone accelerometer X axis in standard gravity units 'g'. Every row shows a 128 element vector. The same description applies for the 'total_acc_x_train.txt' and 'total_acc_z_train.txt' files for the Y and Z axis. 

- 'train/Inertial Signals/body_acc_x_train.txt': The body acceleration signal obtained by subtracting the gravity from the total acceleration. 

- 'train/Inertial Signals/body_gyro_x_train.txt': The angular velocity vector measured by the gyroscope for each window sample. The units are radians/second. 


## The experiments conducted
The experiments have been carried out with a group of 30 volunteers within an age bracket of 19-48 years. Each person performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) on the waist. Using its embedded accelerometer and gyroscope, we captured 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz. The experiments have been video-recorded to label the data manually. The obtained dataset has been randomly partitioned into two sets, where 70% of the volunteers was selected for generating the training data and 30% the test data. 

The sensor signals (accelerometer and gyroscope) were pre-processed by applying noise filters and then sampled in fixed-width sliding windows of 2.56 sec and 50% overlap (128 readings/window). The sensor acceleration signal, which has gravitational and body motion components, was separated using a Butterworth low-pass filter into body acceleration and gravity. The gravitational force is assumed to have only low frequency components, therefore a filter with 0.3 Hz cutoff frequency was used. From each window, a vector of features was obtained by calculating variables from the time and frequency domain. 

For each record it is provided:

- Triaxial acceleration from the accelerometer (total acceleration) and the estimated body acceleration.

- Triaxial Angular velocity from the gyroscope. 

- A 561-feature vector with time and frequency domain variables. 

- Its activity label. 

- An identifier of the subject who carried out the experiment.

# The variables (column names) in the tidy_data 
There are 81 variables (column names).

 - 'ID': The IDentification numbers ( ranging from 1 to 30) for the 30 volunteers 

 - 'Activity': Activity Labels--LAYING, SITTING, STANDING, WALKING, WALKING_DOWNSTAIRS, AND WALKING_UPSTAIRS in alphabetical order 
 
The rest of 79 variables include the mean values (mean) and the standard deviation (std) of the following list

- 'tBodyAcc-XYZ' : The body acceleration signals
- 'tGravityAcc-XYZ': The gravity acceleration signalsJerk signals 
- 'tBodyAccJerk-XYZ': The body acceleration Jerk signal
- 'tBodyGyroJerk-XYZ': The body gyro Jerk signal 
- 'tBodyAccMag': Euclidean norm 
- 'tGravityAccMag': Euclidean norm 
- 'tBodyAccJerkMag': Euclidean norm 
- 'tBodyGyroMag': Euclidean norm 
- 'tBodyGyroJerkMag': Euclidean norm 
- 'fBodyAcc-XYZ': Signals from applying Fast Fourier Transform (FFT) of 'tBodyAcc-XYZ'
- 'fBodyAccJerk-XYZ':  Signals from applying Fast Fourier Transform (FFT) of 'tBodyAccJerk-XYZ'
- 'fBodyGyro-XYZ':  Signals from applying Fast Fourier Transform (FFT) of 'tBodyGyro-XYZ'
- 'fBodyAccJerkMag':  Signals from applying Fast Fourier Transform (FFT) of 'tBodyAccJerkMag'
- 'fBodyGyroMag':  Signals from applying Fast Fourier Transform (FFT) of 'tBodyGyroMag'
- 'fBodyGyroJerkMag':  Signals from applying Fast Fourier Transform (FFT) of 'tBodyGyroJerkMag'

## The detailed procedure in run_analysis.R
1. We use data.table and reshape2 library.

2. We first focus on the test folder and load the subject ID list, Activity list (y_test), test results (X_test), the column names for the test_restuls (features), and the activity label (Act_Label).

3. After assigning each column its proper column name, we use cbind to column combine the test data.

4. Repeat the same procedure for the train data.

5. Use rbind to row combine the test and train data

6. In order to calculate the average of each activity for each ID, we use melt to form a long data table whose id = "ID", "Activity", and the rest of teh columns are variables

7. We then use dcast to bring the long data table back to the usual form along with calculating the average of each variable for each activity and each subject.

8. We finally export the tidy_data.txt