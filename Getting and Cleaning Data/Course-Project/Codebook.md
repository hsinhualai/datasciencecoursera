# CODEBOOK FOR RUN.ANALYSIS.R PROGRAM

This code book describeds the variables, the data, and any transformations or working that you performed to clean up the data
  
## The data source for the project
The data source is from the link https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 

Extracting the zip file gives the data folder named "UCI HAR Dataset""

The UCI HAR Dataset contains the raw data which we are asked to clean up

The UCI HAR Dataset folder includes the following files:

- **'README.txt'**

- **'features_info.txt':** Shows information about the variables used on the feature vector.

- **'features.txt':** List of all features.

- **'activity_labels.txt':** Links the class labels with their activity name.

- **'train/X_train.txt':** Training set.

- **'train/y_train.txt':** Training labels.

- **'test/X_test.txt':** Test set.

- **'test/y_test.txt':** Test labels.

   The following files are available for the train and test data. Their descriptions are equivalent. 

- **'train/subject_train.txt':** Each row identifies the subject who performed the activity for each window sample. Its range is from 1 to 30.

- **'train/Inertial Signals/total_acc_x_train.txt':** The acceleration signal from the smartphone accelerometer X axis in standard gravity units 'g'. Every row shows a 128 element vector. The same description applies for the 'total_acc_x_train.txt' and 'total_acc_z_train.txt' files for the Y and Z axis. 

- **'train/Inertial Signals/body_acc_x_train.txt':** The body acceleration signal obtained by subtracting the gravity from the total acceleration. 

- **'train/Inertial Signals/body_gyro_x_train.txt':** The angular velocity vector measured by the gyroscope for each window sample. The units are radians/second. 


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
There are 68 variables (column names).

 - **'Subject_ID':** Subject IDentification numbers ( ranging from 1 to 30) for the 30 volunteers 

 - **'Activity':** Activity Labels--LAYING, SITTING, STANDING, WALKING, WALKING_DOWNSTAIRS, AND WALKING_UPSTAIRS in alphabetical order 
 
The rest of 66 variables include the mean values (Mean) and the standard deviation (STD) of the following list

- **'Time_Body_Accelerometer_Mean_X':** Mean of body acceleration signals along X

- **'Time_Body_Accelerometer_Mean_Y':** Mean of body acceleration signals along Y

- **'Time_Body_Accelerometer_Mean_Z':** Mean of body acceleration signals along Z

- **'Time_Gravity_Accelerometer_Mean_X':** Mean of gravity acceleration signalsJerk signals along X

- **'Time_Gravity_Accelerometer_Mean_Y':** Mean of gravity acceleration signalsJerk signals along Y

- **'Time_Gravity_Accelerometer_Mean_Z':** Mean of gravity acceleration signalsJerk signals along Z

- **'Time_Body_Accelerometer_Jerk_Mean_X':** Mean of body acceleration Jerk signal along X

- **'Time_Body_Accelerometer_Jerk_Mean_Y':** Mean of body acceleration Jerk signal along Y

- **'Time_Body_Accelerometer_Jerk_Mean_Z':** Mean of body acceleration Jerk signal along Z

- **'Time_Body_Gyroscope_Mean_X':** Mean of The body gyroscope signal along X

- **'Time_Body_Gyroscope_Mean_Y':** Mean of The body gyroscope signal along Y

- **'Time_Body_Gyroscope_Mean_Z':** Mean of The body gyroscope signal along Z

- **'Time_Body_Gyroscope_Jerk_Mean_X':** Mean of The body gyroscope Jerk signal along X

- **'Time_Body_Gyroscope_Jerk_Mean_Y':** Mean of The body gyroscope Jerk signal along Y

- **'Time_Body_Gyroscope_Jerk_Mean_Z':** Mean of The body gyroscope Jerk signal along Z

- **'Time_Body_Accelerometer_Magnitude_Mean':** Mean of time body accelerometer magnetide (Euclidean norm)

- **'Time_Gravity_Accelerometer_Magnitude_Mean':** Mean of  time body accelerometer magnetide (Euclidean norm)

- **'Time_Body_Accelerometer_Jerk_Magnitude_Mean':** Mean of time body accelerometer Jerk Magnitude (Euclidean norm)

- **'Time_Body_Gyroscope_Magnitude_Mean':** Mean of time body gyroscope magnitude (Euclidean norm) 

- **'Time_Body_Gyroscope_Jerk_Magnitude_Mean':** Mean of time body gyroscope Jerk magnitude (Euclidean norm)

- **'Time_Body_Accelerometer_STD_X':** STD of body acceleration signals along X

- **'Time_Body_Accelerometer_STD_Y':** STD of body acceleration signals along Y

- **'Time_Body_Accelerometer_STD_Z':** STD of body acceleration signals along Z

- **'Time_Gravity_Accelerometer_STD_X':** STD of gravity acceleration signalsJerk signals along X

- **'Time_Gravity_Accelerometer_STD_Y':** STD of gravity acceleration signalsJerk signals along Y

- **'Time_Gravity_Accelerometer_STD_Z':** STD of gravity acceleration signalsJerk signals along Z

- **'Time_Body_Accelerometer_Jerk_STD_X':** STD of body acceleration Jerk signal along X

- **'Time_Body_Accelerometer_Jerk_STD_Y':** STD of body acceleration Jerk signal along Y

- **'Time_Body_Accelerometer_Jerk_STD_Z':** STD of body acceleration Jerk signal along Z

- **'Time_Body_Gyroscope_STD_X':** STD of The body gyroscope signal along X

- **'Time_Body_Gyroscope_STD_Y':** STD of The body gyroscope signal along Y

- **'Time_Body_Gyroscope_STD_Z':** STD of The body gyroscope signal along Z

- **'Time_Body_Gyroscope_Jerk_STD_X':**STD of The body gyroscope Jerk signal along X

- **'Time_Body_Gyroscope_Jerk_STD_Y':**STD of The body gyroscope Jerk signal along Y

- **'Time_Body_Gyroscope_Jerk_STD_Z':**STD of The body gyroscope Jerk signal along Z

- **'Time_Body_Accelerometer_Magnitude_STD':**STD of time body accelerometer magnetide (Euclidean norm)

- **'Time_Gravity_Accelerometer_Magnitude_STD':** STD of  time body accelerometer magnetide (Euclidean norm)

- **'Time_Body_Accelerometer_Jerk_Magnitude_STD':** STD of time body accelerometer Jerk Magnitude (Euclidean norm)

- **'Time_Body_Gyroscope_Magnitude_STD':** STD of time body gyroscope magnitude (Euclidean norm) 

- **'Time_Body_Gyroscope_Jerk_Magnitude_STD':** STD of time body gyroscope Jerk magnitude (Euclidean norm)

- **'Frequency_Body_Accelerometer_Mean_X':** Mean of body accelerometer signal along X in the Frequency domain

- **'Frequency_Body_Accelerometer_Mean_Y':** Mean of body accelerometer signal along Y in the Frequency domain

- **'Frequency_Body_Accelerometer_Mean_Z':** Mean of body accelerometer signal along Z in the Frequency domain

- **'Frequency_Body_Accelerometer_Jerk_Mean_X':** Mean of body accelerometer Jerk signal along X in the Frequency domain

- **'Frequency_Body_Accelerometer_Jerk_Mean_Y':** Mean of body accelerometer Jerk signal along Y in the Frequency domain

- **'Frequency_Body_Accelerometer_Jerk_Mean_Z':** Mean of body accelerometer Jerk signal along Z in the Frequency domain

- **'Frequency_Body_Gyroscope_Mean_X':** Mean of body gyroscope signal along X in Frequency domain 

- **'Frequency_Body_Gyroscope_Mean_Y':** Mean of body gyroscope signal along Y in Frequency domain 

- **'Frequency_Body_Gyroscope_Mean_Z':** Mean of body gyroscope signal along Z in Frequency domain 

- **'Frequency_Body_Accelerometer_Magnitude_Mean':** Mean of body accelerometer signal magnitude (Euclidean norm) in the Frequency domain

- **'Frequency_Body_Accelerometer_Jerk_Magnitude_Mean':** Mean of body accelerometer Jerk signal magnitude (Euclidean norm) in the Frequency domain

- **'Frequency_Body_Gyroscope_Magnitude_Mean':** Mean of body gyroscope signal magnitude (Eulidean norm) in the Frequency domain

- **'Frequency_Body_Gyroscope_Jerk_Magnitude_Mean':** Mean of boy gyroscope Jerk signal magnitude (Euclidean norm) in the Frequency domain 

- **'Frequency_Body_Accelerometer_STD_X':** STD of body accelerometer signal along X in the Frequency domain

- **'Frequency_Body_Accelerometer_STD_Y':** STD of body accelerometer signal along Y in the Frequency domain

- **'Frequency_Body_Accelerometer_STD_Z':** STD of body accelerometer signal along Z in the Frequency domain

- **'Frequency_Body_Accelerometer_Jerk_STD_X':** STD of body accelerometer Jerk signal along X in the Frequency domain

- **'Frequency_Body_Accelerometer_Jerk_STD_Y':** STD of body accelerometer Jerk signal along Y in the Frequency domain

- **'Frequency_Body_Accelerometer_Jerk_STD_Z':** STD of body accelerometer Jerk signal along Z in the Frequency domain

- **'Frequency_Body_Gyroscope_STD_X':** STD of body gyroscope signal along X in Frequency domain 

- **'Frequency_Body_Gyroscope_STD_Y':** STD of body gyroscope signal along Y in Frequency domain 

- **'Frequency_Body_Gyroscope_STD_Z':** STD of body gyroscope signal along Z in Frequency domain 

- **'Frequency_Body_Accelerometer_Magnitude_STD':** STD of body accelerometersignal  magnitude (Euclidean norm) in the Frequency domain

- **'Frequency_Body_Accelerometer_Jerk_Magnitude_STD':** STD of body accelerometer Jerk signal magnitude (Euclidean norm) in the Frequency domain

- **'Frequency_Body_Gyroscope_Magnitude_STD':** STD of body gyroscope signal magnitude (Eulidean norm) in the Frequency domain

- **'Frequency_Body_Gyroscope_Jerk_Magnitude_STD':** STD of boy gyroscope Jerk signal magnitude (Euclidean norm) in the Frequency domain 

## The detailed procedure in run_analysis.R
1. We use data.table and reshape2 packages.

2. We first focus on the test folder in the 'UCI HAR Dataset' folder and load the Subject_ID list (subject_test.txt), Activity list (y_test.txt), test results (X_test.txt), the column names for the test_restuls (features), and the activity label (Act_Label).

3. After assigning each column its column name, we use cbind to column-combine the test data. The column names of the test data are 'Subject_ID +  Activity + list of features'

4. Repeat the same procedure for the train data.

5. Use rbind to row-combine the test and train data to give a merged data

6. We rename each features' column name  a more 'descriptive' name before calculating the required averages

7. In order to calculate the average of each activity for each Subject_ID, we use melt to form a long data table whose id = "Subject_ID", "Activity", and the rest of teh columns are variables

8. We then use dcast to bring the long data table back to the usual form along with calculating the average of each variable for each activity and each subject.

9. We export the tidy_data.txt