# CodeBook.md
This code book describes the variables, the data, and any transformations or work that were performed to clean up the data.

## Data source
https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip

### Content
+ 'README.txt'
+ 'features_info.txt': Shows information about the variables used on the feature vector.
+ 'features.txt': List of all features.
+ 'activity_labels.txt': Links the class labels with their activity name.
+ 'train/X_train.txt': Training set.
+ 'train/y_train.txt': Training labels.
+ 'test/X_test.txt': Test set.
+ 'test/y_test.txt': Test labels.

The following files are available for the train and test data. Their descriptions are equivalent. 
+ 'train/subject_train.txt': Each row identifies the subject who performed the activity for each window sample. Its range is from 1 to 30. 
+ 'train/Inertial Signals/total_acc_x_train.txt': The acceleration signal from the smartphone accelerometer X axis in standard gravity units 'g'. Every row shows a 128 element vector. The same description applies for the 'total_acc_x_train.txt' and 'total_acc_z_train.txt' files for the Y and Z axis. 
+ 'train/Inertial Signals/body_acc_x_train.txt': The body acceleration signal obtained by subtracting the gravity from the total acceleration. 
+ 'train/Inertial Signals/body_gyro_x_train.txt': The angular velocity vector measured by the gyroscope for each window sample. The units are radians/second. 

### Performed transformations
1. Load the features and activity label data: 
 features.txt -> dat.features; 
 activity_labels.txt -> dat.activitylabels
2. Define the selection and make the labels readable: 
 Select the features of interest, extract only the data on mean and standard deviation -> dat.wanted, dat.wanted.names
3. Load the test and training data sets and subset them: 
 X_train.txt -> dat.trainset; 
 X_test.txt -> dat.testset
4. Load the labels and the subject data: 
 y_train.txt -> dat.trainactivity; 
 y_test.txt -> dat.testactivity;
 subject_train.txt -> dat.trainsubjects; 
 subject_test.txt -> dat.testsubjects; 
5. Combine by columns cbind:
 dat.trainsubjects, dat.trainactivity, dat.trainset -> train.cbind;
 dat.testsubjects, dat.testactivity, dat.testset -> test.cbind
6. Merge the test and training data by rows rbind:
 train.cbind, test.cbind -> dat.rbind
7. Label the dataframe dat.rbind
8. Label the activities using the dataframe dat.activitylabels
9. Calculate the mean of each variable by subject and activity -> dat.mean
10. Export the tidy data as tidy_data.txt
