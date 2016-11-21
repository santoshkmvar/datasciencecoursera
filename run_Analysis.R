######################
#install.packages("downloader")
#library(downloader)
######################

## Set up Working Directory <- User need to replace Working directory
wdURL <- "C:/Users/212446591/Desktop/coursera/Assignment/3. Cleaning Data/Week4/Project"
setwd(wdURL)
## Create folder UCI_DATA in working directory
if(!file.exists("./UCI_DATA")){
  dir.create("./UCI_DATA")
} 

## download file and unzip it
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip" 
download.file(fileUrl,destfile="./UCI_DATA/Data.zip")
unzip(zipfile="./UCI_DATA/Data.zip",exdir="./UCI_DATA") 

## Read Training data
x_train <- read.table("./UCI_DATA/UCI HAR Dataset/train/x_train.txt") 
y_train <- read.table("./UCI_DATA/UCI HAR Dataset/train/y_train.txt") 
subject_train <- read.table("./UCI_DATA/UCI HAR Dataset/train/subject_train.txt")

## Read Test Data
x_test <- read.table("./UCI_DATA/UCI HAR Dataset/test/x_test.txt")
y_test <- read.table("./UCI_DATA/UCI HAR Dataset/test/y_test.txt")
subject_test <- read.table("./UCI_DATA/UCI HAR Dataset/test/subject_test.txt")

## Read Metadata
activity_labels <- read.table("./UCI_DATA/UCI HAR Dataset/activity_labels.txt") 
features <- read.table("./UCI_DATA/UCI HAR Dataset/features.txt") 

## Setting column names
colnames(x_train)       <- features[,2] 
colnames(x_test)        <- features[,2] 
colnames(y_train)       <- "activityId" 
colnames(y_test)        <- "activityId" 
colnames(subject_train) <- "subjectId"
colnames(subject_test)  <- "subjectId"
colnames(activity_labels) <-c("activityId", "activityType") 

## merged train/test data in single data fram
merged_train_data <- cbind(subject_train,x_train,y_train) 
merged_test_data  <- cbind(subject_test,x_test,y_test) 
merged_data       <- rbind(merged_train_data,merged_test_data)

## Find all column which has STD/MEAN, SUBJECTID,ACTIVITYID is for aggregation
colNames          <- colnames(merged_data) 
mean_std_indx     <-  (grepl("STD" , colNames,ignore.case = TRUE) | 
                  grepl("MEAN" , colNames,ignore.case = TRUE) |
                  grepl("ACTIVITYID" , colNames,ignore.case = TRUE) |
                  grepl("SUBJECTID" , colNames,ignore.case = TRUE) |  
                  grepl("ACTIVITYTYPE" , colNames,ignore.case = TRUE))  

## Subset all column which are required as per grepl
mean_std_set <- merged_data[,mean_std_indx]

## Remove all NA from dataframe
mean_std_set_no_na <- mean_std_set[rowSums(!is.na(mean_std_set))==ncol(mean_std_set), ]

## Grouping Activity ID, split by Subject ID and apply mean on all columns
tidy_dataset_new <- aggregate(.~subjectId + activityId, mean_std_set_no_na,mean)

## Adding Activity description in data set like "WALKING" for "1"
tidy_dataset_new <- merge(
  tidy_dataset_new,activity_labels,by.x = "activityId",by.y="activityId",all.x = TRUE
  )

## Sorting data frame by subjectId, Activity Id
tidy_dataset_new <- tidy_dataset_new[order(tidy_dataset_new$subjectId, tidy_dataset_new$activityId),]

## Reorder column by swapping (1,2) column, bringing last column on 3 column and rest all are same
tidy_dataset_new_reorder <- tidy_dataset_new[c(2,1,ncol(tidy_dataset_new),4:ncol(tidy_dataset_new)-1)]

## Give descriptive name to all columns
colnames(tidy_dataset_new_reorder)[which(colnames(tidy_dataset_new_reorder) %in% c("subjectId","activityId","activityType",
                                                                                   "tBodyAcc-mean()-X","tBodyAcc-mean()-Y","tBodyAcc-mean()-Z",
                                                                                   "tBodyAcc-std()-X","tBodyAcc-std()-Y","tBodyAcc-std()-Z",
                                                                                   "tGravityAcc-mean()-X","tGravityAcc-mean()-Y","tGravityAcc-mean()-Z",
                                                                                   "tGravityAcc-std()-X","tGravityAcc-std()-Y","tGravityAcc-std()-Z",
                                                                                   "tBodyAccJerk-mean()-X","tBodyAccJerk-mean()-Y","tBodyAccJerk-mean()-Z",
                                                                                   "tBodyAccJerk-std()-X","tBodyAccJerk-std()-Y","tBodyAccJerk-std()-Z",
                                                                                   "tBodyGyro-mean()-X","tBodyGyro-mean()-Y","tBodyGyro-mean()-Z",
                                                                                   "tBodyGyro-std()-X","tBodyGyro-std()-Y","tBodyGyro-std()-Z",
                                                                                   "tBodyGyroJerk-mean()-X","tBodyGyroJerk-mean()-Y","tBodyGyroJerk-mean()-Z",
                                                                                   "tBodyGyroJerk-std()-X","tBodyGyroJerk-std()-Y","tBodyGyroJerk-std()-Z",
                                                                                   "tBodyAccMag-mean()","tBodyAccMag-std()","tGravityAccMag-mean()",
                                                                                   "tGravityAccMag-std()","tBodyAccJerkMag-mean()","tBodyAccJerkMag-std()",
                                                                                   "tBodyGyroMag-mean()","tBodyGyroMag-std()","tBodyGyroJerkMag-mean()",
                                                                                   "tBodyGyroJerkMag-std()","fBodyAcc-mean()-X","fBodyAcc-mean()-Y",
                                                                                   "fBodyAcc-mean()-Z","fBodyAcc-std()-X","fBodyAcc-std()-Y",
                                                                                   "fBodyAcc-std()-Z","fBodyAcc-meanFreq()-X","fBodyAcc-meanFreq()-Y",
                                                                                   "fBodyAcc-meanFreq()-Z","fBodyAccJerk-mean()-X","fBodyAccJerk-mean()-Y",
                                                                                   "fBodyAccJerk-mean()-Z","fBodyAccJerk-std()-X","fBodyAccJerk-std()-Y",
                                                                                   "fBodyAccJerk-std()-Z","fBodyAccJerk-meanFreq()-X","fBodyAccJerk-meanFreq()-Y",
                                                                                   "fBodyAccJerk-meanFreq()-Z","fBodyGyro-mean()-X","fBodyGyro-mean()-Y",
                                                                                   "fBodyGyro-mean()-Z","fBodyGyro-std()-X","fBodyGyro-std()-Y",
                                                                                   "fBodyGyro-std()-Z","fBodyGyro-meanFreq()-X","fBodyGyro-meanFreq()-Y",
                                                                                   "fBodyGyro-meanFreq()-Z","fBodyAccMag-mean()","fBodyAccMag-std()",
                                                                                   "fBodyAccMag-meanFreq()","fBodyBodyAccJerkMag-mean()","fBodyBodyAccJerkMag-std()",
                                                                                   "fBodyBodyAccJerkMag-meanFreq()","fBodyBodyGyroMag-mean()","fBodyBodyGyroMag-std()",
                                                                                   "fBodyBodyGyroMag-meanFreq()","fBodyBodyGyroJerkMag-mean()","fBodyBodyGyroJerkMag-std()",
                                                                                   "fBodyBodyGyroJerkMag-meanFreq()","angle(tBodyAccMean,gravity)","angle(tBodyAccJerkMean),gravityMean)",
                                                                                   "angle(tBodyGyroMean,gravityMean)","angle(tBodyGyroJerkMean,gravityMean)","angle(X,gravityMean)",
                                                                                   "angle(Y,gravityMean)","angle(Z,gravityMean)"))] <- 
                                                                                    c("subjectId","activityId","activityType",
                                                                                      "Time-Body-Accelerate-mean()-X","Time-Body-Accelerate-mean()-Y","Time-Body-Accelerate-mean()-Z",
                                                                                      "Time-Body-Accelerate-std()-X","Time-Body-Accelerate-std()-Y","Time-Body-Accelerate-std()-Z",
                                                                                      "Time-Gravity-Accelerate-mean()-X","Time-Gravity-Accelerate-mean()-Y","Time-Gravity-Accelerate-mean()-Z",
                                                                                      "Time-Gravity-Accelerate-std()-X","Time-Gravity-Accelerate-std()-Y","Time-Gravity-Accelerate-std()-Z",
                                                                                      "Time-Body-AccelerateJerk-mean()-X","Time-Body-AccelerateJerk-mean()-Y","Time-Body-AccelerateJerk-mean()-Z",
                                                                                      "Time-Body-AccelerateJerk-std()-X","Time-Body-AccelerateJerk-std()-Y","Time-Body-AccelerateJerk-std()-Z",
                                                                                      "Time-Body-Gyroscope-mean()-X","Time-Body-Gyroscope-mean()-Y","Time-Body-Gyroscope-mean()-Z",
                                                                                      "Time-Body-Gyroscope-std()-X","Time-Body-Gyroscope-std()-Y","Time-Body-Gyroscope-std()-Z",
                                                                                      "Time-Body-GyroscopeJerk-mean()-X","Time-Body-GyroscopeJerk-mean()-Y","Time-Body-GyroscopeJerk-mean()-Z",
                                                                                      "Time-Body-GyroscopeJerk-std()-X","Time-Body-GyroscopeJerk-std()-Y","Time-Body-GyroscopeJerk-std()-Z",
                                                                                      "Time-Body-AccelerateMag-mean()","Time-Body-AccelerateMag-std()","Time-Gravity-AccelerateMag-mean()",
                                                                                      "Time-Gravity-AccelerateMag-std()","Time-Body-AccelerateJerkMag-mean()","Time-Body-AccelerateJerkMag-std()",
                                                                                      "Time-Body-GyroscopeMag-mean()","Time-Body-GyroscopeMag-std()","Time-Body-GyroscopeJerkMag-mean()",
                                                                                      "Time-Body-GyroscopeJerkMag-std()","Freq-Body-Accelerate-mean()-X","Freq-Body-Accelerate-mean()-Y",
                                                                                      "Freq-Body-Accelerate-mean()-Z","Freq-Body-Accelerate-std()-X","Freq-Body-Accelerate-std()-Y",
                                                                                      "Freq-Body-Accelerate-std()-Z","Freq-Body-Accelerate-meanFreq()-X","Freq-Body-Accelerate-meanFreq()-Y",
                                                                                      "Freq-Body-Accelerate-meanFreq()-Z","Freq-Body-AccelerateJerk-mean()-X","Freq-Body-AccelerateJerk-mean()-Y",
                                                                                      "Freq-Body-AccelerateJerk-mean()-Z","Freq-Body-AccelerateJerk-std()-X","Freq-Body-AccelerateJerk-std()-Y",
                                                                                      "Freq-Body-AccelerateJerk-std()-Z","Freq-Body-AccelerateJerk-meanFreq()-X","Freq-Body-AccelerateJerk-meanFreq()-Y",
                                                                                      "Freq-Body-AccelerateJerk-meanFreq()-Z","Freq-Body-Gyroscope-mean()-X","Freq-Body-Gyroscope-mean()-Y",
                                                                                      "Freq-Body-Gyroscope-mean()-Z","Freq-Body-Gyroscope-std()-X","Freq-Body-Gyroscope-std()-Y",
                                                                                      "Freq-Body-Gyroscope-std()-Z","Freq-Body-Gyroscope-meanFreq()-X","Freq-Body-Gyroscope-meanFreq()-Y",
                                                                                      "Freq-Body-Gyroscope-meanFreq()-Z","Freq-Body-AccelerateMag-mean()","Freq-Body-AccelerateMag-std()",
                                                                                      "Freq-Body-AccelerateMag-meanFreq()","Freq-Body-Accelerate-Jerk-Magnitude-mean()","Freq-Body-Accelerate-Jerk-Magnitude-std()",
                                                                                      "Freq-Body-Accelerate-Jerk-Magnitude-meanFreq()","Freq-Body-Gyroscope-Magnitude-mean()","Freq-Body-Gyroscope-Magnitude-std()",
                                                                                      "Freq-Body-Gyroscope-Magnitude-meanFreq()","Freq-Body-Gyroscope-Jerk-Magnitude-mean()","Freq-Body-Gyroscope-Jerk-Magnitude-std()",
                                                                                      "Freq-Body-Gyroscope-Jerk-Magnitude-meanFreq()","Time-Body-AccelerateMean-gravity-angle","Time-Body-AccelerateJerkMean-gravityMean-Angle",
                                                                                      "Time-Body-GyroscopeMean-gravityMean-Angle","Time-Body-GyroscopeJerkMean-gravityMean-angle","X-gravityMean-angle",
                                                                                      "YgravityMean-angle","Z-gravityMean-angle"
                                                                                    )
## Writing it to file                                                                 
write.table(tidy_dataset_new_reorder, ".//UCI_DATA//TidySet.txt", row.name=FALSE) 






