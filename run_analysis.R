library(reshape2)

# Folder of the files
folder <- "UCI HAR Dataset"

# Import test and train data
test.data <- read.table(file.path(folder, 'test', 'X_test.txt'))
test.activities <- read.table(file.path(folder, 'test', 'y_test.txt'))
test.subjects <- read.table(file.path(folder, 'test', 'subject_test.txt'))

train.data <- read.table(file.path(folder, 'train', 'X_train.txt'))
train.activities <- read.table(file.path(folder, 'train', 'y_train.txt'))
train.subjects <- read.table(file.path(folder, 'train', 'subject_train.txt'))




## 1. Merges the training and the test sets to create one data set.

# Bind the rows for each of the data sets together
data.data <- rbind(test.data, train.data)
data.activities <- rbind(test.activities, train.activities)
data.subjects <- rbind(test.subjects,train.subjects)

# Combine all of of the different columns together into one table
data <- cbind(data.subjects, data.activities, data.data)




## 2. Extracts only the measurements on the mean and standard deviation for each measurement.

# Grab the complete list of features
features <- read.table(file.path(folder, 'features.txt'))

# Filter to the features we want
requiredFeatures <- grep('-(mean|std)\\(\\)', features[, 2 ])
data <- data[, c(1, 2, requiredFeatures)]




## 3. Uses descriptive activity names to name the activities in the data set

# Read in the activity labels
activities <- read.table(file.path(folder, 'activity_labels.txt'))

# Update the activity name
data[, 2] <- activities[data[,2], 2]



## 4. Appropriately labels the data set with descriptive variable names. 

colnames(data) <- c(
    'subject',
    'activity',
    # Remove the brackets from the features columns
    gsub('\\-|\\(|\\)', '', as.character(features[requiredFeatures, 2]))
)

# Coerce the data into strings
data[, 2] <- as.character(data[, 2])



## 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

# Melt the data so we have a unique row for each combination of subject and acitivites
final.melted <- melt(data, id = c('subject', 'activity'))

# Cast it getting the mean value
final.mean <- dcast(final.melted, subject + activity ~ variable, mean)

# Write data in the file tidy.txt
write.table(final.mean, file=file.path("tidy.txt"), row.names = FALSE, quote = FALSE)




