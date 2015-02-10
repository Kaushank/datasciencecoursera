## 1: Merges the training and the test sets to create one data set
library(reshape2)

train_subject <- read.table("subject_train.txt")
test_subject <- read.table("subject_test.txt")
X_train <- read.table("X_train.txt")
X_test <- read.table("X_test.txt")
y_train <- read.table("y_train.txt")
y_test <- read.table("y_test.txt")

# combine files into one dataset
names(train_subject) <- "subjectID"
names(test_subject) <- "subjectID"
features_txt <- read.table("features.txt")
names(X_train) <- features_txt$V2
names(X_test) <- features_txt$V2
names(y_train) <- "activity"
names(y_test) <- "activity"
train <- cbind(subject_train, y_train, X_train)
test <- cbind(subject_test, y_test, X_test)
combined <- rbind(train, test)


## 2: Extracts only the measurements on the mean and standard
meanstdcols <- grepl("mean\\(\\)", names(combined)) |grepl("std\\(\\)", names(combined))
meanstdcols[1:2] <- TRUE
combined <- combined[, meanstdcols]


## 3: Uses descriptive activity names to name the activities
##  4: Appropriately labels the data set with descriptive

combined$activity <- factor(combined$activity, labels=c("Walking",
                                                        "Walking Upstairs", "Walking Downstairs", "Sitting", "Standing", "Laying"))

melted <- melt(combined, id=c("subjectID","activity"))
tidy <- dcast(melted, subjectID+activity ~ variable, mean)
write.csv(tidy, "tidiedData.csv", row.names=FALSE)
