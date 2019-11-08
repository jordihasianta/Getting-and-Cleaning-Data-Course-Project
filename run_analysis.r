#get melt library
library(reshape2)

# Reading Descriptive Variable Names and the activity names
feature <- read.table('./features.txt')
activity <- read.table('./activity_labels.txt')

# Merge features value and target variable
## train
train <- read.table("./train/X_train.txt")
colnames(train) <- feature$V2 # column names
y_train <- read.table("./train/y_train.txt") # target  variable
train$activity <- y_train$V1
subject_train <- read.table('./train/subject_train.txt')
train$subject <- factor(subject_train$V1)

##test
test <- read.table("./test/X_test.txt")
colnames(test) <- feature$V2 # column names
y_test <- read.table("./test/y_test.txt") # target  variable
test$activity <- y_test$V1
subject_test <- read.table('./test/subject_test.txt')
test$subject <- factor(subject_test$V1)

#Step 1: Merging data (train and test)
dframe <- rbind(train,test)

#Step 2: 
## filter column names
columns <- colnames(dframe)
## get value consists of mean, std, activity values, and subject values
filtered <- grep("std\\(\\)|mean\\(\\)|activity|subject",columns,value= TRUE)
dframenew <- dframe[,filtered]

#Step 3: Adding Descriptive names
dframenew$activitylabel <- factor(dframenew$activity, labels = as.vector(activity$V2))

#Step 4 and 5:
## Creating Tidy dataset with mean for each subject and  activity, and then write the data
columns_avg <- grep("std\\(\\)|mean\\(\\)",columns,value= TRUE)
dframe_melt <- melt(dframenew, id  = c('activitylabel','subject'),measure.vars = columns_avg)
tidydframe  <- dcast(dframe_melt,activitylabel+subject~variable , mean)

write.table(tidydframe, file = 'tidy_dataframe.txt',row.names = FALSE)

