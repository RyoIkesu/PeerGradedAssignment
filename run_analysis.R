library(dplyr)
setwd("~/Desktop/UCI HAR Dataset")
ft <- read.table("features.txt", stringsAsFactors = FALSE)

setwd("~/Desktop/UCI HAR Dataset/test")

xtest <- read.table("X_test.txt", header = FALSE, sep = "")
ytest <- read.table("y_test.txt", header = FALSE, sep = "")
subtest <- read.table("subject_test.txt", header = FALSE, sep = "")
test <- cbind(xtest, ytest, subtest) #combine all test data

setwd("~/Desktop/UCI HAR Dataset/train")

xtrain <- read.table("X_train.txt", header = FALSE, sep = "")
ytrain <- read.table("y_train.txt", header = FALSE, sep = "")
subtrain <- read.table("subject_train.txt", header = FALSE, sep = "")
train <- cbind(xtrain, ytrain, subtrain) # combine all train data

obs <- rbind(test, train) # all obsevationed data, composed of test & train data
colnames(obs) <- c(ft$V2, "activity", "id")

#extract the numbers of rows containing the string "mean()"
num1<- grep("mean()", ft$V2) 
#extract rows containing the string "std()"
num2<- grep("std()", ft$V2) 
num <- c(563, 562, num1, num2) 

obs2 <- obs[,num]

setwd("~/Desktop/UCI HAR Dataset")
act <- read.table("activity_labels.txt", stringsAsFactors = FALSE)

merged <- merge(obs2, act, by.x = "activity", by.y = "V1")
# apply descriptive activity names to the activity column
merged$activity <- merged$V2 
mergeddata <- merged[,c(2, 1, 3:81)]
#arrange the dataframe with the ids in ascending order
df <- mergeddata[order(mergeddata$id),] 

# set the column names to appropriate, descriptive variable names
colnames(df) <- colnames(df) %>% gsub("^t", "time.", .)%>%
    gsub("^f", "freq.", .)%>%gsub("Acc", "Acceleration", .)%>%
    gsub("Gyro", "Gyroscopic", .)%>%
    gsub("Mag", "Magnitude", .)%>%gsub("-std()", "S.D.", .)%>%
    
    ## use the "\\" as the escape signal
    gsub("-mean()", "Mean", .)%>%gsub("\\()", "", .)%>%gsub("-X", "in X", .)%>%
    gsub("-Y", "in Y", .)%>%gsub("-Z", "in Z", .)

ndf <- df %>% group_by(id, activity) %>% summarize_all(.funs = mean)