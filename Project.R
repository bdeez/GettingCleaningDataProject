

# Basic Steps taken to complete the assignment
#1 Read tables in
#2 Add / Update column names for each table
#3 Add activity labels to Y tables
#4 Bind additional columns to testdataX and traindataX tables
#5 Merge testdataX and traindataX tables to create master project data frame
#6 Trim project data frame to only include mean and std values (69 columnns with addt'l bound columns)
#7 Trim dataframe down to only mean values, reorder columns and clean column names
#8 - Group and summarize by subject and activity in new data frame
#9 - Create txt file for submission

#1 - Read tables in
testdataX <- read.table("./data/X_test.txt", sep = "", header = FALSE)
traindataX <- read.table("./data/X_train.txt", sep = "", header = FALSE) 
testdataY <- read.table("./data/y_test.txt", sep = "", header = FALSE)
traindataY <- read.table("./data/y_train.txt", sep = "", header = FALSE)
activitylabels <- read.table("./data/activity_labels.txt", sep = "", header = FALSE)
features <- read.table("./data/features.txt", sep = "", header = FALSE)
subject_test <- read.table("./data/subject_test.txt", sep = "", header = FALSE)
subject_train <- read.table("./data/subject_train.txt", sep = "", header = FALSE)

#2 - Add / Update column names for each table
testdataX <- setNames(testdataX, features[ ,2])
traindataX <- setNames(traindataX, features[ ,2])
testdataY <- rename(testdataY, actnum = V1)
traindataY <- rename(traindataY, actnum = V1)
activitylabels <- rename(activitylabels, actnum = V1, actdesc = V2)
subject_test <- rename(subject_test, subjectnum = V1)
subject_train <- rename(subject_train, subjectnum = V1)

#3 - Add activity labels to Y tables (using subset approach)
testdataY$actdesc <- "undefined"
testdataY$actdesc[testdataY$actnum == 1] <- "WALKING"
testdataY$actdesc[testdataY$actnum == 2] <- "WALKING_UPSTAIRS"
testdataY$actdesc[testdataY$actnum == 3] <- "WALKING_DOWNSTAIRS"
testdataY$actdesc[testdataY$actnum == 4] <- "SITTING"
testdataY$actdesc[testdataY$actnum == 5] <- "STANDING"
testdataY$actdesc[testdataY$actnum == 6] <- "LAYING"

traindataY$actdesc <- "undefined"
traindataY$actdesc[traindataY$actnum == 1] <- "WALKING"
traindataY$actdesc[traindataY$actnum == 2] <- "WALKING_UPSTAIRS"
traindataY$actdesc[traindataY$actnum == 3] <- "WALKING_DOWNSTAIRS"
traindataY$actdesc[traindataY$actnum == 4] <- "SITTING"
traindataY$actdesc[traindataY$actnum == 5] <- "STANDING"
traindataY$actdesc[traindataY$actnum == 6] <- "LAYING"

#4 - Bind additional columns to testdataX and traindataX tables
testdataX <- bind_cols(testdataX, testdataY, subject_test)
traindataX <- bind_cols(traindataX, traindataY, subject_train)

#5 - Merge testdataX and traindataX tables to create master project data frame
prjdata <- rbind(testdataX, traindataX)

#6 - Trim project data frame to only mean and std values
prjdata <- prjdata[,grep("\\bmean\\b|\\bstd\\b|actnum|actdesc|subjectnum", colnames(prjdata))]

#7 - Trim dataframe down to only mean values, reorder columns and clean column names
tidydata <- prjdata[,grep("\\bmean\\b|actdesc|subjectnum", colnames(prjdata))]
tidydata <- tidydata[, c(35,34,1:33)]
names(tidydata) <- tolower(names(tidydata))
names(tidydata) <- gsub("-","",names(tidydata))
names(tidydata) <- gsub(" ","",names(tidydata))
names(tidydata) <- gsub("\\()","",names(tidydata))

#8 - Group and summarize by subject and activity in new data frame
bysubact <- group_by(tidydata, subjectnum, actdesc)
tidydf <- as.data.frame(summarise_each(bysubact,funs(mean)))

#9 - Create txt file for submission
write.table(tidydf, "tidydf.txt" ,row.names = FALSE)

