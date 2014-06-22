##Read data sets from files in the test or train folders
getData <- function(filename, path) {
    # Activity ID col from y file
    filepath <- file.path(path, paste0("y_", filename, ".txt"))
    yData <- read.table(filepath, header=F, col.names=c("ActivityID"))
    
    # subject ID col from subject file
    filepath <- file.path(path, paste0("subject_", filename, ".txt"))
    subjectData <- read.table(filepath, header=F, col.names=c("SubjectID"))
    
    # get column names
    dataCols <- read.table("features.txt", header=F, as.is=T, col.names=c("MeasureID", "MeasureName"))
    
    # X data file
    filepath <- file.path(path, paste0("X_", filename, ".txt"))
    data <- read.table(filepath, header=F, col.names=dataCols$MeasureName)
    
    # Get data col names and subset
    subsetDataCols <- grep(".*mean\\(\\)|.*std\\(\\)", dataCols$MeasureName)
    data <- data[,subsetDataCols]
    
    # add activity id and subject id
    data$ActivityID <- yData$ActivityID
    data$SubjectID <- subjectData$SubjectID
    
    data
}

# Merge train and test data sets from getData()
mergeData <- function() {
    data <- rbind(getData("test", "test"), getData("train", "train"))
    cNames <- colnames(data)
    cNames <- gsub("\\.+mean\\.+", cNames, replacement="Mean")
    cNames <- gsub("\\.+std\\.+", cNames, replacement="Std")
    colnames(data) <- cNames
    data
}

# Add activity names col
addActivityLabel <- function(data) {
    actLabels <- read.table("activity_labels.txt", header=F, as.is=T, col.names=c("ActivityID", "ActivityName"))
    actLabels$ActivityName <- as.factor(actLabels$ActivityName)
    dataLbs <- merge(data, actLabels)
    dataLbs
}

# Create tidy data set with the average of each variable for each activity and each subject
getTidiedData <- function(data) {
    library(reshape2)
  
    measures = setdiff(colnames(data), c("ActivityID", "ActivityName", "SubjectID"))
    melt_data <- melt(data, id = c("ActivityID", "ActivityName", "SubjectID"), measure.vars=measures)
    
    dcast(melt_data, ActivityName + SubjectID ~ variable, mean)
}

# Create the tidy data set and save it on to the named file
setTidiedDataFile <- function(filename) {
    data <- getTidiedData(addActivityLabel(mergeData()))
    write.table(data, filename)
}

setTidiedDataFile("tidy_data.txt")
