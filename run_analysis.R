#Runs analysis on dataset : https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip



ReadSensorData <- function()
{
    #READS ALL THE MAIN DATA
    testy <- read.table("test/y_test.txt")
    trainy <- read.table("train/y_train.txt")
    trainsubject <- read.table("train/subject_train.txt")
    
    trainx <- read.table("train/X_train.txt")
    testx <- read.table("test/X_test.txt")
    testsubject <- read.table("test/subject_test.txt")
    
    features <- read.table("features.txt")
    activitylabels <- read.table("activity_labels.txt")
    
    #MERGES THE RELEVANT TABLES AND NAMES THE COLUMNS
    train <- cbind(trainy,trainsubject,trainx)
    test <- cbind(testy,testsubject, testx)
    fullset <- rbind(train,test)
    
    #LABEL THE DATA SET WITH SUITABLY CONVERTED VARIABLE NAMES FROM THE TEXT FILE
    features[,2] <- tolower(features[,2])               #convert to lower case
    features[,2] <- sub("\\(\\)","",features[,2])       #remove brackets for readability
    names(fullset) <- c("activity","subject",features[,2])        #rename dataframe columns
    
    #EXTRACT ONLY MEAN AND SD MEASUREMENTS
    colsneeded <- c(1,2,grep("mean|std",names(fullset)))  #decide columns needed
    newset <- fullset[,colsneeded]                      #subset the dataframe
    
    #Change activity names
    for(i in 1:nrow(newset))
    {
        for(j in 1:nrow(activitylabels))
        {
            if(activitylabels[j,1]==newset$activity[i])
            {
                newset$activity[i] = as.character(activitylabels[j,2])
            }
        }
    }    
    
    return(newset)
}

SummarizeData <- function(setdata)
{
    #This function accepts a data frame and summarizes it with means based on activity and subject
    
    #Based on BOTH activities and subjects
    setdata$code <- paste(setdata$activity,setdata$subject,sep = ",")
    newset <- group_by(setdata,setdata$code)
    k <- summarise_all(newset,mean,na.rm = TRUE)
    #x <- lapply(as.character(k$`setdata$code`),strsplit,split = ",")
    k$activity <- unlist(lapply(as.character(k$`setdata$code`),strsplit,split = ","))[seq(1,(nrow(k)*2-1),by=2)]
    k <- k[,c(-1,-1*ncol(k))]
    k <- arrange(k,activity,subject)
    
    #Means Based on ACTIVITIES for ALL subjects
    newset <- group_by(setdata,setdata$activity)
    l <- summarise_all(newset,mean,na.rm = TRUE)
    l <- l[,c(-2,-1*ncol(l))]
    l[,2] <- rep("ALL",times = nrow(l))
    names(l)<-names(k)
    
    #Means Based on SUBJECTS for ALL activities
    newset <- group_by(setdata,setdata$subject)
    m <- summarise_all(newset,mean,na.rm = TRUE)
    m[,3] <- m[,1]
    m[,2] <- rep("ALL",times = nrow(m))
    m <- m[,c(-1,-1*ncol(m))]
    names(m)<-names(k)
    
    summarized <- rbind(l,m,k)
    
    return(summarized)
}

ProcessAndSaveSummary <- function(filename)
{
    #A Single Function to process and save data summary into a file of given filename
    x <- summarizedata(ReadSensorData())
    write.table(x, file = filename)
}