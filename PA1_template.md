# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data
# Asuming the RAW file exists on the local 
rawURL = "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"

# If the file does not exist on the current directory, download it 
if (!file.exists("activity.zip")) {
      download.file(url = rawURL,
            destfile = "activity.zip", method = "curl")
}

# Check/create data folder
if(!file.exists("data")) { dir.create("data") }

#Check if the data has been unzipped, if it hasn't, then unzip it
if (!file.exists("data/activity.csv")){
      unzip(zipfile = "activity.zip", exdir = "data", overwrite = TRUE)
}


## What is mean total number of steps taken per day?



## What is the average daily activity pattern?



## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?
