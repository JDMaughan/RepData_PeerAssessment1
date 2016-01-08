##library(plyr)
##library(dplyr)
##library(ggplot2)
##library(data.table)

## Read in the datafile
## 
activityData <- read.csv("Data/activity.csv", na.strings = "NA")
activityData$date <- as.Date(activityData$date)


##Look at the data 
# str(activityData)
# sum(is.na(activityData$steps))
##any(is.na(activityData$steps))
##
##We have 2304 rows with 'NA' in steps.
colSums(is.na(activityData))

##########
##What is the MEAN toal number of steps taken per day?
##'NA' values are ignored for this question per assignment
##########

# 1. Calculate total number of steps per day
#stepsPerDay <- aggregate(steps ~ date, activityData, FUN = "sum") 
stepsPerDay <- group_by(activityData, date) %>%
        summarize(DailySteps = sum(steps))

# 2. Make a histogram of total number of steps taken each day
plot(stepsPerDay,
     type = "h", 
     xlab = "Date", 
     ylab = "Number of Steps",
     main = "Daily Step Count",
     col = "blue",
     lwd = 4)

# 3. calculate and report the mean and median of the total number of steps taken per day
stepsPerDay <- filter(activityData, steps > 0) %>%
        group_by(date) %>%
        summarize(DailySteps = sum(steps, na.rm = TRUE),
                  DailyMean = mean(steps, na.rm = TRUE),
                  DailyMedian = median(steps, na.rm = TRUE))
stepsPerDay

##########
## What is the average daily pattern?
##########
# 1. Make a time-series plot of the 5-minute interval and average nubmer of steps taken, averaged across all days
plot(stepsPerDay$date, stepsPerDay$DailyMean,
     type = "l",
     ylab = "Mean Steps per Day",
     xlab = "Date")
points(x = stepsPerDay$date,
       y = stepsPerDay$DailyMean,
       type = "p",
       pch = 16)
abline(h = mean(stepsPerDay$DailyMean))


# 2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum # number of steps? 
maxDate <- arrange(stepsPerDay, desc(DailyMean))[1,]
text(maxDate$date, y = maxDate$DailyMean, labels = maxDate$date, pos = 2)

##########
## Impute missing values 
##########
# 1. Calculate and report the total number of missing values
colSums(is.na(activityData))  ##We have 2304 rows with 'NA' in steps.

# 2. & 3. Replace 'NA' with mean for that day



## small example to figure out that median returns 0 unles steps > 0 rows are filtered out
Oct28 <- filter(activityData, date == "2012-10-28" & steps > 0) %>%
        summarize(DailySteps = sum(steps, na.rm = TRUE),
                  DailyMean = mean(steps, na.rm = TRUE),
                  DailyMedian = median(steps, na.rm = TRUE))

Oct28 <- filter(activityData, date == "2012-10-28" & steps > 0)
sum(Oct28$steps)
mean(Oct28$steps)
median(Oct28$steps)

