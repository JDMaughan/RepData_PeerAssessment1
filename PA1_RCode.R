##library(plyr)
##library(dplyr)
##library(ggplot2)
##library(data.table)

## Read in the datafile
## 
activityData <- read.csv("Data/activity.csv", na.strings = "NA")
activityData$date <- as.Date(activityData$date)
str(activityData)


##Look at the data 
# str(activityData)
# sum(is.na(activityData$steps))
##any(is.na(activityData$steps))
##
##We have 2304 rows with 'NA' in steps.
##colSums(is.na(activityData))

##########
##What is the MEAN toal number of steps taken per day?
##'NA' values are ignored for this question per assignment
##########

# 1. Calculate total number of steps per day
#stepsPerDay <- aggregate(steps ~ date, activityData, FUN = "sum") 
stepsPerDay <- group_by(activityData, date) %>%
        summarize(DailySteps = sum(steps))

head(stepsPerDay)

# 2. Make a histogram of total number of steps taken each day
hist(stepsPerDay$DailySteps,
     main = "Distribution of Total Number of Steps per Day",
     xlab = "Total Number of Steps per Day",
     ylab = "Frequency (number of days)",
     breaks = 20,
     col = "grey")
 
# plot(stepsPerDay,
#      type = "h", 
#      xlab = "Date", 
#      ylab = "Number of Steps",
#      main = "Daily Step Count",
#      col = "blue",
#      lwd = 4)

# 3. calculate and report the mean and median of the total number of steps taken per day
stepsPerDay <- filter(activityData, steps > 0) %>%
        group_by(date) %>%
        summarize(DailySteps = sum(steps, na.rm = TRUE),
                  DailyMean = mean(steps, na.rm = TRUE),
                  DailyMedian = median(steps, na.rm = TRUE))
stepsPerDay
mean(stepsPerDay$DailySteps, na.rm = TRUE)
median(stepsPerDay$DailySteps, na.rm = TRUE)

##########
## What is the average daily pattern?
##########
# 1. Make a time-series plot of the 5-minute interval and average nubmer of steps taken, averaged across all days
stepsPerInterval <- group_by(activityData, interval)%>%
        summarize(AvgIntSteps = mean(steps, na.rm = TRUE))

stepsPerInterval

plot(strptime(sprintf("%04d", stepsPerInterval$interval), format = "%H%M"),
        stepsPerInterval$AvgIntSteps, 
        type = "l",
        main = "Average Daily Activity",
        xlab = "Time of Day (HH:MM)",
        ylab = "Avg Number of Steps")


# plot(stepsPerDay$date, stepsPerDay$DailyMean,
#      type = "l",
#      ylab = "Mean Steps per Day",
#      xlab = "Date")
# points(x = stepsPerDay$date,
#        y = stepsPerDay$DailyMean,
#        type = "p",
#        pch = 16)
# abline(h = mean(stepsPerDay$DailyMean))


# 2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum # number of steps? 
maxDate <- arrange(stepsPerInterval, desc(AvgIntSteps))[1,] 
maxDate
# Can't annote high point easily using plot
#text(maxDate$interval, y = maxDate$AvgIntSteps, labels = maxDate$interval, pos = 2)

##########
## Impute missing values 
##########
# 1. Calculate and report the total number of missing values
colSums(is.na(activityData))  ##We have 2304 rows with 'NA' in steps.

# 2. & 3. Create a new dataset, Replacing 'NA' with mean for that day, using
# stepsPerInterval$AvgIntSteps
activityImputed <- inner_join(activityData,stepsPerInterval,by="interval") %>%
                mutate(steps=ifelse(is.na(steps), AvgIntSteps, steps)) %>%
                select(date, interval, steps)
str(activityImputed)
head(activityImputed)

# 4. Make a histogram of the total number of steps taken each day.  
# What is the impact of imputing missing data on the estimates of total daily number of steps?
# 
stepsPerDayImputed <- group_by(activityImputed, date)%>%
        summarize(TotalSteps = sum(steps))
stepsPerDayImputed

hist(stepsPerDayImputed$TotalSteps,
     main = "Distribution of Total Number of Steps per Day\nImputed 'NA' values",
     xlab = "Total Number of Steps per Day",
     ylab = "Frequency (number of days)",
     breaks = 20,
     col = "grey")
# Calculate mean and median
stepsPerDayImputed <- filter(activityData, steps > 0) %>%
        group_by(date) %>%
        summarize(TotalSteps = sum(steps),
                  DailyMean = mean(steps, na.rm = TRUE),
                  DailyMedian = median(steps, na.rm = TRUE))
stepsPerDayImputed
mean(stepsPerDayImputed$TotalSteps)
median(stepsPerDayImputed$TotalSteps)

##########
## Are there Differences in activity patterns between weekdays and weekends?
##########
# 1. Create a new factor variable
library(dplyr)
activityImputed <- mutate(activityImputed, 
                          DayClass = as.factor(ifelse(weekdays(date) %in% 
                          c("Saturday", "Sunday"), "Weekend", "Weekday")))

str(activityImputed)
head(activityImputed)

activityPattern <- group_by(activityImputed, DayClass, interval)%>%
        summarize(MeanSteps = mean(steps))
str(activityPattern)
head(activityPattern)

library(ggplot2)
library(scales)
g <- ggplot(activityPattern, 
            aes(strptime(sprintf("%04d", interval), format = "%H%M"), MeanSteps),
            environment = environment())+ 
        geom_line() +
        facet_wrap(~ DayClass, nrow = 2) +
        xlab("Time of Day") +
        ggtitle("Average Daily Activity\nWeekday vs. Weekend") +
        scale_x_datetime(limits = as.POSIXct(c("2012-10-01 00:00:00", "2012-11-30 24:00:00")), labels = date_format("%H:%M"), breaks = date_breaks("2 hour")) +
        scale_y_continuous("Average Number of Steps") +
        theme(plot.title = element_text(vjust = 2))
print(g)


## small example to figure out that median returns 0 unles steps > 0 rows are filtered out
Oct28 <- filter(activityData, date == "2012-10-28" & steps > 0) %>%
        summarize(DailySteps = sum(steps, na.rm = TRUE),
                  DailyMean = mean(steps, na.rm = TRUE),
                  DailyMedian = median(steps, na.rm = TRUE))

Oct28 <- filter(activityData, date == "2012-10-28" & steps > 0)
sum(Oct28$steps)
mean(Oct28$steps)
median(Oct28$steps)

