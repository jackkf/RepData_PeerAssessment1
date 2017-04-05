library(ggplot2)

# Reading data
activity <- read.csv("activity.csv", stringsAsFactors = F)
activity$date <- as.Date(activity$date)

# Steps per day
stepsPerDay <- tapply(activity$steps, activity$date, FUN=sum, na.rm=TRUE)
ggplot() + 
    geom_histogram(aes(stepsPerDay), binwidth = 1000, fill = "aquamarine3", color = "black") + 
    theme_minimal()
mean(stepsPerDay, na.rm = T)
median(stepsPerDay, na.rm = T)

# Average daily activity pattern
stepsPerInterval <- aggregate(x=list(steps=activity$steps), by=list(interval=activity$interval), FUN=mean, na.rm=TRUE)
ggplot(stepsPerInterval, aes(x=interval, y=steps)) +
    geom_line() +
    xlab("Interval (5 Minutes)") +
    ylab("Average Steps") +
    theme_minimal()
stepsPerInterval[which.max(stepsPerInterval$steps),]

# Missing Values
nrow(activity[is.na(activity$steps),])
missingValue <- function(x) {
    if(is.na(x[1])){
        x[1] <- stepsPerInterval$steps[as.integer(x[3]) == stepsPerInterval$interval]
        return(x)
    } else{
        return(x)
    }
}
activitymv <- as.data.frame(t(apply(activity,1,missingValue)))
activitymv$steps <- as.numeric(as.character(activitymv$steps))
activitymv$interval <- as.numeric(as.character(activitymv$interval))
activitymv$date <- as.Date(activitymv$date)
stepsPerDay <- tapply(activitymv$steps, activitymv$date, FUN=sum, na.rm=TRUE)
ggplot() + 
    geom_histogram(aes(stepsPerDay), binwidth = 1000, fill = "aquamarine3", color = "black") + 
    theme_minimal()
mean(stepsPerDay, na.rm = T)
median(stepsPerDay, na.rm = T)

# Weekdays and Weekends
activitymv$day <- ifelse(weekdays(activitymv$date) %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"), "weekday", "weekend")
stepsPerIntervalDay <- aggregate(x=list(steps=activitymv$steps), by=list(interval=activitymv$interval, day=activitymv$day), FUN=mean, na.rm=TRUE)
ggplot(stepsPerIntervalDay, aes(interval, steps)) + 
    geom_line() + 
    facet_grid(day ~ .) +
    xlab("Interval (5 Minutes)") +
    ylab("Average Steps") +
    theme_minimal()
