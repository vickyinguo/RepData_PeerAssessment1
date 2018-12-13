---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---

## Loading and preprocessing the data
```{r}
# read in data
data <- read.csv("C:/Users/yguo/Documents/R/activity.csv")
dim(data)
str(data)
summary(data)
head(data)
# change date to date format
data$date <- as.Date(data$date, format = "%Y-%m-%d")
```

## What is mean total number of steps taken per day?
```{r}
# total number of steps taken per day
totalstepsperday <- aggregate(steps ~ date, data = data, FUN = sum)
# histogram of total number of steps taken each day and store into plot1
hist(totalstepsperday$steps, breaks = 10)
# mean and median of total number of steps taken per day
mean(totalstepsperday$steps)
median(totalstepsperday$steps)
```

## What is the average daily activity pattern?
```{r}
# time series plot and store into plot2
avgstepsperinterval <- aggregate(steps ~ interval, data = data, FUN = mean)
plot(avgstepsperinterval$interval,avgstepsperinterval$steps, type='l')
# which interval has the max average steps
avgstepsperinterval[which.max(avgstepsperinterval$steps),][[1]]
```

## Imputing missing values
```{r}
# total number of NAs in the dataset
sum(is.na(data))
# fill in missing value using mean of that 5-minute interval and round up to nearest 1 since no partial step can be taken
for (i in 1:nrow(data))
    {if(is.na(data[i,]$steps))
        {data[i,]$steps <- ceiling(avgstepsperinterval[avgstepsperinterval$interval == data[i,]$interval,]$steps)} 
    }
# Below should be TRUE
sum(is.na(data)) == 0
# total number of steps taken per day in new data
newtotalstepsperday <- aggregate(steps ~ date, data = data, FUN = sum)
# histogram of total number of steps taken each day in new data and store into plot3
hist(newtotalstepsperday$steps, breaks = 10)
# mean and median of total number of steps taken per day in new data
mean(newtotalstepsperday$steps)
median(newtotalstepsperday$steps)
```

## Are there differences in activity patterns between weekdays and weekends?
```{r}
# add weekday to the new data set
data$day <- as.POSIXlt(data$date)$wday
for (i in 1:nrow(data)) {if (data[i,]$day %in% c(0,6)){data[i,]$day<-'weekend'} else {data[i,]$day<-'weekday'}}
data$day <- as.factor(data$day)
# time series plot for weekdays and weekends and store into plot4
avgstepsperintervalweekday <- aggregate(steps ~ interval, data = data[which(data$day == 'weekday'),], FUN = mean)
avgstepsperintervalweekend <- aggregate(steps ~ interval, data = data[which(data$day == 'weekend'),], FUN = mean)
par(mfrow=c(2,1))
plot(avgstepsperintervalweekday$interval,avgstepsperintervalweekday$steps, type='l',main = 'Weekdays',ylab='Steps')
plot(avgstepsperintervalweekend$interval,avgstepsperintervalweekend$steps, type='l',main = 'Weekends',ylab='Steps')
```
