---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data

### 1. Load Data

```r
act <- read.csv("activity.csv")
names(act)[1] <- "Steps"
names(act)[3] <- "Interval"
```
### 2. Process/Transform the data into a format suitab;e for your analysis

```r
install.packages("ggplot2")
```

```
## Error in install.packages : Updating loaded packages
```

```r
library(ggplot2)
install.packages("dplyr")
```

```
## Error in install.packages : Updating loaded packages
```

```r
library(dplyr)
install.packages("lattice")
```

```
## Error in install.packages : Updating loaded packages
```

```r
library(lattice)
install.packages("tidyr")
```

```
## Error in install.packages : Updating loaded packages
```

```r
library(tidyr)
install.packages("lubridate")
```

```
## Error in install.packages : Updating loaded packages
```

```r
library(lubridate)
```
### Change Date format to column with date type

```r
act$date <- as.Date(act$date , format = "%Y-%m-%d")
```
### Create total steps per day

```r
act.day <- aggregate(act$steps, by =list (act$date), sum)
```

```
## Error in aggregate.data.frame(as.data.frame(x), ...): no rows to aggregate
```

```r
names(act.day)[1] <- "Date"
names(act.day)[2] <- "Steps"
```
### Create total steps per interval

```r
act.int <- aggregate(act$steps, by=list (act$interval), sum, na.rm=TRUE, na.action=NULL)
```

```
## Error in aggregate.data.frame(as.data.frame(x), ...): no rows to aggregate
```

```r
names(act.int)[1] <- "Interval"
names(act.int)[2] <- "Steps"
```
### Create dataframe with with mean steps per interval

```r
act.meanint <- aggregate(act$steps, by=list(act$interval), mean, na.rm=TRUE, na.action=NULL)
```

```
## Error in aggregate.data.frame(as.data.frame(x), ...): no rows to aggregate
```

```r
names(act.meanint)[1] <- "Interval"
names(act.meanint)[2] <- "MeanSteps"
```

## What is mean total number of steps taken per day?

### 1. Make a histogram of total number of steps taken each day (ignore missing values)

```r
act.dayNA <- act.day[!is.na(act.day$Steps), ]

png("ACTday.png", width=480, height=480, bg="white")

hist(act.day$Steps, 
  main = "Total Number of Steps Take Each Day",
  xlab= "Total Number of Steps Taken")
  ylab= "Frequency"
  
  dev.off()
```

```
## RStudioGD 
##         2
```
### 2. Calcutae and report the mean and median totla bumber of steps taken  
  
### Mean number of steps taken each day  

```r
act.mean <- mean(act.day$Steps, na.rm=TRUE)
mean(act.mean)
```

```
## [1] 10766.19
```

### Median number of steps taken each day

```r
act.median <- median(act.day$Steps, na.rm=TRUE)
median(act.median)
```

```
## [1] 10765
```

## What is the average daily activity pattern?

### 1. Make a Time Series Plot of Average Daily Actvity

```r
png("AVEplot.png", width=480, height=480, bg="white")

plot(act.meanint)

dev.off()
```

```
## RStudioGD 
##         2
```
### 2. which interval, on average across all the days contains the maximum number of steps

### Which Row and Mean Value

```r
which.max( act.meanint[,2])
```

```
## [1] 104
```

```r
max(act.meanint[,2])
```

```
## [1] 206.1698
```
## Imputing missing values

#### 1. Calculate and report the total number of missing values in the dataset

```r
sum(!complete.cases(act))
```

```
## [1] 2304
```
### 2. Fill in the missing values in the dataset

```r
act.NA <- merge(act, act.meanint, by = "Interval", sort = FALSE)
## replace NA in Steps with MeanSteps value

act.NA$Steps <- ifelse(is.na(act.NA$Steps), act.NA$MeanSteps, act.NA$Steps)
act.NA$MeanSteps <- NULL
```

```r
act.day2 <- aggregate(act.NA$Steps, by =list(act.NA$date), sum)
names(act.day2)[1] <- "Date"
names(act.day2)[2] <- "Steps"
```
### 4. Create a Histogram of Total Number of Steps per day
png("ACTday2.png", width=480, height=480, bg="white")

hist(act.day2$Steps, 
  main = "Total Number of Steps Take Each Day (NA replaced)",
  xlab= "Total Number of Steps Taken")
  ylab= "Frequency"
  
  dev.off()
```
#### 4.B Find Mean and Median total number of steps taken per day

```r
mean(act.day2$Steps)
```

```
## [1] 10766.19
```

```r
median(act.day2$Steps)
```

```
## [1] 10766.19
```
## Are there differences in activity patterns between weekdays and weekends?

```r
act.day2$weekdayType <- ifelse(weekdays(act.day2$Date) %in% 
                                c("Saturday", "Sunday"), 
                                "weekend", "weekday")
head(act.week)
```

```
##         Date    Steps weekdayType       day
## 1 2012-10-01 10766.19     weekday    Monday
## 2 2012-10-02   126.00     weekday   Tuesday
## 3 2012-10-03 11352.00     weekday Wednesday
## 4 2012-10-04 12116.00     weekday  Thursday
## 5 2012-10-05 13294.00     weekday    Friday
## 6 2012-10-06 15420.00     weekend  Saturday
```

```r
act.week <- function(date) {
    if (weekdays(as.Date(date)) %in% c("Saturday", "Sunday")) {
        "weekend"
    } else {
        "weekday"
    }
}
activity$act.week <- as.factor(sapply(act.week$date, act.week))
```

```
## Error in act.week$date: object of type 'closure' is not subsettable
```

```r
df$day <- weekdays(as.Date(df$date))
```

```
## Error in df$date: object of type 'closure' is not subsettable
```

```r
act.day2$day <- weekdays(as.Date(act.day2$Date))

act.week <- act.day2
```
