---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data

### 1. Load Data

act <- read.csv("activity.csv")

### 2. Process/Transform the data into a format suitab;e for your analysis

install.packages("dplyr")
library(dplyr)
install.packages("lattice")
library(lattice)
install.packages("tidyr")
library(tidyr)
install.packages("lubridate")
library(lubridate)

### Change Date format to column with date type

act$date <- as.Date(act$date , format = "%Y-%m-%d")

### Create total steps per day

act.day <- aggregate(act$steps, by =list (act$date), sum)
names(act.day)[1] <- "Date"
names(act.day)[2] <- "Steps"

### Create total steps per interval

act.int <- aggregate(act$steps, by=list (act$interval), sum, na.rm=TRUE, na.action=NULL)
names(act.int)[1] <- "Interval"
names(act.int)[2] <- "Steps"

### Create dataframe with with mean steps per interval

act.meanint <- aggregate(act$steps, by=list(act$interval), mean, na.rm=TRUE, na.action=NULL)
names(act.meanint)[1] <- "Interval"
names(act.meanint)[2] <- "Mean Steps"


## What is mean total number of steps taken per day?

### 1. Make a histogram of total number of steps taken each day
png("ACTday.png", width=480, height=480, bg="white")

hist(act.day$steps, 
  main = "Histogram of Total Number of Steps Take Each Day",
  xlab= "Total Number of Steps Taken")
  
  dev.off()
  
### 2. Calcutae and report the mean and median totla bumber of steps taken  
  
### Mean number of steps taken each day  

act.mean <- mean(act.day$steps, na.rm=TRUE)

### Median number of steps taken each day

act.median <- median(act.day$steps, na.rm=TRUE)

## What is the average daily activity pattern?

### 1. Make a Time Series Plot of Average Daily Actity

png("AVEplot.png", width=480, height=480, bg="white")
aveplot <- ggplot(act.meanint$interval, act.meanint$interval"Mean Steps", type="n",
  main="Time Series Plot of Average # of Steps Taken"
  xlab= "5 Minutes Intervals"
  ylab= "AVerage # of Steps Taken"

dev.off()

### 2. which interval, on average across all the days contains the maximum number of steps

Maxint <- which.max(act.meanint$mean)
Maxint <- act.meanint$interval[Maxint]

## Imputing missing values

#### 1. Calculate and report the total number of missing values in the dataset

NA <- sum(is.na(act$steps))

### 2. Fill in the missing values in the dataset

## Are there differences in activity patterns between weekdays and weekends?
