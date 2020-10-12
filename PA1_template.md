---
title: "Coursera Project"
author: "Facundo Felix"
date: "11/10/2020"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## Loading and preprocessing the data

1. Load the data with read.csv())

```{r}
library(readr)
library(ggplot2)
library(dplyr)

activity <- read_csv("C:/Users/Facundo/Desktop/Reproducible Reseach/activity/activity.csv")

```


2. Process/transform the data (if necessary) into a format suitable for your analysis

```{r}
activity$steps <- as.numeric(activity$steps)

activity$date <- as.POSIXct(activity$date, "%Y-%m-%d")
weekday <- weekdays(activity$date)
activity <- cbind(activity,weekday)

```

--------------------------------------------------------------------------------

## What is mean total number of steps taken per day?

1. Calculate the total number of steps taken per day

```{r}
activity_total <- aggregate(activity$steps, by = list(activity$date),
                            FUN = sum, na.rm = T)

names(activity_total) <- c("date", "steps")


```


2. Make a histogram of the total number of steps taken each day

```{r}
hist(activity_total$steps, xlab = "Steps per day",
     main = "Histogram of total steps per day",
     col = "steelblue", ylim = c(0,15), breaks = seq(0,25000, by = 1000))
```


3. Calculate and report the mean and median of the total number of steps taken per day

```{r}
round(mean(activity_total$steps))

median(activity_total$steps)

```

--------------------------------------------------------------------------------

## What is the average daily activity pattern?

1. Make a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```{r}
av_activity <- aggregate(activity$steps, by = list(activity$interval), FUN = mean,
                         na.rm = T)

names(av_activity) <- c("interval", "mean")

plot(av_activity$interval, av_activity$mean, col = "steelblue", type = "l",
     xlab = "5-minute-interval", ylab = "Average number of steps",
     main = "Average number of steps taken per day")

```


2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```{r}

av_activity[which.max(av_activity$mean),]$interval

```

--------------------------------------------------------------------------------

## Imputing missing values

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```{r}
sum(is.na(activity))

```


2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

```{r}
steps_na <- av_activity$mean[match(activity$interval, av_activity$interval)]

activity_na <- transform(activity, steps = ifelse(is.na(activity$steps),
                                              steps_na,activity$steps))
```


3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

```{r}
na_av_activity <- aggregate(activity_na$steps, by = list(activity_na$date), 
                            sum,na.rm = T)
names(na_av_activity) <- c("Date", "Steps")
```


4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```{r}
hist(na_av_activity$Steps, xlab = "Steps per day",
     main = "Histogram of total steps per day",
     col = "steelblue", ylim = c(0,15), breaks = seq(0,25000, by = 1000))
```
--------------------------------------------------------------------------------

# Are there differences in activity patterns between weekdays and weekends?

1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

```{r}
activity$date <- as.Date(strptime(activity$date, format="%Y-%m-%d"))
activity$datetype <- sapply(activity$date, function(x) {
    if (weekdays(x) == "sábado" | weekdays(x) =="domingo") 
    {y <- "Weekend"} else 
    {y <- "Weekday"}
    y
}) 
```

2. Make a panel plot containing a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).

```{r}
activity_by_date <- aggregate(steps~interval + datetype, activity, mean, na.rm = TRUE)
gg<- ggplot(activity_by_date, aes(x = interval , y = steps, color = datetype)) +
    geom_line() +
    labs(title = "Average daily steps by type of date", x = "Interval", y = "Average number of steps") +
    facet_wrap(~datetype, ncol = 1, nrow=2)+
    theme(legend.position = "none")

gg
    
```

