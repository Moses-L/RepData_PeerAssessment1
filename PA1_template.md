---
title: "Reproducible Research Assignment"
author: "Moses Luke"
date: "11/15/2019"
output: html_document
keep_md: yes
---

## Loading and preprocessing the data
```{r, echo=TRUE}
library(readr)
activity <- read_csv("activity.csv")
View(activity)
head(activity)
```

## What is mean total number of steps taken per day?
```{r total_numbner_of_steps_taken_each_day, echo=FALSE}
library(ggplot2)
total.steps <- tapply(activity$steps, activity$date, FUN=sum, na.rm=TRUE)
qplot(total.steps, binwidth=1000, xlab="total number of steps taken each day")
mean(total.steps, na.rm=TRUE)
median(total.steps, na.rm=TRUE)
```


## What is the average daily activity pattern?
```{r 5-minute_interval_average_number_of_steps_taken, echo=FALSE}
library(ggplot2)
averages <- aggregate(x=list(steps=activity$steps), by=list(interval=activity$interval),
                      FUN=mean, na.rm=TRUE)
ggplot(data=averages, aes(x=interval, y=steps)) +
    geom_line() +
    xlab("5-minute interval") +
    ylab("average number of steps taken")
```

## on the Average
```{r, echo=TRUE}
averages[which.max(averages$steps),]
```

## Imputing missing values
```{r how_many_missing, echo=TRUE}
missing <- is.na(activity$steps)
```

## How many missing steps
```{r, echo=TRUE}
table(missing)
```
## The total count of NA in the dataset is 2304. 

## All of the missing values are filled in with mean value for that 5-minute
interval.

```{r, echo=TRUE}
# Replace each missing value with the mean value of its 5-minute interval
fill.value <- function(steps, interval) {
    filled <- NA
    if (!is.na(steps))
        filled <- c(steps)
    else
        filled <- (averages[averages$interval==interval, "steps"])
    return(filled)
}
filled.activity <- activity
filled.activity$steps <- mapply(fill.value, filled.activity$steps, filled.activity$interval)
```
## Now, using the filled data set, let's make a histogram of the total number of steps taken each day and ## calculate the mean and median total number of steps.
```{r, echo=TRUE}
total.steps <- tapply(filled.activity$steps, filled.activity$date, FUN=sum)
qplot(total.steps, binwidth=1000, xlab="total number of steps taken each day")
mean(total.steps)
median(total.steps)
```
## Mean and median values are higher after imputing missing data. The reason is
## that in the original data, there are some days with `steps` values `NA` for 
## any `interval`. The total number of steps taken in such days are set to 0s by
## default. However, after replacing missing `steps` values with the mean `steps`
## of associated `interval` value, these 0 values are removed from the histogram
## of total number of steps taken each day.

## Are there differences in activity patterns between weekdays and weekends?

## First, let's find the day of the week for each measurement in the dataset. In
## this part, we use the dataset with the filled-in values.

```{r, echo=TRUE}
weekday.or.weekend <- function(date) {
    day <- weekdays(date)
    if (day %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))
        return("weekday")
    else if (day %in% c("Saturday", "Sunday"))
        return("weekend")
    else
        stop("invalid date")
}
filled.activity$date <- as.Date(filled.activity$date)
filled.activity$day <- sapply(filled.activity$date, FUN=weekday.or.weekend)
```

## Now, let's make a panel plot containing plots of average number of steps taken
## on weekdays and weekends.

```{r 5-minute_interval_Number_of_steps, echo=FALSE}
averages <- aggregate(steps ~ interval + day, filled.activity, mean)
ggplot(averages, aes(interval, steps)) + geom_line() + facet_grid(day ~ .) +
    xlab("5-minute interval") + ylab("Number of steps")
```
## Thanks for your time, your comment and contribution is highly welcome.
