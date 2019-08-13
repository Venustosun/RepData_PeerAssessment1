---
title: "Assignment"
author: "Sandra"
date: "2019Äê8ÔÂ13ÈÕ"
output: 
  html_document: 
    keep_md: yes
---
---
title: "assign 1"
author: "Sandra"
date: "2019Äê8ÔÂ13ÈÕ"
output: html_document
---
# Reproducible Research assignment

## Set working directory and Loading data

```r
setwd("C:\\Users\\qitcw\\Desktop\\activity")
act <- read.csv("activity.csv")
```

## What is mean total number of steps taken per day?
### 1. Calculate the total number of steps taken per day

```r
totals <- with(act, tapply(steps, date, sum, na.rm = T))
totalsdf <-data.frame(date = names(totals), sum = totals)
```
### 2. Make a histogram of the total number of steps taken each day

```r
plot1 <- hist(totalsdf[,2], breaks = 10, main = "total steps per day", xlab = "steps", col = "grey")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->
### 3. Calculate and report the mean and median of the total number of steps taken per day

```r
step <- as.vector(totalsdf[,2])
A <- mean(step)
B <- median(step)
```
The mean of the total number of steps taken per day is 9354.2295082.
The median of the total number of steps taken per day is 10395.

## What is the average daily activity pattern?
### 1. Make a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
ave <- aggregate(steps~interval, data = act, FUN = mean)
plot(ave$interval, ave$steps, type = "l", xlab = "interval", ylab ="average steps" ,main = "daily activity pattern", col = "steel blue" )
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->
### 2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
maxmatch <- subset(ave, steps == max(ave$steps))
```
The 835, 206.1698113 interval contains the maximum number of steps.

## Imputing missing values
### 1.Calculate and report the total number of missing values in the dataset

```r
nacount <- as.vector(is.na(act$steps))
nasum <- sum(nacount)
```
There are 2304 NAs.
### 2. Devise a strategy for filling in all of the missing values in the dataset

```r
daymean <- aggregate(steps~interval + date, data = act, FUN = mean)
actnew <- act
library(Hmisc)
```

```
## Loading required package: lattice
```

```
## Loading required package: survival
```

```
## Loading required package: Formula
```

```
## Loading required package: ggplot2
```

```
## 
## Attaching package: 'Hmisc'
```

```
## The following objects are masked from 'package:base':
## 
##     format.pval, units
```

```r
actnew$steps <- impute(act$steps, fun=mean)
```
### 3. Create a new dataset that is equal to the original dataset but with the missing data filled in

```r
actnew <- act
```
### 4.Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day

```r
actnew1 <- aggregate(steps~date, data = actnew, FUN = sum)
hist(actnew1$steps, xlab = "steps per day", main = "total steps per day", col = "steel blue")
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

```r
step1 <- as.vector(actnew[,1])
AA <- mean(step1)
BB <- median(step1)
```

## Are there differences in activity patterns between weekdays and weekends?
### 1. Create a new factor variable in the dataset with two levels ¨C ¡°weekday¡± and ¡°weekend¡± indicating whether a given date is a weekday or weekend day

```r
actnew$newdate <- as.Date(actnew$date, format = "%Y-%m-%d")
A <- weekdays(actnew$newdate)
head(A)
week <- ifelse(A == "æ¼ã¸°æŒ¼ã¸·æŒ¼ã¸¶æ¼ã¹¡æŒ¼ã¸±æ˜¼ã¸¹"| A == "æ¼ã¸°æŒ¼ã¸·æŒ¼ã¸¶æ¼ã¹¡æŒ¼ã¹£æ”¼ã¹£", "Weekand", "weekdays")
cbind(actnew, week)
ave <- aggregate(steps~interval + week, data = actnew, FUN = mean)
```
### 2. Make a panel plot containing a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis)

```r
sub1 <- subset(ave, week == "Weekand")
sub2 <- subset(ave, week == "weekdays")
par(mfrow = c(2,1))
plot(sub1$interval, sub1$steps, type = "l", xlab = "interval", ylab = "number of steps", main = "steps in weekand", col = "blue", xlim = c(0,2000), ylim = c(0,300))
plot(sub2$interval, sub2$steps, type = "l", xlab = "interval", ylab = "number of steps", main = "steps in weekdays", col = "red", xlim = c(0,2000),ylim= c(0,250))
```

![](PA1_template_files/figure-html/unnamed-chunk-12-1.png)<!-- -->
knit2html()
