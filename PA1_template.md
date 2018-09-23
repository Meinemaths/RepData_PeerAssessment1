---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---




## Loading and preprocessing the data

The language is set at English, the required libraries are read.



```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(lattice)
Sys.setlocale("LC_ALL", "English")
```

```
## [1] "LC_COLLATE=English_United States.1252;LC_CTYPE=English_United States.1252;LC_MONETARY=English_United States.1252;LC_NUMERIC=C;LC_TIME=English_United States.1252"
```

The data is loaded:

```r
setwd( "C:/Users/corin/Desktop/coursera/course5/week2")

dataset_url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(dataset_url, "active.zip")
unzip("active.zip", exdir = "active_data")

data <- read.csv("activity.csv")
```


## What is mean total number of steps taken per day?

The data exist of the number of steps for every 5 seconds. First the data is grouped by day, then the number of steps is summed. The histogram shows the frequentie of the number of steps per day.


```r
group <- group_by(data, date)
steps_per_day <- summarize(group, sum(steps))
names(steps_per_day)[[2]] <- "total_steps"
hist(steps_per_day$total_steps, 
      xlab = "Total steps per day", 
      main = "Histogram of the total number of steps per day")
```

![](figs/fig-unnamed-chunk-3-1.png)<!-- -->

```r
dev.copy(png, file = "Daily_steps.png", height = 480, width = 480)
```

```
## png 
##   3
```

```r
dev.off()
```

```
## png 
##   2
```

The mean and median are calculated:

```r
mean(steps_per_day$total_steps, na.rm = TRUE)
```

```
## [1] 10766.19
```

```r
median(steps_per_day$total_steps, na.rm = TRUE)
```

```
## [1] 10765
```


## What is the average daily activity pattern?
The data is grouped by the interval to calculate the daily activity. The plot shows the daily average


```r
group_time <- group_by(data, interval)
steps_per_interval <- summarize(group_time, mean(steps, na.rm = TRUE))
names(steps_per_interval)[[2]] <- "average_steps"

plot(steps_per_interval$interval, 
     steps_per_interval$average_steps, 
     type = "l", 
     xlab = "Interval", 
     ylab = "Average steps per interval")
```

![](figs/fig-unnamed-chunk-5-1.png)<!-- -->

```r
dev.copy(png, file = "Steps_per_interval.png", height = 480, width = 480)
```

```
## png 
##   3
```

```r
dev.off()
```

```
## png 
##   2
```

The interval with the highest average number of steps is:

```r
steps_per_interval[which.max( steps_per_interval$average_steps ),1]
```

```
## # A tibble: 1 x 1
##   interval
##      <int>
## 1      835
```

## Imputing missing values

There are some missing values in the data. 
The total amount of missing numbers is:

```r
sum(is.na(data$steps))
```

```
## [1] 2304
```

The NA's will be replaced by the mean of the non-missing numbers:

```r
NAmean <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))
data[] <- lapply(data, NAmean)
```

```
## Warning in mean.default(x, na.rm = TRUE): argument is not numeric or
## logical: returning NA
```

The new histogram has a higher number of steps, averaged by day. 


```r
steps_per_day_2 <- tapply(data$steps,data$date,sum)

hist(steps_per_day_2,
     xlab = "Total steps per day", 
     main = "Histogram of the total number of steps per day")
```

![](figs/fig-unnamed-chunk-9-1.png)<!-- -->

```r
dev.copy(png, file = "Daily_steps_NA.png", height = 480, width = 480)
```

```
## png 
##   3
```

```r
dev.off()
```

```
## png 
##   2
```

The calculated mean and median of the total number of steps per day:


```r
mean(steps_per_day_2)
```

```
## [1] 10766.19
```

```r
median(steps_per_day_2)
```

```
## [1] 10766.19
```
## Are there differences in activity patterns between weekdays and weekends?

FOr the differences between working days and weekend, the data first is divided into these two categories:

```r
data$date<- as.POSIXct(as.character(data$date))
data$weekday <- weekdays(data$date)

data$day <- ifelse((data$weekday == "Monday" | 
                      data$weekday == "Tuesday"|
                      data$weekday == "Wednesday"|
                      data$weekday == "Thursday"|
                      data$weekday == "Friday"),
                    "workday",
                    "weekend")
```

The data is grouped by the days and the interval, to make a plot of the average number of steps in the two categories. 

```r
group <- group_by(data, interval)
group2 <- group_by(group, day, add = TRUE)
steps_per_interval <- summarize(group2, mean(steps, na.rm = TRUE))
names(steps_per_interval)[[3]] <- "average_steps"


attach(steps_per_interval)

par(mfrow=c(2,1))
xyplot(average_steps~interval|day,
            xlab="Interval",
            ylab ="Number of steps",
            layout = c(1,2),
            type = "l")
```

![](figs/fig-unnamed-chunk-12-1.png)<!-- -->

```r
dev.copy(png, file = "workday_weekend.png", height = 480, width = 480)
```

```
## png 
##   3
```

```r
dev.off()
```

```
## png 
##   2
```
