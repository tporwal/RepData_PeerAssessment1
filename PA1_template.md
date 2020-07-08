---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---

Code for reading the dataset

```r
activity<-read.csv("activity.csv",header = TRUE, sep = ",")
```

Understanding the dataset

```r
dim(activity)
```

```
## [1] 17568     3
```

```r
summary(activity)
```

```
##      steps            date              interval     
##  Min.   :  0.00   Length:17568       Min.   :   0.0  
##  1st Qu.:  0.00   Class :character   1st Qu.: 588.8  
##  Median :  0.00   Mode  :character   Median :1177.5  
##  Mean   : 37.38                      Mean   :1177.5  
##  3rd Qu.: 12.00                      3rd Qu.:1766.2  
##  Max.   :806.00                      Max.   :2355.0  
##  NA's   :2304
```

```r
head(activity)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```

Now, calculating the total number of steps taken per day and plotting Histogram

```r
daywiseSteps<- aggregate(activity$steps, by=list(Date=activity$date), FUN = sum)
hist(daywiseSteps$x, xlab = "Total number of steps taken per day", main = "Histogram of total steps taken per day",breaks = 30)
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

Calculating and reporting the mean and median of the total number of steps taken per day

```r
oldMean<-mean(na.omit(daywiseSteps$x))
oldMedian<-median(na.omit(daywiseSteps$x))
oldMean
```

```
## [1] 10766.19
```

```r
oldMedian
```

```
## [1] 10765
```

Makeing a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
library(ggplot2)
```

```
## Warning: package 'ggplot2' was built under R version 4.0.2
```

```r
fiveMinuteInterval<- aggregate(steps ~ interval,data = activity, FUN = mean)
timeseriesPlot<- ggplot(fiveMinuteInterval,aes(x=interval, y=steps))+ geom_line()+ xlab("5-minute interval")+ylab("Average number of steps taken, averaged across all days")+ggtitle("Time series plot of the 5-minute interval and the average number of steps taken, averaged across all days")
print(timeseriesPlot)
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

Finding out Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
head(fiveMinuteInterval)
```

```
##   interval     steps
## 1        0 1.7169811
## 2        5 0.3396226
## 3       10 0.1320755
## 4       15 0.1509434
## 5       20 0.0754717
## 6       25 2.0943396
```

```r
fiveMinuteInterval[which.max(fiveMinuteInterval$steps),]
```

```
##     interval    steps
## 104      835 206.1698
```

## Imputing missing values
Calculating and reporting the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
tolMissingValues<- sum(is.na(activity))
tolMissingValues
```

```
## [1] 2304
```
Devising a strategy for filling in all of the missing values in the dataset.

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
replacingWithMean <- function(x) {replace(x, is.na(x), mean(x, na.rm = TRUE))}
replaceData <- activity%>% group_by(interval) %>% mutate(steps= replacingWithMean(steps))
head(replaceData)
```

```
## # A tibble: 6 x 3
## # Groups:   interval [6]
##    steps date       interval
##    <dbl> <chr>         <int>
## 1 1.72   2012-10-01        0
## 2 0.340  2012-10-01        5
## 3 0.132  2012-10-01       10
## 4 0.151  2012-10-01       15
## 5 0.0755 2012-10-01       20
## 6 2.09   2012-10-01       25
```
Now, we will make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Moreover, also check whether these values differ from the estimates from the first part. What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
daywise<- aggregate(replaceData$steps, by=list(Date=replaceData$date), FUN = sum)
hist(daywise$x, xlab = "Total number of steps taken per day", main = "Histogram of total steps taken per day",breaks = 30)
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

```r
mean(daywise$x)
```

```
## [1] 10766.19
```

```r
median(daywise$x)
```

```
## [1] 10766.19
```

```r
oldMean
```

```
## [1] 10766.19
```

```r
oldMedian
```

```
## [1] 10765
```
We can see that their is change in medain after imputing missing values.

Now, Seeing are there differences in activity patterns between weekdays and weekends?

```r
replaceData$WeekendOrWeekday <- ifelse(weekdays(as.Date(replaceData$date)) %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"), "Weekday", "Weekend")
head(replaceData)
```

```
## # A tibble: 6 x 4
## # Groups:   interval [6]
##    steps date       interval WeekendOrWeekday
##    <dbl> <chr>         <int> <chr>           
## 1 1.72   2012-10-01        0 Weekday         
## 2 0.340  2012-10-01        5 Weekday         
## 3 0.132  2012-10-01       10 Weekday         
## 4 0.151  2012-10-01       15 Weekday         
## 5 0.0755 2012-10-01       20 Weekday         
## 6 2.09   2012-10-01       25 Weekday
```
Now, plotting a time series plot for Weekday or Weekend

```r
dataWeekendWeekday <- aggregate(replaceData$steps , by= list(replaceData$WeekendOrWeekday, replaceData$interval), na.omit(mean))
names(dataWeekendWeekday) <- c("weekend", "interval", "steps")
library(ggplot2)
ggplot(dataWeekendWeekday, aes(x=interval, y=steps, color=weekend)) + geom_line()+
facet_grid(weekend ~.) + xlab("Five minute interval") + ylab("Average number of steps taken across all weekday days or weekend days ") + ggtitle("Time Series Plot for Weekend or Weekday")
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png)<!-- -->
