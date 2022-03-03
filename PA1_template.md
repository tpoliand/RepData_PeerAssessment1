---
title: 'Reproducible Research: Peer Assessment 1'
output: 
  html_document:
    keep_md: true
---



## 1. Loading and preprocessing the data


```r
download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip", destfile = "activity.zip", mode="wb")
unzip("activity.zip")
stepdata = read.csv("activity.csv", header = TRUE)
```

## 2. What is mean total number of steps taken per day?


```r
library(magrittr)
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
databydate = stepdata %>% 
        select(date, steps) %>% 
        group_by(date) %>% 
        summarize(tsteps= sum(steps)) %>%
        na.omit() %>%
        print(n=60)
```

```
## # A tibble: 53 x 2
##    date       tsteps
##    <chr>       <int>
##  1 2012-10-02    126
##  2 2012-10-03  11352
##  3 2012-10-04  12116
##  4 2012-10-05  13294
##  5 2012-10-06  15420
##  6 2012-10-07  11015
##  7 2012-10-09  12811
##  8 2012-10-10   9900
##  9 2012-10-11  10304
## 10 2012-10-12  17382
## 11 2012-10-13  12426
## 12 2012-10-14  15098
## 13 2012-10-15  10139
## 14 2012-10-16  15084
## 15 2012-10-17  13452
## 16 2012-10-18  10056
## 17 2012-10-19  11829
## 18 2012-10-20  10395
## 19 2012-10-21   8821
## 20 2012-10-22  13460
## 21 2012-10-23   8918
## 22 2012-10-24   8355
## 23 2012-10-25   2492
## 24 2012-10-26   6778
## 25 2012-10-27  10119
## 26 2012-10-28  11458
## 27 2012-10-29   5018
## 28 2012-10-30   9819
## 29 2012-10-31  15414
## 30 2012-11-02  10600
## 31 2012-11-03  10571
## 32 2012-11-05  10439
## 33 2012-11-06   8334
## 34 2012-11-07  12883
## 35 2012-11-08   3219
## 36 2012-11-11  12608
## 37 2012-11-12  10765
## 38 2012-11-13   7336
## 39 2012-11-15     41
## 40 2012-11-16   5441
## 41 2012-11-17  14339
## 42 2012-11-18  15110
## 43 2012-11-19   8841
## 44 2012-11-20   4472
## 45 2012-11-21  12787
## 46 2012-11-22  20427
## 47 2012-11-23  21194
## 48 2012-11-24  14478
## 49 2012-11-25  11834
## 50 2012-11-26  11162
## 51 2012-11-27  13646
## 52 2012-11-28  10183
## 53 2012-11-29   7047
```


## 2. Make a histogram of the total number of steps taken each day


```r
library(magrittr)
library(dplyr)

databydate = stepdata %>% 
        select(date, steps) %>% 
        group_by(date) %>% 
        summarize(tsteps= sum(steps)) %>%
        na.omit() %>%
        print(n=60)
```

```
## # A tibble: 53 x 2
##    date       tsteps
##    <chr>       <int>
##  1 2012-10-02    126
##  2 2012-10-03  11352
##  3 2012-10-04  12116
##  4 2012-10-05  13294
##  5 2012-10-06  15420
##  6 2012-10-07  11015
##  7 2012-10-09  12811
##  8 2012-10-10   9900
##  9 2012-10-11  10304
## 10 2012-10-12  17382
## 11 2012-10-13  12426
## 12 2012-10-14  15098
## 13 2012-10-15  10139
## 14 2012-10-16  15084
## 15 2012-10-17  13452
## 16 2012-10-18  10056
## 17 2012-10-19  11829
## 18 2012-10-20  10395
## 19 2012-10-21   8821
## 20 2012-10-22  13460
## 21 2012-10-23   8918
## 22 2012-10-24   8355
## 23 2012-10-25   2492
## 24 2012-10-26   6778
## 25 2012-10-27  10119
## 26 2012-10-28  11458
## 27 2012-10-29   5018
## 28 2012-10-30   9819
## 29 2012-10-31  15414
## 30 2012-11-02  10600
## 31 2012-11-03  10571
## 32 2012-11-05  10439
## 33 2012-11-06   8334
## 34 2012-11-07  12883
## 35 2012-11-08   3219
## 36 2012-11-11  12608
## 37 2012-11-12  10765
## 38 2012-11-13   7336
## 39 2012-11-15     41
## 40 2012-11-16   5441
## 41 2012-11-17  14339
## 42 2012-11-18  15110
## 43 2012-11-19   8841
## 44 2012-11-20   4472
## 45 2012-11-21  12787
## 46 2012-11-22  20427
## 47 2012-11-23  21194
## 48 2012-11-24  14478
## 49 2012-11-25  11834
## 50 2012-11-26  11162
## 51 2012-11-27  13646
## 52 2012-11-28  10183
## 53 2012-11-29   7047
```

## 3. Calculate and report the mean and median of the total number of steps taken per day


```r
mean(databydate$tsteps)
```

```
## [1] 10766.19
```

```r
median(databydate$tsteps)
```

```
## [1] 10765
```

## 4. Time series plot


```r
library(ggplot2)

databyinterval <- stepdata %>% 
        select(interval, steps) %>% 
        na.omit() %>% 
        group_by(interval) %>% 
        summarize(tsteps= mean(steps)) 

ggplot(databyinterval, aes(x=interval, y=tsteps)) + geom_line()
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

## 4. Imputing missing values

```r
replacewithmean <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))

meandata <- stepdata %>% 
        group_by(interval) %>% 
        mutate(steps= replacewithmean(steps)) %>%
        print(n=60)
```

```
## # A tibble: 17,568 x 3
## # Groups:   interval [288]
##     steps date       interval
##     <dbl> <chr>         <int>
##  1 1.72   2012-10-01        0
##  2 0.340  2012-10-01        5
##  3 0.132  2012-10-01       10
##  4 0.151  2012-10-01       15
##  5 0.0755 2012-10-01       20
##  6 2.09   2012-10-01       25
##  7 0.528  2012-10-01       30
##  8 0.868  2012-10-01       35
##  9 0      2012-10-01       40
## 10 1.47   2012-10-01       45
## 11 0.302  2012-10-01       50
## 12 0.132  2012-10-01       55
## 13 0.321  2012-10-01      100
## 14 0.679  2012-10-01      105
## 15 0.151  2012-10-01      110
## 16 0.340  2012-10-01      115
## 17 0      2012-10-01      120
## 18 1.11   2012-10-01      125
## 19 1.83   2012-10-01      130
## 20 0.170  2012-10-01      135
## 21 0.170  2012-10-01      140
## 22 0.377  2012-10-01      145
## 23 0.264  2012-10-01      150
## 24 0      2012-10-01      155
## 25 0      2012-10-01      200
## 26 0      2012-10-01      205
## 27 1.13   2012-10-01      210
## 28 0      2012-10-01      215
## 29 0      2012-10-01      220
## 30 0.132  2012-10-01      225
## 31 0      2012-10-01      230
## 32 0.226  2012-10-01      235
## 33 0      2012-10-01      240
## 34 0      2012-10-01      245
## 35 1.55   2012-10-01      250
## 36 0.943  2012-10-01      255
## 37 0      2012-10-01      300
## 38 0      2012-10-01      305
## 39 0      2012-10-01      310
## 40 0      2012-10-01      315
## 41 0.208  2012-10-01      320
## 42 0.623  2012-10-01      325
## 43 1.62   2012-10-01      330
## 44 0.585  2012-10-01      335
## 45 0.491  2012-10-01      340
## 46 0.0755 2012-10-01      345
## 47 0      2012-10-01      350
## 48 0      2012-10-01      355
## 49 1.19   2012-10-01      400
## 50 0.943  2012-10-01      405
## 51 2.57   2012-10-01      410
## 52 0      2012-10-01      415
## 53 0.340  2012-10-01      420
## 54 0.358  2012-10-01      425
## 55 4.11   2012-10-01      430
## 56 0.660  2012-10-01      435
## 57 3.49   2012-10-01      440
## 58 0.830  2012-10-01      445
## 59 3.11   2012-10-01      450
## 60 1.11   2012-10-01      455
## # ... with 17,508 more rows
```

# 5. The 5-minute interval that, on average, contains the maximum number of steps

```r
databyinterval[which(databyinterval$tsteps == max(databyinterval$tsteps)),]
```

```
## # A tibble: 1 x 2
##   interval tsteps
##      <int>  <dbl>
## 1      835   206.
```

# 6. Code to describe and show a strategy for imputing missing data

```r
# code to describe
missingVals <- sum(is.na(data))
```

```
## Warning in is.na(data): is.na() applied to non-(list or vector) of type
## 'closure'
```

```r
## Warning in is.na(data): is.na() applied to non-(list or vector) of type
## 'closure'
missingVals
```

```
## [1] 0
```

```r
## [1] 0

# strategy for imputing missing data
replacewithmean <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))

meandata <- stepdata %>% 
        group_by(interval) %>% 
        mutate(steps= replacewithmean(steps))

meandata
```

```
## # A tibble: 17,568 x 3
## # Groups:   interval [288]
##     steps date       interval
##     <dbl> <chr>         <int>
##  1 1.72   2012-10-01        0
##  2 0.340  2012-10-01        5
##  3 0.132  2012-10-01       10
##  4 0.151  2012-10-01       15
##  5 0.0755 2012-10-01       20
##  6 2.09   2012-10-01       25
##  7 0.528  2012-10-01       30
##  8 0.868  2012-10-01       35
##  9 0      2012-10-01       40
## 10 1.47   2012-10-01       45
## # ... with 17,558 more rows
```

# 7. Histogram of the total number of steps taken each day after missing values are imputed

```r
FullSummedDataByDay <- aggregate(meandata$steps, by=list(meandata$date), sum)

FullSummedDataByDay
```

```
##       Group.1        x
## 1  2012-10-01 10766.19
## 2  2012-10-02   126.00
## 3  2012-10-03 11352.00
## 4  2012-10-04 12116.00
## 5  2012-10-05 13294.00
## 6  2012-10-06 15420.00
## 7  2012-10-07 11015.00
## 8  2012-10-08 10766.19
## 9  2012-10-09 12811.00
## 10 2012-10-10  9900.00
## 11 2012-10-11 10304.00
## 12 2012-10-12 17382.00
## 13 2012-10-13 12426.00
## 14 2012-10-14 15098.00
## 15 2012-10-15 10139.00
## 16 2012-10-16 15084.00
## 17 2012-10-17 13452.00
## 18 2012-10-18 10056.00
## 19 2012-10-19 11829.00
## 20 2012-10-20 10395.00
## 21 2012-10-21  8821.00
## 22 2012-10-22 13460.00
## 23 2012-10-23  8918.00
## 24 2012-10-24  8355.00
## 25 2012-10-25  2492.00
## 26 2012-10-26  6778.00
## 27 2012-10-27 10119.00
## 28 2012-10-28 11458.00
## 29 2012-10-29  5018.00
## 30 2012-10-30  9819.00
## 31 2012-10-31 15414.00
## 32 2012-11-01 10766.19
## 33 2012-11-02 10600.00
## 34 2012-11-03 10571.00
## 35 2012-11-04 10766.19
## 36 2012-11-05 10439.00
## 37 2012-11-06  8334.00
## 38 2012-11-07 12883.00
## 39 2012-11-08  3219.00
## 40 2012-11-09 10766.19
## 41 2012-11-10 10766.19
## 42 2012-11-11 12608.00
## 43 2012-11-12 10765.00
## 44 2012-11-13  7336.00
## 45 2012-11-14 10766.19
## 46 2012-11-15    41.00
## 47 2012-11-16  5441.00
## 48 2012-11-17 14339.00
## 49 2012-11-18 15110.00
## 50 2012-11-19  8841.00
## 51 2012-11-20  4472.00
## 52 2012-11-21 12787.00
## 53 2012-11-22 20427.00
## 54 2012-11-23 21194.00
## 55 2012-11-24 14478.00
## 56 2012-11-25 11834.00
## 57 2012-11-26 11162.00
## 58 2012-11-27 13646.00
## 59 2012-11-28 10183.00
## 60 2012-11-29  7047.00
## 61 2012-11-30 10766.19
```

```r
names(FullSummedDataByDay)[1] ="date"
names(FullSummedDataByDay)[2] ="totalsteps"

summary(FullSummedDataByDay)
```

```
##      date             totalsteps   
##  Length:61          Min.   :   41  
##  Class :character   1st Qu.: 9819  
##  Mode  :character   Median :10766  
##                     Mean   :10766  
##                     3rd Qu.:12811  
##                     Max.   :21194
```

```r
hist(FullSummedDataByDay$totalsteps, xlab = "Steps", ylab = "Frequency", main = "Total Daily Steps", breaks = 20)
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

# 8. Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends

```r
meandata$date <- as.Date(meandata$date)
meandata$weekday <- weekdays(meandata$date)
meandata$weekend <- ifelse(meandata$weekday=="Saturday" | meandata$weekday=="Sunday", "Weekend", "Weekday" )

library(ggplot2)

meandataweekendweekday <- aggregate(meandata$steps , by= list(meandata$weekend, meandata$interval), na.omit(mean))

names(meandataweekendweekday) <- c("weekend", "interval", "steps")

ggplot(meandataweekendweekday, aes(x=interval, y=steps, color=weekend)) + geom_line() +
        facet_grid(weekend ~.) + xlab("Interval") + ylab("Mean of Steps") +
        ggtitle("Comparison of Average Number of Steps in Each Interval")
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

