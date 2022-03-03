---
title: 'Reproducible Research: Peer Assessment 1'
output: 
  html_document:
    keep_md: true
---



## 1. Loading and preprocessing the data


```r
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

## 3. What is the average daily activity pattern?


```r
library(ggplot2)

databyinterval <- stepdata %>% 
        select(interval, steps) %>% 
        na.omit() %>% 
        group_by(interval) %>% 
        summarize(tsteps= mean(steps)) 

ggplot(databyinterval, aes(x=interval, y=tsteps)) + geom_line()
```

![](PA1_template_files/figure-html/3-1.png)<!-- -->

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

## 5. Are there differences in activity patterns between weekdays and weekends?


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

![](PA1_template_files/figure-html/5-1.png)<!-- -->


