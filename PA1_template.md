# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

We are getting the data using $read.csv$ function. Then we check what are the data
columns and values using $head$ function. We $aggregate$ to have 
average values by day and ignore time intervals. We give resulting columns new
$colnames$ using respective funciton. We embelish the dataset with weekday names
and numbers using $format$ and $wday$ functions.


```r
require(lubridate)
```

```
## Loading required package: lubridate
```

```r
tblload = read.csv("~/git/RepData_PeerAssessment1/activity.csv")

head(tblload)
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

```r
stpByDay= aggregate(tblload$steps, list(tblload$date), mean, na.action=na.omit)
colnames(stpByDay) = c('date','stpAvg')
dow <- function(x) format(as.Date(x), "%A")
stpByDay$weekDay <- dow(stpByDay$date)
stpByDay$weekDayNum <- wday(stpByDay$date)
table(stpByDay$weekDay)
```

```
## 
##    Friday    Monday  Saturday    Sunday  Thursday   Tuesday Wednesday 
##         9         9         8         8         9         9         9
```

Let's see what we have in tabular form. In the next section we will analyze plots.

```r
library(xtable)
print(stpByDay, type = "HTML")
```

```
##          date     stpAvg   weekDay weekDayNum
## 1  2012-10-01         NA    Monday          2
## 2  2012-10-02  0.4375000   Tuesday          3
## 3  2012-10-03 39.4166667 Wednesday          4
## 4  2012-10-04 42.0694444  Thursday          5
## 5  2012-10-05 46.1597222    Friday          6
## 6  2012-10-06 53.5416667  Saturday          7
## 7  2012-10-07 38.2465278    Sunday          1
## 8  2012-10-08         NA    Monday          2
## 9  2012-10-09 44.4826389   Tuesday          3
## 10 2012-10-10 34.3750000 Wednesday          4
## 11 2012-10-11 35.7777778  Thursday          5
## 12 2012-10-12 60.3541667    Friday          6
## 13 2012-10-13 43.1458333  Saturday          7
## 14 2012-10-14 52.4236111    Sunday          1
## 15 2012-10-15 35.2048611    Monday          2
## 16 2012-10-16 52.3750000   Tuesday          3
## 17 2012-10-17 46.7083333 Wednesday          4
## 18 2012-10-18 34.9166667  Thursday          5
## 19 2012-10-19 41.0729167    Friday          6
## 20 2012-10-20 36.0937500  Saturday          7
## 21 2012-10-21 30.6284722    Sunday          1
## 22 2012-10-22 46.7361111    Monday          2
## 23 2012-10-23 30.9652778   Tuesday          3
## 24 2012-10-24 29.0104167 Wednesday          4
## 25 2012-10-25  8.6527778  Thursday          5
## 26 2012-10-26 23.5347222    Friday          6
## 27 2012-10-27 35.1354167  Saturday          7
## 28 2012-10-28 39.7847222    Sunday          1
## 29 2012-10-29 17.4236111    Monday          2
## 30 2012-10-30 34.0937500   Tuesday          3
## 31 2012-10-31 53.5208333 Wednesday          4
## 32 2012-11-01         NA  Thursday          5
## 33 2012-11-02 36.8055556    Friday          6
## 34 2012-11-03 36.7048611  Saturday          7
## 35 2012-11-04         NA    Sunday          1
## 36 2012-11-05 36.2465278    Monday          2
## 37 2012-11-06 28.9375000   Tuesday          3
## 38 2012-11-07 44.7326389 Wednesday          4
## 39 2012-11-08 11.1770833  Thursday          5
## 40 2012-11-09         NA    Friday          6
## 41 2012-11-10         NA  Saturday          7
## 42 2012-11-11 43.7777778    Sunday          1
## 43 2012-11-12 37.3784722    Monday          2
## 44 2012-11-13 25.4722222   Tuesday          3
## 45 2012-11-14         NA Wednesday          4
## 46 2012-11-15  0.1423611  Thursday          5
## 47 2012-11-16 18.8923611    Friday          6
## 48 2012-11-17 49.7881944  Saturday          7
## 49 2012-11-18 52.4652778    Sunday          1
## 50 2012-11-19 30.6979167    Monday          2
## 51 2012-11-20 15.5277778   Tuesday          3
## 52 2012-11-21 44.3993056 Wednesday          4
## 53 2012-11-22 70.9270833  Thursday          5
## 54 2012-11-23 73.5902778    Friday          6
## 55 2012-11-24 50.2708333  Saturday          7
## 56 2012-11-25 41.0902778    Sunday          1
## 57 2012-11-26 38.7569444    Monday          2
## 58 2012-11-27 47.3819444   Tuesday          3
## 59 2012-11-28 35.3576389 Wednesday          4
## 60 2012-11-29 24.4687500  Thursday          5
## 61 2012-11-30         NA    Friday          6
```

## What is mean total number of steps taken per day?
Let's see what is the average mean and median, ignoring NA values.

```r
mean(stpByDay$stpAvg, na.rm=TRUE)
```

```
## [1] 37.3826
```

```r
median(stpByDay$stpAvg, na.rm=TRUE)
```

```
## [1] 37.37847
```


## What is the average daily activity pattern?
First we see the overall plot by all days, then we analyze the plot by weekdays.
Lastly we analyze the histogram to see what average number of steps are most 
common across the whole dataset.

```r
plot(stpByDay$stpAvg ~ stpByDay$date)
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png) 

```r
plot(stpByDay$weekDayNum, stpByDay$stpAvg)
```

![](PA1_template_files/figure-html/unnamed-chunk-4-2.png) 

```r
hist(stpByDay$stpAvg)
```

![](PA1_template_files/figure-html/unnamed-chunk-4-3.png) 

The next plot shows us that most steps are usually taken at around time interval 800-900:


```r
adt<-aggregate(steps~interval ,data=tblload, FUN=mean)
plot(adt$interval, adt$steps, type="l", xlab="time interval", ylab="average steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png) 

## Imputing missing values
Replace missing values with average mean steps.

```r
missingIdx = is.na(stpByDay$stpAvg)

stpByDay$stpAvg[missingIdx] = mean(stpByDay$stpAvg, na.rm=TRUE)
stpByDay
```

```
##          date     stpAvg   weekDay weekDayNum
## 1  2012-10-01 37.3825996    Monday          2
## 2  2012-10-02  0.4375000   Tuesday          3
## 3  2012-10-03 39.4166667 Wednesday          4
## 4  2012-10-04 42.0694444  Thursday          5
## 5  2012-10-05 46.1597222    Friday          6
## 6  2012-10-06 53.5416667  Saturday          7
## 7  2012-10-07 38.2465278    Sunday          1
## 8  2012-10-08 37.3825996    Monday          2
## 9  2012-10-09 44.4826389   Tuesday          3
## 10 2012-10-10 34.3750000 Wednesday          4
## 11 2012-10-11 35.7777778  Thursday          5
## 12 2012-10-12 60.3541667    Friday          6
## 13 2012-10-13 43.1458333  Saturday          7
## 14 2012-10-14 52.4236111    Sunday          1
## 15 2012-10-15 35.2048611    Monday          2
## 16 2012-10-16 52.3750000   Tuesday          3
## 17 2012-10-17 46.7083333 Wednesday          4
## 18 2012-10-18 34.9166667  Thursday          5
## 19 2012-10-19 41.0729167    Friday          6
## 20 2012-10-20 36.0937500  Saturday          7
## 21 2012-10-21 30.6284722    Sunday          1
## 22 2012-10-22 46.7361111    Monday          2
## 23 2012-10-23 30.9652778   Tuesday          3
## 24 2012-10-24 29.0104167 Wednesday          4
## 25 2012-10-25  8.6527778  Thursday          5
## 26 2012-10-26 23.5347222    Friday          6
## 27 2012-10-27 35.1354167  Saturday          7
## 28 2012-10-28 39.7847222    Sunday          1
## 29 2012-10-29 17.4236111    Monday          2
## 30 2012-10-30 34.0937500   Tuesday          3
## 31 2012-10-31 53.5208333 Wednesday          4
## 32 2012-11-01 37.3825996  Thursday          5
## 33 2012-11-02 36.8055556    Friday          6
## 34 2012-11-03 36.7048611  Saturday          7
## 35 2012-11-04 37.3825996    Sunday          1
## 36 2012-11-05 36.2465278    Monday          2
## 37 2012-11-06 28.9375000   Tuesday          3
## 38 2012-11-07 44.7326389 Wednesday          4
## 39 2012-11-08 11.1770833  Thursday          5
## 40 2012-11-09 37.3825996    Friday          6
## 41 2012-11-10 37.3825996  Saturday          7
## 42 2012-11-11 43.7777778    Sunday          1
## 43 2012-11-12 37.3784722    Monday          2
## 44 2012-11-13 25.4722222   Tuesday          3
## 45 2012-11-14 37.3825996 Wednesday          4
## 46 2012-11-15  0.1423611  Thursday          5
## 47 2012-11-16 18.8923611    Friday          6
## 48 2012-11-17 49.7881944  Saturday          7
## 49 2012-11-18 52.4652778    Sunday          1
## 50 2012-11-19 30.6979167    Monday          2
## 51 2012-11-20 15.5277778   Tuesday          3
## 52 2012-11-21 44.3993056 Wednesday          4
## 53 2012-11-22 70.9270833  Thursday          5
## 54 2012-11-23 73.5902778    Friday          6
## 55 2012-11-24 50.2708333  Saturday          7
## 56 2012-11-25 41.0902778    Sunday          1
## 57 2012-11-26 38.7569444    Monday          2
## 58 2012-11-27 47.3819444   Tuesday          3
## 59 2012-11-28 35.3576389 Wednesday          4
## 60 2012-11-29 24.4687500  Thursday          5
## 61 2012-11-30 37.3825996    Friday          6
```
No missing values are in the dataset now.

## Are there differences in activity patterns between weekdays and weekends?
Let's look at our plot by weekdays again. Saturday (day 6) and Sunday (day 1) 
look distinct, different from the rest of the days, they have less dispersion, their data points are tightly grouped.

```r
plot(stpByDay$weekDayNum, stpByDay$stpAvg)
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png) 

```r
hist(stpByDay$stpAvg)
```

![](PA1_template_files/figure-html/unnamed-chunk-7-2.png) 
