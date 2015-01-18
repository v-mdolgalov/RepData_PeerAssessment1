# Reproducible Research: Peer Assessment 1


### Loading and preprocessing the data

We are getting the data using $read.csv$ function. We embelish the dataset with weekday names and numbers using $format$ and $wday$ functions.


```r
require(lubridate)
```

```
## Loading required package: lubridate
```

```r
dat1 = read.csv("~/git/RepData_PeerAssessment1/activity.csv")

dow <- function(x) format(as.Date(x), "%A")
dat1$weekDay <- dow(dat1$date)
dat1$weekDayNum <- wday(dat1$date)

datetotals = aggregate(steps ~ date, data = dat1, FUN=sum)
```
### What is mean total number of steps taken per day?
First we build histogram of total numer of steps

```r
hist(datetotals$steps, xlab = "Total Daily Number of Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png) 


Let's see what is the average mean and median, ignoring NA values.

```r
mean(datetotals$steps, na.rm=TRUE)
```

```
## [1] 10766.19
```

```r
median(datetotals$steps, na.rm=TRUE)
```

```
## [1] 10765
```

### What is the average daily activity pattern?
1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
avdaily = aggregate(steps ~ interval, data = dat1, FUN = mean)
plot(steps ~ interval, data=avdaily, type = "l", xlab = "Time Interval", ylab = "Mean # of Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png) 

```r
avdaily[which.max(avdaily$steps),]
```

```
##     interval    steps
## 104      835 206.1698
```

*Result*: interval 835 has the largest number of steps.

###Imputing Missing Values
1. Calculate total number of NAs

```r
allNAs = is.na(dat1$steps)
sum(allNAs)
```

```
## [1] 2304
```
2. Replace NAs with mean for the same date.
Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
avgSteps = mean(dat1$steps, na.rm = TRUE)
avgSteps
```

```
## [1] 37.3826
```

```r
dat2 = dat1 # head(dat2)
datetotals$avgSteps = datetotals$steps/288
m2= merge(dat1, datetotals, by.x = "date", by.y = "date")
dat2 = dat1
dat2$steps[allNAs] = m2$avgSteps[allNAs] 
```

Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
datetotals2 = aggregate(steps ~ date, data = dat2, FUN=sum)
hist(datetotals2$steps, xlab = "Total Daily Number of Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png) 


Let's see what is the average mean and median, ignoring NA values.

```r
mean(datetotals2$steps)
```

```
## [1] 10447.65
```

```r
median(datetotals2$steps)
```

```
## [1] 10585.5
```


*Impact*: histogram shows more occurances near the center of the distribution, 
in general all the dates now have more steps because more values appeared. Both
_median_ and _mean_ have changed after imputing the missing values.

###Are there differences in activity patterns between weekdays and weekends?

Using the dataset with the filled-in missing values for this part.


```r
dat2$daytype[dat2$weekDayNum == 1 | dat2$weekDayNum == 7] = "Weekend"
dat2$daytype[dat2$weekDayNum != 1 & dat2$weekDayNum != 7] = "Workday"
avdailyWE = aggregate(steps ~ interval, data = dat2[dat2$daytype == "Weekend",], FUN = mean)
avdailyWD = aggregate(steps ~ interval, data = dat2[dat2$daytype == "Workday",], FUN = mean)

plot(steps ~ interval, data=avdailyWD, type = "l", main = "Activity chart for workdays", xlab = "Time Interval", ylab = "Mean # of Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png) 

```r
plot(steps ~ interval, data=avdailyWE, type = "l", main = "Activity chart for weekends", xlab = "Time Interval", ylab = "Mean # of Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-9-2.png) 

*Result*: workdays show significantly less activity in the middle of the day and more steps before the beginning of work hours. Weekend days show more activity throught the day, and there is not such a big spike around 9 a.m. as on workdays.

*Thank you for reviewing my work. Have a great day!*
