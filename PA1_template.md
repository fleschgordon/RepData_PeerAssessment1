# Reproducible Research: Peer Assessment 1
fleschgordon  
8 August 2016  


```r
knitr::opts_chunk$set(echo = TRUE)
```

## Loading and preprocessing the data
1. Load the data (i.e. read.csv())


```r
fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(fileUrl, destfile ="./data/activity.zip")

unzip("./data/activity.zip", exdir = "./data")
dtactivity <- read.csv("./data/activity.csv")
```
2. Process/transform the data (if necessary) into a format suitable for your analysis

```r
dtactivity$date<- as.Date(dtactivity$date)
```

## What is mean total number of steps taken per day?
###1. Make a histogram of the total number of steps taken each day

```r
stepsByDate <- aggregate(steps ~ date , dtactivity, sum)
hist(stepsByDate$steps, breaks=10, main="The total number of steps per date", xlab="Number of Steps")
```

![](PA1_template_files/figure-html/hist-1.png)<!-- -->


###2. Calculate and report the mean and median total number of steps taken per day

```r
mean(stepsByDate$steps)
```

```
## [1] 10766.19
```

```r
median(stepsByDate$steps)
```

```
## [1] 10765
```

## What is the average daily activity pattern?
### 1. Make a time series plot (i.e. type = “l” ) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
stepsMeanByIntervall <- aggregate(steps ~ interval , dtactivity, mean)
plot(stepsMeanByIntervall$interval, stepsMeanByIntervall$steps, 
        type = "l", 
        xlab = "5 minute intervall", 
        ylab="average number of steps", 
        main="average daily activity pattern")
```

![](PA1_template_files/figure-html/daily activity-1.png)<!-- -->

### 2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
subset(stepsMeanByIntervall, steps==max(stepsMeanByIntervall$steps), select = c("interval"))
```

```
##     interval
## 104      835
```


## Imputing missing values
###1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
nrow(dtactivity[!complete.cases(dtactivity$steps),])
```

```
## [1] 2304
```


###2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
###3. Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
naRows <- is.na(dtactivity$steps)
AverageStepsperIntervall <- tapply(dtactivity$steps, dtactivity$interval, mean, na.rm = TRUE, simplify = T)

dtactivity2 <- dtactivity

dtactivity2$steps[naRows] <- AverageStepsperIntervall[as.character(dtactivity2$interval[naRows])]

sum(is.na(dtactivity2))
```

```
## [1] 0
```

###4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
stepsByDate2 <- aggregate(steps ~ date , dtactivity2, sum)
hist(stepsByDate2$steps, breaks=10, main="The total number of steps per date", xlab="Number of Steps")
```

![](PA1_template_files/figure-html/hist2-1.png)<!-- -->
Mean and Median of the modified dataset with the imputing missing data

```r
mean(stepsByDate2$steps)
```

```
## [1] 10766.19
```

```r
median(stepsByDate2$steps)
```

```
## [1] 10766.19
```
The impact of imputing missing data is that now the mean and the median has now the same value.


## Are there differences in activity patterns between weekdays and weekends?
###1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.


```r
dtactivity2$wend <- as.factor(ifelse(weekdays( dtactivity2$date) %in% c("Samstag","Sonntag"), "Weekend", "Weekday"))
```


###2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). The plot should look something like the following, which was creating using simulated data:


```r
stepsByIntervalWend <- aggregate(steps ~ interval + wend , dtactivity2, mean)
library(lattice)
xyplot(steps ~ interval|wend, data=stepsByIntervalWend, type="l", layout= c(1,2))
```

![](PA1_template_files/figure-html/ts plot weekdays-1.png)<!-- -->

