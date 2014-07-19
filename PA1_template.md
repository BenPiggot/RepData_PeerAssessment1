
# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data
Load the data set into R and transform the steps and interval variables.

```r
activity <- read.csv("activity.csv")
activity$steps <- as.numeric(activity$steps)
activity$interval <- as.numeric(activity$interval)
activity$interval <- unique(activity$interval)
summary(activity)
```

```
##      steps               date          interval   
##  Min.   :  0.0   2012-10-01:  288   Min.   :   0  
##  1st Qu.:  0.0   2012-10-02:  288   1st Qu.: 589  
##  Median :  0.0   2012-10-03:  288   Median :1178  
##  Mean   : 37.4   2012-10-04:  288   Mean   :1178  
##  3rd Qu.: 12.0   2012-10-05:  288   3rd Qu.:1766  
##  Max.   :806.0   2012-10-06:  288   Max.   :2355  
##  NA's   :2304    (Other)   :15840
```

## What is mean total number of steps taken per day?
Use the aggregaate() function to sum steps over unique dates. Then, create a histogram using the results aggregate function (sum total of steps per date). Then, determine the mean and median for number of steps per day of the sum total of steps per date.

```r
ActivitySum <-aggregate(activity$steps ~ activity$date, data=activity, sum, na.rm=TRUE)
hist(ActivitySum[,2], main="Step Totals per Day", xlab="Number of Steps", breaks=10)
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2.png) 

```r
mean(ActivitySum[,2])
```

```
## [1] 10766
```

```r
median(ActivitySum[,2])
```

```
## [1] 10765
```

## What is the average daily activity pattern?
Use the aggregate() function again to find the mean number of steps taken per 5 minute intervals. Then, plot the mean number of steps per interval. 

```r
ActivityMean <-aggregate(activity$steps ~ activity$interval, data=activity, mean, na.rm=TRUE)

plot(ActivityMean[,2] ~ unique(activity$interval), type="l", ylab="Number of Steps", xlab="Time of Day (in Five Minute Intervals)", main="Average Number of Steps at Different Times of Day")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 

## Imputing missing values
First, determine the numbe of NAs in the activity dataframe. Then, create a new variable column in the activity dataframe, StepMeans (StepMeans is the mean number of steps taken per 5 minutes intervals determined above). Then, use a for() loop to replace the NA values in the steps variable with the mean number of steps for the corresponding 5 minute interval, updating the original activity dataframe. Finally, use the aggregate function to determine the sum total steps over unique dates in updated dataframe. Create a histogram and determine the mean and median number of steps per day based ont the results. Note that the updated dataframe (with NA values replaced) has the same mean number of steps per day as the original data, while the mean is almost exactly the same. This is not surprising, as the NA values were replaced with 5 minute interval averages taken from the original dataframe. 

```r
sum(is.na(activity$steps)) + sum(is.na(activity$interval)) + sum(is.na(activity$date))
```

```
## [1] 2304
```

```r
activity$StepMeans <- ActivityMean[,2]

for(i in 1:nrow(activity)){
  if (is.na(activity[i,1])) {
    activity[i,1] <- activity[i,4]
  }
}

summary(activity)
```

```
##      steps               date          interval      StepMeans     
##  Min.   :  0.0   2012-10-01:  288   Min.   :   0   Min.   :  0.00  
##  1st Qu.:  0.0   2012-10-02:  288   1st Qu.: 589   1st Qu.:  2.49  
##  Median :  0.0   2012-10-03:  288   Median :1178   Median : 34.11  
##  Mean   : 37.4   2012-10-04:  288   Mean   :1178   Mean   : 37.38  
##  3rd Qu.: 27.0   2012-10-05:  288   3rd Qu.:1766   3rd Qu.: 52.83  
##  Max.   :806.0   2012-10-06:  288   Max.   :2355   Max.   :206.17  
##                  (Other)   :15840
```

```r
ActivityMean2 <- aggregate(activity[,1] ~ activity[,2], data=activity, sum, na.rm=TRUE)

hist(ActivityMean2[,2], main="Step Totals per Day", xlab="Number of Steps", breaks=10)
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4.png) 

```r
mean(ActivityMean2[,2])
```

```
## [1] 10766
```

```r
median(ActivityMean2[,2])
```

```
## [1] 10766
```

## Are there differences in activity patterns between weekdays and weekends?
Create a new variable, days, using the as.Date() and weekdays() function. Then, using the ifelse() function, create a two level factor variable that divides the days variable into weekdays or weekends. Finally, use lattice  to create a two panel plot showing how the number of steps taken per 5 minute intervals each day differs on weekdays and weekend days.

```r
activity$days <- weekdays(as.Date(activity$date))

activity$DayType <- ifelse(activity$days == "Monday" |  activity$days == "Tuesday" |
                           activity$days == "Wednesday" | activity$days == "Thursday" | 
                           activity$days == "Friday", c("Weekday"), c("Weekend"))

activity$DayType <- as.factor(activity$DayType)

summary(activity)
```

```
##      steps               date          interval      StepMeans     
##  Min.   :  0.0   2012-10-01:  288   Min.   :   0   Min.   :  0.00  
##  1st Qu.:  0.0   2012-10-02:  288   1st Qu.: 589   1st Qu.:  2.49  
##  Median :  0.0   2012-10-03:  288   Median :1178   Median : 34.11  
##  Mean   : 37.4   2012-10-04:  288   Mean   :1178   Mean   : 37.38  
##  3rd Qu.: 27.0   2012-10-05:  288   3rd Qu.:1766   3rd Qu.: 52.83  
##  Max.   :806.0   2012-10-06:  288   Max.   :2355   Max.   :206.17  
##                  (Other)   :15840                                  
##      days              DayType     
##  Length:17568       Weekday:12960  
##  Class :character   Weekend: 4608  
##  Mode  :character                  
##                                    
##                                    
##                                    
## 
```

```r
ActivityMean3 <- aggregate(activity$steps ~ activity$interval + activity$DayType, data=activity, mean)

library(lattice)

xyplot(ActivityMean3[,3] ~ ActivityMean3[,1] | ActivityMean3[,2], type ="l",
       main=list(cex=1.37, label="Difference in Activity Patterns, Weekends vs. Weekdays"),
       ylab="Number of Steps", xlab="Time of Day (in Five Minute Intervals)",
       layout=c(1,2))
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5.png) 

