# Reproducible Research: Peer Assessment 1

## Loading and preprocessing the data

```r
unzip("activity.zip")
data <- read.csv("activity.csv", colClasses = c("numeric", "character", "numeric"), 
    na.strings = c("NA"))
```


## What is mean total number of steps taken per day?

```r
total_steps <- tapply(data$steps, data$date, sum)
hist(total_steps, ylim = c(1, 30), main = "Frequency of steps taken per day", 
    xlab = "Total steps per day")
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1.png) 


#### Mean of total number of steps taken per day

```r
mean(total_steps, na.rm = TRUE)
```

[1] 10766


#### Median of total number of steps taken per day

```r
median(total_steps, na.rm = TRUE)
```

[1] 10765


## What is the average daily activity pattern?

```r
average_activity <- tapply(data$steps, data$interval, mean, na.rm = TRUE)
average_activity_df <- data.frame(key = as.numeric(names(average_activity)), 
    value = as.numeric(average_activity))
colnames(average_activity_df) <- c("Interval", "AverageSteps")
plot(average_activity_df$AverageSteps ~ average_activity_df$Interval, type = "l", 
    ylab = "Average Daily Activity", xlab = "Interval")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4.png) 


### Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
average_activity_df[which.max(average_activity_df$AverageSteps), ]$Interval
```

```
## [1] 835
```


## Imputing missing values
### Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
sum(is.na(data))
```

```
## [1] 2304
```


### Devise a strategy for filling in all of the missing values in the dataset.
#### If value in data is "NA", then replace with the average daily activity for that interval

```r
data_imputed <- within(data, steps <- ifelse(is.na(steps), average_activity_df[(average_activity_df$Interval == 
    interval), ]$AverageSteps, steps))
total_steps_imputed <- tapply(data_imputed$steps, data_imputed$date, sum)
hist(total_steps_imputed, ylim = c(1, 30), main = "Frequency of steps taken per day (Imputed Data)", 
    xlab = "Total steps per day")
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7.png) 


#### Mean of total number of steps taken per day (Imputed)

```r
mean(total_steps_imputed, na.rm = TRUE)
```

[1] 10766


#### Median of total number of steps taken per day (Imputed)

```r
median(total_steps_imputed, na.rm = TRUE)
```

[1] 10766


#### As we can see from the values above the imputed data histogram, mean and median produce the same results as the original, non-imputed, data

## Are there differences in activity patterns between weekdays and weekends?
### Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.


```r
data_imputed$date <- strptime(data_imputed$date, format = "%Y-%m-%d")
data_imputed$Day <- ifelse(weekdays(data_imputed$date) %in% c("Saturday", "Sunday"), 
    "Weekend", "Weekday")
data_imputed$Day <- as.factor(data_imputed$Day)
```


### Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).


```r
data_weekday = subset(data_imputed, Day == "Weekday")
average_activity_weekday <- tapply(data_weekday$steps, data_weekday$interval, 
    mean, na.rm = TRUE)
average_activity_weekday_df <- data.frame(key = as.numeric(names(average_activity_weekday)), 
    value = as.numeric(average_activity_weekday))
colnames(average_activity_weekday_df) <- c("Interval", "AverageSteps")

data_weekend = subset(data_imputed, Day == "Weekend")
average_activity_weekend <- tapply(data_weekend$steps, data_weekend$interval, 
    mean, na.rm = TRUE)
average_activity_weekend_df <- data.frame(key = as.numeric(names(average_activity_weekend)), 
    value = as.numeric(average_activity_weekend))
colnames(average_activity_weekend_df) <- c("Interval", "AverageSteps")

par(mfrow = c(2, 1))
plot(average_activity_weekend_df$AverageSteps ~ average_activity_weekend_df$Interval, 
    type = "l", ylab = "Average Weekend Activity", xlab = "Interval")
plot(average_activity_weekday_df$AverageSteps ~ average_activity_weekday_df$Interval, 
    type = "l", ylab = "Average Weekday Activity", xlab = "Interval")
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11.png) 



