# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

```r
data <- read.csv("activity.csv")
data <- data[complete.cases(data),]
```

## What is mean total number of steps taken per day?

```r
stepsPerDay <-aggregate(data$steps, by=list(data$date), FUN=sum)
hist(stepsPerDay$x, main="Histogram of Steps per day", xlab="Steps")
```

![](./PA1_template_files/figure-html/unnamed-chunk-2-1.png) 

```r
meanStepsPerDay <- mean(stepsPerDay$x)
medianStepsPerDay <- median(stepsPerDay$x)
```
### The mean total number of steps taken per day is 1.0766189\times 10^{4}
### The corresponding median for the steps taken per day is 10765

## What is the average daily activity pattern?

```r
meanStepsInterval <- aggregate(data$steps, by=list(data$interval), FUN=mean)

plot(meanStepsInterval$Group.1, meanStepsInterval$x, type="l",xlab="intervals", ylab="steps", col="blue")
```

![](./PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

```r
result <- meanStepsInterval[meanStepsInterval$x==max(meanStepsInterval$x),]$Group.1
```
### The interval, on average across all days with the maximum number of steps is 835

## Imputing missing values

```r
dataNA <-read.csv("activity.csv")
dataNA <-dataNA[!complete.cases(dataNA),]
nrowsNA <- nrow(dataNA)
```
### The number of missing values in the dataset is 2304
#### The Stragety used to fill the NAs values is to use the mean fot that 5-interval over all days.

## Are there differences in activity patterns between weekdays and weekends?
