# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data
1. Set working directory 
2. Construct function to modify the intervals in the data to hour basis.
	- The data are in hour-minute format which if not modified create 
		artifacts in the plots which give apparence of straght line at 
		the end of each hours.
3. Read in data file
4. Convert interval into hours basis
 

```r
setwd("C:/Users/Kenneth/Documents/RepData_PeerAssessment1")

interval <- function(x){
  hour <- floor(x/100)
  min = x - hour*100
  return(hour+min/60)
  }

data <- read.csv("activity/activity.csv")
data$interval <- interval(data$interval)
```


## What is mean total number of steps taken per day?
1. Create a dataframe with needed data - sum of steps by date
2. plot histogram
3. Print out mean and median of the daily steps counts


```r
byDate <-aggregate(steps ~ date, data, sum)
hist(byDate$steps,xlab="Steps",main="Histogram of Daily Steps, Missing data Ignored")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png) 

```r
summary(byDate$steps)[3:4]
```

```
## Median   Mean 
##  10760  10770
```

## What is the average daily activity pattern?
1. Create a dataframe for needed data - mean of steps by intervals
2. plot line graph
3. determine the maximun steps counts among the intervals
4. identify which of the interval give the maxiumum amount
5. print out the interval and data thereof. Note that the interval in the priginal form 
	would have been 835. 


```r
byInterval <- aggregate(steps ~ interval, data, mean)
plot(byInterval, type="l", xlab="Interval (hour)", main="Missing Data Ignored")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

```r
max <- max(byInterval$steps)
byInterval$max = (byInterval$steps==max)
subset(byInterval, max == TRUE)
```

```
##     interval    steps  max
## 104 8.583333 206.1698 TRUE
```


## Imputing missing values
1. Print out the number of rows with missing data


```r
nrow(subset(data, is.na(steps)))
```

```
## [1] 2304
```

2. Impute missing value by substituting the average value for that interval
3. Create a dataframe with missing value imputed 


```r
data2 <- merge(data, byInterval[,c("interval", "steps")], by="interval")
names(data2) <- c("interval", "original", "date", "ave") 
data2$steps = ifelse(is.na(data2$original), data2$ave, data2$original)
```

4 The number of usable data have been increased by about 13%. But the mean and median
	of the data have not changed much. Both dialy step histogram and line graph by 
	interval remain about the same


```r
byDate2 <-aggregate(steps ~ date, data2, sum)
hist(byDate2$steps,xlab="Steps",main="Histogram of Daily Steps, Missing data Imputed")
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png) 

```r
summary(byDate2$steps)[3:4]
```

```
## Median   Mean 
##  10770  10770
```

```r
byInterval2 <- aggregate(steps ~ interval, data2, mean)
plot(byInterval2, type="l", xlab="Interval (hour)",main="Missing Data Imputed")
```

![](PA1_template_files/figure-html/unnamed-chunk-6-2.png) 

## Are there differences in activity patterns between weekdays and weekends?
1. A variable is added indicating the date is a weekend or not to the data
2. A line grpah by interval is generated for weekend data and for weekday 
	seperatedly
3. These two graph is printed in the same page one above the other.
4. It shows that the individual is more active in the weekend. 



```r
library(chron)
```

```
## Warning: package 'chron' was built under R version 3.1.3
```

```r
data2$weekend <- is.weekend(as.Date(data2$date,'%Y-%m-%d'))

data_we <- subset(data2, weekend==TRUE)
byI_we <- aggregate(steps ~ interval, data_we, mean)

data_wd <- subset(data2, weekend==FALSE)
byI_wd <- aggregate(steps ~ interval, data_wd, mean)

old.par <- par(mfrow=c(2, 1))
plot(byI_wd, type="l", main="Weekday", xlab="Interval (hour)")
plot(byI_we, type="l", main="Weekend", xlab="Interval (hour)")
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png) 

```r
par(old.par)
```