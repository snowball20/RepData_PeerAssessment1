---
output: 
  word_document: 
    keep_md: yes
---
REPORT personal activity monitoring device
==========================================

Hi I am PAvitra this is my course project 1  

1.Code for reading datasets


```r
setwd("C:/Users/Pavitra/Desktop/repdata_data_activity")
Dataload<-read.csv("activity.csv")
```

2.Histogram of total no of steps each day

```r
totalstep<-aggregate(steps~date,Dataload,sum)
hist(totalstep$steps,xlab = "steps per day")
```

![](assignment1_files/figure-docx/unnamed-chunk-2-1.png)<!-- -->

```r
mean1<-mean(totalstep$steps)
median1<-median(totalstep$steps)
```

3.Mean and median number of steps taken each day  
MEAN=1.0766189\times 10^{4}  
MEDIAN=10765    

4.Time series plot of the average number of steps taken

```r
tspattern<-aggregate(steps~interval,Dataload,mean)
plot(tspattern$interval/1000,tspattern$steps,type = "l",xlab = "5 minute interval",ylab = "days",main = "time series plot")
```

![](assignment1_files/figure-docx/unnamed-chunk-3-1.png)<!-- -->

5.The 5-minute interval that contains the maximum number of steps 

```r
result<-tspattern[which.max(tspattern[,2]),1]
```
835

6.Code to describe and show a strategy for imputing missing data

```r
missingdata<-is.na(Dataload[,1])
```
7.Histogram of the total number of steps taken each day after missing values are imputed

```r
new<-Dataload
new[missingdata,1]<-mean(tspattern$steps)
head(new)
```

```
##     steps       date interval
## 1 37.3826 2012-10-01        0
## 2 37.3826 2012-10-01        5
## 3 37.3826 2012-10-01       10
## 4 37.3826 2012-10-01       15
## 5 37.3826 2012-10-01       20
## 6 37.3826 2012-10-01       25
```

```r
tspd<-aggregate(steps~date,new,sum)
hist(tspd$steps,xlab = "steps",ylab = "days")
```

![](assignment1_files/figure-docx/unnamed-chunk-6-1.png)<!-- -->

```r
mean2<-mean(tspd$steps)
median2<-median(tspd$steps)
```
MEAN=1.0766189\times 10^{4}  
MEDIAN=1.0766189\times 10^{4}   

8.Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends

```r
m<-aggregate(steps~interval,Dataload,mean)
new[missingdata,1]<-mean(m$steps)

new$date<-as.Date(new$date)
head(new)
```

```
##     steps       date interval
## 1 37.3826 2012-10-01        0
## 2 37.3826 2012-10-01        5
## 3 37.3826 2012-10-01       10
## 4 37.3826 2012-10-01       15
## 5 37.3826 2012-10-01       20
## 6 37.3826 2012-10-01       25
```

```r
new$daytype<-sapply(new$date,function(x){ ifelse(weekdays(x)=="Saturday"|weekdays(x)=="Sunday","Weekend","Weekday")})
full<-aggregate(steps~daytype+interval,new,mean)
library(lattice)
xyplot(steps~interval|factor(daytype),type="l",data = full)
```

![](assignment1_files/figure-docx/unnamed-chunk-7-1.png)<!-- -->
