


setwd("/home/jorgen/Documents/coursera/ReproducibleResearch/RepData_PeerAssessment1")
list.files()

#download data

#read data
activity<-read.csv(unz("activity.zip", "activity.csv"))
head(activity)
str(activity)

#format date
library(lubridate)
activity$date<-ymd(activity$date)

#total number of steps per day

library(dplyr)
totalsteps<-activity%>%
    group_by(date)%>%
    summarise(total = sum(steps))
head(totalsteps)    

avgtotal<-round(mean(totalsteps$total,na.rm=T),2)
medtotal<-median(totalsteps$total,na.rm=T)

#histogram of total number of steps per day
library(ggplot2)
bw <- diff(range(totalsteps$total)) / (2 * IQR(totalsteps$total) / length(totalsteps$total)^(1/3)) #calculates optimum binwidth

qplot(total,data=totalsteps,
      geom="histogram",
      binwidth=1000,
      xlab="Total number of steps taken each day",
      main = "Histogram of total number of steps taken each day",
      fill =I("antiquewhite4"))

#Daily activity pattern
# 
# Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average 
# number of steps taken, averaged across all days (y-axis)
# 
# Which 5-minute interval, on average across all the days in the dataset, 
# contains the maximum number of steps?

actpattern<-activity%>%
    group_by(interval)%>%
    summarise(avg_steps_interval=mean(steps,na.rm=T),
              med_steps_interval=median(steps,na.rm=T))

with(actpattern,plot(interval,avg_steps_interval,type="l"))

max_avg_steps_per_int<-max(actpattern$avg_steps_interval)
max_avg_steps_per_int

#five min interval with average maximum number of steps
actpattern[actpattern$avg_steps_interval==max_avg_steps_per_int,"interval"] #835

abline(v=835, col="red")
text(900,200,"max")


# Imputing missing values
# 
# Note that there are a number of days/intervals where there are missing values (coded as NA). 
# The presence of missing days may introduce bias into some calculations or summaries of the data.
# 
# Calculate and report the total number of missing values in the dataset 
# (i.e. the total number of rows with NAs)
# 
# Devise a strategy for filling in all of the missing values in the dataset. 
# The strategy does not need to be sophisticated. For example, you could use the mean/median 
# for that day, or the mean for that 5-minute interval, etc.
# 
# Create a new dataset that is equal to the original dataset but with the missing data filled in.
# 
# Make a histogram of the total number of steps taken each day and Calculate and report the mean 
# and median total number of steps taken per day. Do these values differ from the estimates from 
# the first part of the assignment? What is the impact of imputing missing data on the estimates 
# of the total daily number of steps?


#number of missing valuesSAP

countnull<-function(s){
    y<-sapply(s,is.na)
    sum(y)
}

countnull(activity$steps)  #2304

#impute missing values
a_i<-activity
a_i$steps.i<-ifelse(is.na(activity$steps),actpattern$med_steps_interval[which(activity$interval==actpattern$interval)],activity$steps)

activity_bu<-activity
activity$steps<-ifelse(is.na(activity$steps),actpattern$avg_steps_interval[which(activity$interval==actpattern$interval)],activity$steps)

#histogram of the total number of steps taken each day
total

hist(activity$steps)

# mean and median total number of steps taken per day

#comparson of imputed vs original data

#impact of imputing missing data on the estimates 
