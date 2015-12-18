

setwd("/home/jorgen/Documents/coursera/ReproducibleResearch/RepData_PeerAssessment1")
#setwd("F:\\repdata")
#setwd("/media/jorgen/UUI/repdata")
list.files()

#download data
url<-"https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"

if (!file.exists("activity.zip")) {
  download.file(url,destfile = "activity.zip")
}
list.files()

#read data
activity<-read.csv(unz("activity.zip", "activity.csv"))
head(activity)
str(activity)

#########################
#Process data

#format date
library(lubridate)
activity$date<-ymd(activity$date)

#######################################

#What is mean total number of steps taken per day?

#Calculate the total number of steps taken per day
library(dplyr)
totalsteps<-activity%>%
    group_by(date)%>%
    summarise(total = sum(steps))
head(totalsteps)    


#histogram of total number of steps per day
library(ggplot2)
qplot(total,data=totalsteps,
      geom="histogram",
      binwidth=2000,
      xlab="Total number of steps taken each day",
      main = "Histogram of total number of steps taken each day",
      fill =I("antiquewhite4"))

#mean and median of total number of steps taken per day
avgtotal<-round(mean(totalsteps$total,na.rm=T),2)
avgtotal
medtotal<-median(totalsteps$total,na.rm=T)
medtotal


#############################################################
#Daily activity pattern
# 
# Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average 
# number of steps taken, averaged across all days (y-axis)
# 
# Which 5-minute interval, on average across all the days in the dataset, 
# contains the maximum number of steps?

#first calculate average steps per 5min interval
actpattern<-activity%>%
    group_by(interval)%>%
    summarise(avg_steps_interval=mean(steps,na.rm=T),
              med_steps_interval=median(steps,na.rm=T))

with(actpattern,plot(interval,avg_steps_interval,type="l",col="blue"))

qplot(interval,avg_steps_interval,data=actpattern,
      geom="line")
     # colour="yellow")
library(scales) #to allowscaling axes
p<-ggplot(aes(interval,avg_steps_interval), data=actpattern) + 
  geom_line(color="blue", size=1) +
  theme_bw() +
  scale_x_continuous(breaks=pretty_breaks(n=20)) +
  xlab("five minute interval") +
  ylab("Average number of steps across all days") +
  ggtitle("Activity pattern showing average number of steps per five minute interval")
p


max_avg_steps_per_int<-max(actpattern$avg_steps_interval)
max_avg_steps_per_int

#five min interval with average maximum number of steps
max_int<-actpattern[actpattern$avg_steps_interval==max_avg_steps_per_int,"interval"] #835
as.numeric(max_int)
p<-p+geom_vline(xintercept=as.numeric(max_int),colour="red")
p + annotate("text",x=1150,y=200,label="maximum average number of steps", color="red")

###################################################################################################

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


#total number of missing values in the dataset (i.e. the total number of rows with NAs)

#function returns the number of NA's
countnull<-function(s){
    y<-sapply(s,is.na)
    sum(y)
}

countnull(activity$steps)  #2304
countnull(activity$date)  #0
countnull(activity$interval)  #0

#impute missing values
a_i<-activity

#merge a_i and actpattern
a_i<-left_join(a_i,actpattern,by="interval")
head(a_i)

#add average for interval where steps is NA
a_i<-mutate(a_i,
            steps_i=ifelse(is.na(steps),avg_steps_interval,steps))

countnull(a_i$steps_i) #no  nulls

#Create a new dataset that is equal to the original dataset but with the missing data filled in.
a_i<-select(a_i,steps_i,date,interval)
head(a_i)

#histogram of the total number of steps taken each day with imputed data

totalsteps.i<-a_i%>%
  group_by(date)%>%
  summarise(total = sum(steps_i))
head(totalsteps.i)    
head(totalsteps)

#histogram of total number of steps per day imputed
qplot(total,data=totalsteps.i,
      geom="histogram",
      binwidth=2000,
      xlab="Total number of steps taken each day",
      main = "Histogram of total number of steps taken each day",
      fill =I("antiquewhite4"))


# mean and median total number of steps taken per day
avgtotal.i<-round(mean(totalsteps.i$total,na.rm=T),2)
avgtotal.i
medtotal.i<-median(totalsteps.i$total,na.rm=T)
medtotal.i


#comparson of imputed vs original data
library(Hmisc)
describe(a_i$steps_i)  
describe(activity$steps)
#impact of imputing missing data on the estimates 

##################################################################################

#Are there differences in activity patterns between weekdays and weekends?
library(lubridate)
a_i<-mutate(a_i,
            wkday = ifelse(wday(date) %in% 2:6,"weekday","weekend"))
head(a_i,20)

actpattern.i<-a_i%>%
    group_by(interval,wkday)%>%
    summarise(avg_steps_interval=mean(steps_i,na.rm=T),
              med_steps_interval=median(steps_i,na.rm=T))
head(actpattern.i)
head(actpattern)

describe(actpattern)
describe(actpattern.i)

p.i<-ggplot(aes(interval,avg_steps_interval), data=actpattern.i) + 
    geom_line(color="blue", size=1) +
    theme_bw() +
    facet_grid(wkday~.) +
    scale_x_continuous(breaks=pretty_breaks(n=20)) +
    xlab("five minute interval") +
    ylab("Average number of steps across all days") +
    ggtitle("Activity pattern showing average number of steps per five minute interval (imputed) \n over weekdays or weekends")
p.i
names(actpattern.i)
