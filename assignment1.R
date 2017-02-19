
#Reproducable Research

#Assignment 1

#still to do: fill in mean for NA, get new hist mean median
#write up report in Rmd/knitr

getwd()
setwd("C:/Users/mmeacham/Documents/Coursera/reproducible")
activity <- read.csv("activity.csv")

#Calculate total steps per day and mean steps per day
total <- aggregate (activity$steps, by=list(activity$date), FUN=sum, na.rm=TRUE)
names(total) <- c("date", "steps")
summary(total$steps)
hist(total$steps, breaks = 10, col="grey", xlab = "steps per day", 
     title("Histogram of Total Steps per Day")) #fix title
mean(total$steps)
median(total$steps)

#Average daily pattern
avgsteps <- aggregate(activity$steps, by=list(activity$interval), FUN=mean, na.rm=TRUE)
names(avgsteps) <- c("interval", "steps")
#plot 
plot(avgsteps$interval, avgsteps$steps, type = "l", xlab="5 minute interval", 
     ylab="average steps")
title("Average Steps by 5 minute interval") #most steps ~800-1000 minutes

#missing data
library(lubridate)
summary(activity$steps) #2304 NA's
#activity$newsteps <- aggregate(activity$steps, by=list(activity$), FUN=mean, na.rm=TRUE)
#activity$mdy <- mdy(activity$date)
#activity$day <- floor_date(activity$date, "day")

activity$newsteps <- ifelse(is.na(activity$steps), mean(activity$steps), activity$steps)
#something like this, replace mean()

#by weekday - do with imputed dataset
activity$weekday <- as.factor(weekdays(activity$mdy))
summary(activity$weekday) 
#create new factor with weekend or weekday
weekend <- c("Saturday", "Sunday")
activity$wkdf <- factor(activity$weekday %in% weekend, levels=c("TRUE", "FALSE"),
      labels=c("weekend","weekday"))
#table(activity$wkdf, activity$weekday) #checking

act_wkend <- subset(activity, wkdf == "weekend")
act_wkd <- subset(activity, wkdf == "weekday")

                    
#do this for weekend vs. weekday
avgsteps_we <- aggregate(act_wkend$steps, by=list(act_wkend$interval), FUN=mean, na.rm=TRUE)
names(avgsteps_we) <- c("interval", "steps")
plot(avgsteps_we$interval, avgsteps_we$steps, type = "l", xlab="5 minute interval", 
     ylab="average steps")
title("Average Steps by 
      5 minute interval 
      - Weekend") 

avgsteps_wd <- aggregate(act_wkd$steps, by=list(act_wkd$interval), FUN=mean, na.rm=TRUE)
names(avgsteps_wd) <- c("interval", "steps")
plot(avgsteps_wd$interval, avgsteps_wd$steps, type = "l", xlab="5 minute interval", 
     ylab="average steps")
title("Average Steps by 
      5 minute interval 
      - Weekday") 

par(mfrow=c(1,2))
dev.off()





