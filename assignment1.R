
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
avgsteps[which.max(avgsteps$steps),]

#missing data
library(lubridate)
summary(activity$steps) #2304 NA's
sum(is.na(activity$steps))

######

#activity$mdy <- mdy(activity$date)
#activity$day <- floor_date(activity$date, "day")
activity$meansteps <- aggregate(activity$steps, by=list(activity$interval), FUN=mean, na.rm=TRUE)
activity2 = transform(activity, newsteps = ifelse(is.na(activity$steps), 
            aggregate(activity$steps, by=list(activity$interval), FUN=mean, na.rm=TRUE),
            activity$steps))
#nope, need to replace with mean per day or interval

activity$newsteps <- ifelse(is.na(activity$steps), mean(activity$steps), activity$steps)
#something like this, replace mean()

activity$newsteps[which(is.na(activity$steps))] <- mean(activity$steps)
  #(activity$steps, by=list(activity$interval), FUN=mean, na.rm=TRUE)


#####

steps.na <- function(step, minute) {
  #str(minute) #logging out
  #str(avgsteps$interval) #logging out
  impute <- NA
  if (!is.na(step))
    impute <- c(step)
    else impute <- avgsteps[avgsteps$interval == minute, "steps"]
  return(impute)
}
activity.imp <- activity
activity.imp$newsteps <- mapply(steps.na, activity.imp$steps, activity.imp$interval)

str(avgsteps[avgsteps$interval == 5, "steps"])


#by weekday - do with imputed dataset
activity.imp$mdy <- mdy(activity.imp$date)
activity.imp$weekday <- as.factor(weekdays(activity.imp$mdy))
summary(activity.imp$weekday) 
#create new factor with weekend or weekday
weekend <- c("Saturday", "Sunday")
activity.imp$wkdf <- factor(activity$weekday %in% weekend, levels=c("TRUE", "FALSE"),
      labels=c("weekend","weekday"))
#table(activity$wkdf, activity$weekday) #checking

act_wkend <- subset(activity, wkdf == "weekend")
act_wkd <- subset(activity, wkdf == "weekday")

                    
#do this for weekend vs. weekday
par(mfrow=c(2,1))
par(mar=c(2,2,2,2))

avgsteps_we <- aggregate(act_wkend$steps, by=list(act_wkend$interval), FUN=mean, na.rm=TRUE)
names(avgsteps_we) <- c("interval", "steps")
plot(avgsteps_we$interval, avgsteps_we$steps, type = "l", xlab="5 minute interval", 
     ylab="average steps")
title("Average Steps by  5 minute interval  - Weekend") 

avgsteps_wd <- aggregate(act_wkd$steps, by=list(act_wkd$interval), FUN=mean, na.rm=TRUE)
names(avgsteps_wd) <- c("interval", "steps")
plot(avgsteps_wd$interval, avgsteps_wd$steps, type = "l", xlab="5 minute interval", 
     ylab="average steps")
title("Average Steps by 5 minute interval - Weekday") 


dev.off()


#had to do this to resubmit
knit2html("PA1_template.Rmd", force_v1 = TRUE)



