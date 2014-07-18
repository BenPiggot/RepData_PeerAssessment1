setwd("~/Desktop/RepData_PeerAssessment1")
activity <- read.csv("activity.csv")


activity$steps <- as.numeric(activity$steps)
activity$interval <- as.numeric(activity$interval)
activity$interval <- unique(activity$interval)
X <-aggregate(activity$steps ~ activity$date, data=activity, sum, na.rm=TRUE)
hist(X[,2], main="Step Totals per Day",xlab="Number of Steps", breaks=10)
meanSteps <- mean(X[,2])
medianSteps <- median(X[,2])

str(Y[,2])
str(activity$interval)
Y <-aggregate(activity$steps ~ activity$interval, data=activity, mean, na.rm=TRUE)

plot(Y[,2] ~ unique(activity$interval), type="l", ylab="Number of Steps", xlab="Time of Day (in Five Minute Intervals)", main="Average Number of Steps at Different Times of Day")

Z <- which.max(Y[,2])
MaxSteps <- Y[Z,1]

sum(is.na(activity$steps)) + sum(is.na(activity$interval)) + sum(is.na(activity$date))

activity$StepMeans <- Y[,2]

for(i in 1:nrow(activity)){
  if (is.na(activity[i,1])) {
    activity[i,1] <- activity[i,4]
  }
}

A <- aggregate(activity[,1] ~ activity[,2], data=activity, sum, na.rm=TRUE)

hist(A[,2], main="Step Totals per Day", xlab="Number of Steps", breaks=10)

mean(A[,2])
median(A[,2])

activity$days <- weekdays(as.Date(activity$date))

activity$DayType <- ifelse(activity$days == "Monday" |  activity$days == "Tuesday" | activity$days == "Wednesday" |
                    activity$days == "Thursday" | activity$days == "Friday", c("Weekday"), c("Weekend"))

activity$DayType <- as.factor(activity$DayType)

library(lattice)

xyplot(activity$steps ~ activity$interval | activity$DayType, type="l", vertical=TRUE,
       ylab="Number of Steps", xlab="Time of Day (in Five Minute Intervals)", layout=c(1,2))

install.packages("knitr")
library(knitr)

