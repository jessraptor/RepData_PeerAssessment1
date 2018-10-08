##TRY AGAIN:  get and unzip data 

url = "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(url, destfile = "activity.zip", mode = "wb")
unzip("activity.zip")
activity = read.csv("activity.csv", header=T, colClasses = c("integer", "character", 
                                                             "integer") ,sep = ",")

##Explore data

summary(activity)
dim(activity)
str(activity)

##histogram total steps/day, need to transform data to sum up total cols by date

activity$date <- as.Date(activity$date)
totalStepsbyDate <- aggregate(steps ~ date, activity,  FUN = sum)
colnames(totalStepsbyDate) <- c("days" ,"steps")
str(totalStepsbyDate)

##does it work?
hist(totalStepsbyDate$steps)
##now the real one
hist(totalStepsbyDate$steps, main="Histogram of Total Steps Per Day", xlab = "Daily Steps",
     col="blue")

##mean and median of total steps per day
mean(totalStepsbyDate$steps)
median(totalStepsbyDate$steps)
##mean is 10766.19 and median is 10765

##time series plot average steps taken
stepsAverage <- aggregate(steps ~ interval, activity, FUN = mean, na.rm = TRUE)
colnames(stepsAverage) <- c("interval", "avg")
##scatterplot
plot(stepsAverage, type="l") 

##now we need the maximum
maxAverage<-max(stepsAverage$avg)
##which shows the max value but not the interval in which it resides
stepsAverage[stepsAverage$avg == maxAverage, ]
##this indicates that interval 835 shows the max avg which is approx 206

##we want to find out the total number of missing values
sum(is.na(activity$steps))
##there are 2304 NAs

##how will we fill in NAs?  what is the strategy?
activity2 <- subset(activity, !is.na(activity$steps))
dAta <- activity
imPact <- is.na(dAta$steps)
tAvg <- tapply(activity2$steps, activity2$interval, mean, na.rm = TRUE, simplify = T)
dAta$steps[imPact] <- tAvg[as.character(dAta$interval[imPact])]

totalStepsbyDate2 <- aggregate(steps ~ date, dAta, FUN = sum)

par(mar = c(5,8,3,1))

##and the histogram with imputed values
hist(totalStepsbyDate2$steps, col = "green", xlab = "Daily Steps", main = "Histogram 
     of Total Steps Per Day")
hist(totalStepsbyDate$steps, col = "blue", xlab = "Daily Steps", main = "Histogram of 
     Total Steps Per Day", add=T) 
legend("topleft", c("Imputed", "NA"), fill = c("green","blue"))

##now calculate the mean and median
mean(totalStepsbyDate2$steps)
median(totalStepsbyDate2$steps)
##these values are 10766.19 and 10766.19, respectively.  Only the median differs slightly.

##compute avg steps for weekdays and weekends
dAta$date <- as.Date(dAta$date)
dAta$day <- weekdays(dAta$date)
dAta$weekend <- as.factor(ifelse(dAta$day == "Saturday" | dAta$day == "Sunday", 
                                      "weekend", "weekday"))

##two panel weekend vs weekday plot
library(lattice)
plotdata <- aggregate(steps ~ interval + weekend, dAta, mean)
xyplot(steps ~ interval | factor(weekend), data=plotdata, type="l", aspect = 1/2)










