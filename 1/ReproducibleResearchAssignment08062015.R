library(dplyr)
activity <- read.csv("activity.csv")
dim(activity)
head(activity)
tail(activity)
daily <- group_by(activity, date)
dim(daily)
head(daily)
dailySteps <- summarize (daily, steps = sum(steps, na.rm = TRUE))
dim(dailySteps)
head(dailySteps)
hist(dailySteps$steps)
mean(dailySteps$steps)
median(dailySteps$steps)

activity[275:300,]

fiveMin <- group_by(activity, interval)
fiveMinSteps <- summarize (fiveMin, steps = mean(steps, na.rm=TRUE))
fiveMinSteps$intervalID <- seq(1:288)
dim(fiveMinSteps)
head(fiveMinSteps)
with(fiveMinSteps, plot(steps~intervalID, type = "l"))
max(fiveMinSteps$steps)
maxAvg <- filter(fiveMinSteps, fiveMinSteps$steps == max(fiveMinSteps$steps))
maxAvg

summary(activity)


with(activity, nrow)
case.complete(activity)
complete.cases(activity)
table(complete.cases(activity))
dim(table(complete.cases(activity)))
table(complete.cases(activity))[[1]]


activityNoNA <- activity
filler <- mean(fiveMinSteps$steps)
activityNoNA$steps[is.na(activityNoNA$steps)] <- filler
head(activityNoNA)
dailyNoNA <- group_by(activityNoNA, date)
dailyStepsNoNA <- summarize (dailyNoNA, steps = sum(steps, na.rm = TRUE))
hist(dailyStepsNoNA$steps)
mean(dailyStepsNoNA$steps)
median(dailyStepsNoNA$steps)


activityNA <- activity[complete.cases(activity)]
activityNoNA <- activity[!is.na(activity)]
dim(activityNA)
head(activityNA)
head(activity)


head(fiveMinSteps)
dim(fiveMinSteps)
meanSteps <- rep(fiveMinSteps$steps, 61)
length(meanSteps)
nrow(activity)
activity2 <- cbind(activity, meanSteps)
dim(activity)
dim(activity2)
head(activity2)

activity3 <- subset(activity2, is.na(activity2$steps))
dim(activity3)
activity4 <- subset(activity2, !is.na(activity2$steps))
dim(activity4)
activity3$steps <- activity3$meanSteps
head(activity3)
activity5 <- rbind(activity3, activity4)
dim(activity5)
activity5[order(activity5$date),]

head(activity)
head(activity5)
tail(activity)
tail(activity5)


activity6 <- group_by(activity5, date)
activity7 <- summarize (activity6, steps = sum(steps, na.rm = TRUE))
hist(activity7$steps, xlab="Number of steps", main = "Histogram of steps/day with imputed values")
mean(activity7$steps)
median(activity7$steps)


weekDay <- weekdays(as.Date(as.character(activity5$date)))
table(weekDay)
weekDay

activity8 <- cbind(activity5, weekDay)
dim(activity8)
head(activity8)
summary(activity8)
activity9 <- subset(activity8, activity8$weekDay %in% c("Saturday", "Sunday"))
activity9
summary(activity9)

activity10 <- group_by(activity9, interval)
activity11 <- summarize (activity10, steps = mean(steps))
activity11$intervalID <- seq(1:288)
activity11$Day <- as.factor(rep("Weekend", 288))
activity11
summary(activity11)
dim(activity11)

with(activity11, plot(steps~intervalID, type = "l"))

activity12 <- activity8[!(activity8$weekDay %in% c("Saturday", "Sunday")),]
activity12
summary(activity12)

activity13 <- group_by(activity12, interval)
activity14 <- summarize (activity13, steps = mean(steps))
activity14$intervalID <- seq(1:288)
activity14$Day <- as.factor(rep("Weekday", 288))
summary(activity14)
dim(activity14)

with(activity14, plot(steps~intervalID, type = "l"))

activity15 <- rbind(activity11, activity14)
summary(activity15)
dim(activity15)
library(lattice)
xyplot(steps~intervalID | Day, data = activity15, layout =c(1,2), type = "l", ylab = "Number of steps", xlab = "Interval")

________________________________________________________________________________________________
#Setup
library(dplyr)
activity <- read.csv("activity.csv")

#Q1
#grouping activity by date and then finding the sum of number of steps/ day
daily <- group_by(activity, date)
dailySteps <- summarize (daily, steps = sum(steps, na.rm = TRUE))
#plotting and responding to the specific questions asked (histogram, mean and median)
hist(dailySteps$steps, xlab="Number of steps", main = "Histogram of steps/day (with NA)")
mean(dailySteps$steps)
median(dailySteps$steps)

#Q2
#grouping activity by the 5 minute intervals each day is split into
fiveMin <- group_by(activity, interval)
#finding the average of number of steps/ interval (data within each interval is from 
#the same time slot across multiple days)
fiveMinSteps <- summarize (fiveMin, steps = mean(steps, na.rm=TRUE))
#adding an ID to each interval. 288 intervals of 5 minutes each = 1,440 minutes = 24 hours/ day
fiveMinSteps$intervalID <- seq(1:288)
#plotting average number of steps against interval ID
with(fiveMinSteps, plot(steps~intervalID, type = "l", ylab = "Number of steps", xlab = "Interval"))
#identifying the time slot which has the max average number of steps
# Please note that "column interval" shows info in HHMM form while "interval ID" shows a running
# sequence of the 5 minute time slots starting at midnight
maxAvg <- filter(fiveMinSteps, fiveMinSteps$steps == max(fiveMinSteps$steps))
maxAvg

#Q3
#Step 1: Create a vector of mean steps/ interval and repeat it for each of the 61 days
#for which data has been captured.
#Step 2: Bind the new vector to the original activity data frame
#Step 3: Subset two separate data frames with and without NA
#Step 4: Impute interval mean values to NA
#Step 5: Merge the data frame originally with out NA and the one in which values are imputed
#Step 6: Repeat steps from Q1 - group by date, sum steps by date, plot histogram and find mean
#& median
#Step1
meanSteps <- rep(fiveMinSteps$steps, 61)
#Step2
activity2 <- cbind(activity, meanSteps)
#Step3
activity3 <- subset(activity2, is.na(activity2$steps))
activity4 <- subset(activity2, !is.na(activity2$steps))
#Step4
activity3$steps <- activity3$meanSteps
#Step5
activity5 <- rbind(activity3, activity4)
#Step6
activity6 <- group_by(activity5, date)
activity7 <- summarize (activity6, steps = sum(steps))
hist(activity7$steps, xlab="Number of steps", main = "Histogram of steps/day with imputed values")
mean(activity7$steps)
median(activity7$steps)

#Q4
#Step 1: Determine the day of the week and add this information as a new column
weekDay <- weekdays(as.Date(as.character(activity5$date)))
activity8 <- cbind(activity5, weekDay)
#Step 2: Subset weekend data, group by interval, calculate average steps/ interval across 
#weekend days
activity9 <- subset(activity8, activity8$weekDay %in% c("Saturday", "Sunday"))
activity10 <- group_by(activity9, interval)
activity11 <- summarize (activity10, steps = mean(steps))
#Step 3: Add interval ID and a weekend flag as new columns
activity11$intervalID <- seq(1:288)
activity11$Day <- as.factor(rep("Weekend", 288))
#Step 4: Subset weekday data, group by interval, calculate average steps/ interval across 
#weekdays
activity12 <- activity8[!(activity8$weekDay %in% c("Saturday", "Sunday")),]
activity13 <- group_by(activity12, interval)
activity14 <- summarize (activity13, steps = mean(steps))
#Step 5: Add interval ID and a weekday flag as new columns
activity14$intervalID <- seq(1:288)
activity14$Day <- as.factor(rep("Weekday", 288))
#Step 6: Combine the weekday and weekend data and plot it using the lattice plotting system
activity15 <- rbind(activity11, activity14)
library(lattice)
xyplot(steps~intervalID | Day, data = activity15, layout =c(1,2), type = "l", ylab = "Number of steps", xlab = "Interval")