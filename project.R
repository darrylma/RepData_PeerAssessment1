library(timeDate)
unzip("activity.zip")
activity_data <- data.table::fread(input = "activity.csv")
activity_data$date <- as.POSIXct(activity_data$date, format = "%Y-%m-%d")

daily_steps <- aggregate(steps ~ date, activity_data, sum)
hist(daily_steps$steps, xlab = "Daily Steps", ylab = "Frequency", breaks = seq(0, 25000, 1000))

mean(daily_steps$steps, rm.na=TRUE)
median(daily_steps$steps, rm.na=TRUE)

interval_steps <- aggregate(steps ~ interval, activity_data, mean)
with(interval_steps, plot(interval, steps, xlab = "Interval", ylab = "Average Steps", type = "l", lwd = 2))

interval_max_steps <- interval_steps$interval[which.max(interval_steps$steps)]

sum(is.na(activity_data$steps))

tidy_activity_data <- merge(activity_data, interval_steps, by = "interval")
tidy_activity_data$steps.x <- as.numeric(tidy_activity_data$steps.x)
tidy_activity_data[is.na(tidy_activity_data$steps.x),"steps.x"] <- tidy_activity_data[is.na(tidy_activity_data$steps.x),"steps.y"]
tidy_activity_data <- tidy_activity_data[,c(1:3)]
names(tidy_activity_data)[2] <- "steps"

tidy_daily_steps <- aggregate(steps ~ date, tidy_activity_data, sum)
hist(tidy_daily_steps$steps, xlab = "Daily Steps", ylab = "Frequency", breaks = seq(0, 25000, 1000))

mean(tidy_daily_steps$steps)
median(tidy_daily_steps$steps)

weekend_weekday_map = data.frame(day.of.week = c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"), day.category = c("weekday","weekday","weekday","weekday","weekday","weekend","weekend")) 
tidy_activity_data$day.of.week <- weekdays(tidy_activity_data$date) 
tidy_activity_data <- merge(tidy_activity_data, weekend_weekday_map, by = "day.of.week")

par(mfrow = c(2, 1))
weekday_interval_steps <- aggregate(steps ~ interval, tidy_activity_data[tidy_activity_data$day.category == "weekday",], mean)
with(weekday_interval_steps, plot(interval, steps, xlab = "Interval", ylab = "Average Steps", main = "Weekday", type = "l", lwd = 2))
weekend_interval_steps <- aggregate(steps ~ interval, tidy_activity_data[tidy_activity_data$day.category == "weekend",], mean)
with(weekend_interval_steps, plot(interval, steps, xlab = "Interval", ylab = "Average Steps", main = "Weekend", type = "l", lwd = 2))
