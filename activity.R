url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
file <- "activity.zip"
csv <- "activity.csv"

if (!file.exists(csv)) {
  download.file(url, file, method = "curl")
  unzip(file)
}

f <- read.csv(csv)
d <- f

#check if this transformation is needed!!!!!!
#http://stackoverflow.com/questions/25272457/convert-an-integer-column-to-time-hhmm
#t <- d$interval
#t2 <- mapply(function(x, y) paste0(rep(x, y), collapse = ""), 0, 4 - nchar(t))
#t <- paste0(t2, t)
#t <- format(strptime(t, format="%H%M"), format = "%H:%M")
#d$time <- t

d$date <- as.character(d$date)
#d$datetime <- paste(d$date, d$time)
#d$datetime <- strptime(d$datetime, "%Y-%m-%d %H:%M")
d$date <- as.Date(d$date, "%Y-%m-%d")

dailytotal <- aggregate(d$steps, by = d[c("date")], sum)
hist(dailytotal$x, breaks = 20, xlab="Steps", main="Total steps taken daily")
mean(dailytotal$x, na.rm = T)
median(dailytotal$x, na.rm = T)

dailymean <- aggregate(d$steps, by = d[c("interval")], mean, na.rm=T)
names(dailymean) <- c("interval", "steps")
plot(dailymean, type="l")
dailymean[dailymean$steps == max(dailymean$steps),]

sum(is.na(d$steps))

d1 <- merge(d, dailymean, by = "interval")
d1$steps.x[is.na(d1$steps.x)] <- d1$steps.y[is.na(d1$steps.x)]

dailytotal1 <- aggregate(d1$steps.x, by = d1[c("date")], sum)
hist(dailytotal1$x, breaks = 20, xlab="Steps", main="Total steps taken daily")
mean(dailytotal1$x)
median(dailytotal1$x)


d1$weekday <- weekdays(d1$date)

weekend <- d1[d1$weekday %in% c("Saturday", "Sunday"),]
weekday <- d1[!(d1$weekday %in% c("Saturday", "Sunday")),]

weekend1 <- aggregate(weekend$steps.x, by = weekend[c("interval")], mean)
weekday1 <- aggregate(weekday$steps.x, by = weekday[c("interval")], mean)

weekend1$day <- "weekend"
weekday1$day <- "weekday"

weekdays <- rbind(weekend1, weekday1)
weekdays$day <- as.factor(weekdays$day)

library(lattice)

xyplot(x ~ interval | factor(day), 
       weekdays,
       type = "l", 
       ylab = "Number of steps",
       layout = c(1,2))