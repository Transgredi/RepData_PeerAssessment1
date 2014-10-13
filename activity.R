url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
file <- "activity.zip"
csv <- "activity.csv"

if (!file.exists(csv)) {
  download.file(url, file, method = "curl")
  unzip(file)
}

f <- read.csv(csv)

d <- f

#http://stackoverflow.com/questions/25272457/convert-an-integer-column-to-time-hhmm
t <- d$interval
t2 <- mapply(function(x, y) paste0(rep(x, y), collapse = ""), 0, 4 - nchar(t))
t <- paste0(t2, t)
t <- format(strptime(t, format="%H%M"), format = "%H:%M")

d$time <- t

d$date <- as.character(d$date)

d$datetime <- paste(d$date, d$time)
d$datetime <- strptime(d$datetime, "%Y-%m-%d %H:%M")
d$date <- as.Date(d$date, "%Y-%m-%d")

plot(d$datetime, d$steps, type = "l")

d1 <- data.frame()
d1 <- d[,c(5,1)]