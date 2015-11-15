## Load libraries
library(ggplot2)
library(dplyr)
library(plyr)
library(sqldf)

## Read in data set
act <- read.csv("./activity.csv", stringsAsFactors = FALSE)
act$date <- as.Date(act$date, "%Y-%m-%d")

## Get a feel for the data set and the elements
dim(act)
str(act)
typeof(act)

## Plot some data to see what it looks like
plot(act$interval, act$steps)
plot(act$date, act$steps)

## Remove missing data to calculate the total steps per day and both the mean and 
## median of this total
cact <- na.omit(act)
dateSum <- (ddply(cact, ~date, summarise, sum(steps)))
dateSumMean <- mean(dateSum[,2])
dateSumMedian <- median(dateSum[,2])

## Whereas a barplot is used in summarizing categorical data with the height of
## the bar representing the measured value of the category, histograms plot 
## quantitative data with ranges of the data grouped into bins or intervals. 
## The area of the histogram bin gives the percentage of the data contained
## in that bin. The sum of the areas of all bins equals 100%. Thus issustrating
## the distribution of the data. Though the width of the bar in a barplot is
## arbritrary, the barplot allows us to compare a measure between the categories.
## Since the total number of steps per day is a continuous variable, we will
## the histogram to represent these data and get a view of the shape of their
## distribution.
g <- ggplot(dateSum, aes(dateSum[,2]), xlab = "", ylab = "")
g + geom_histogram(color = 1, binwidth = 2000, fill = 5) + theme_bw() + 
     geom_vline(xintercept = 10766.19, color = "green") + 
     geom_text(aes(x=10766.19, label="Mean = 10766.19", y=15), colour="black", hjust = -0.1) +
     geom_vline(xintercept = 10765, color = "black") +
     geom_text(aes(x=10765, label="Median = 10765", y=14.5), colour="black", hjust = -0.1) +
     xlab("Steps") +
     ylab("Frequency") +
     ggtitle("Histogram of Total Steps per Day")

## Let's look at the average daily activity pattern. Missing data will be
## excluded. The interval with the maximum number of steps will be identified.
cact <- ddply(cact, .(interval), transform, intAve = mean(steps))
maxInt <- cact[which.max(cact$intAve), ]

g <- ggplot(cact, aes(x = interval, y = intAve), xlab = "", ylab = "")
g + geom_line() + 
     theme_bw() +
     xlab("Time Interval (5 mins)") +
     ylab("Average Number of Steps") +
     ggtitle("Average Number of Steps per 5 Minute Interval (61 days)") +
     scale_x_continuous(limits = c(0, 2355), 
                        breaks = round(seq(min(cact$interval), 
                        max(cact$interval), by = 100),1)) +
     geom_vline(xintercept = maxInt$interval, color = "blue") +
     geom_text(data = cact, aes(x=maxInt$interval, 
                   y=maxInt$steps, 
                   label = paste("<-- Max Average Steps at Interval ",maxInt$interval, sep = "")), 
                   hjust = -0.1, 
                   vjust = -65,
                   colour = "blue",
                   show_guide = FALSE)

## There are days where no steps are reported. Let's determine the total number
## missing observations for each column.
colSums(is.na(act))

## Let's fill in the missing values with the median value for the interval 
## across all days. The median value is the value of the interval below which
## 50% of the days will fall. This makes most sense to me as there are outliers
## in each interval as shown by the boxplot and histogram of interval 825 and
## we are trying to keep it simple. Though there are packages that utilize 
## more sophisticated methods of imputing. Three references are provided.
## https://cran.r-project.org/web/packages/mi/mi.pdf
## http://www.stefvanbuuren.nl/publications/MICE%20in%20R%20-%20Draft.pdf
## http://www.unt.edu/rss/class/Jon/Benchmarks/
## MissingValueImputation_JDS_Nov2010.pdf
boxplot(act$steps ~ act$interval, data = act, pch = 20)
int825 <- act[act$interval == 825, ]
hist(int825$steps)
## Let's impute by first calculating the median for each interval and then 
## replacing any missing step values with the media of its interval.
actImp <- act
actImp$steps[is.na(actImp$steps)] <- median(actImp$steps, na.rm = TRUE)
colSums(is.na(actImp))
plot(actImp$interval, actImp$steps)
plot(actImp$date, actImp$steps)
intImp825 <- actImp[actImp$interval == 825, ]
hist(intImp825$steps)
actImpSum <- ddply(actImp, .(date), summarise, sum(steps))
hist(actImpSum[,2])
hist(dateSum[,2])
mean(actImpSum[,2])
median(actImpSum[,2])

## Imputing missing information based on the interval median has the effect of
## skewing the distribution of the total number of steps per day to the left and
## slightly reducing the median total number of step per day.



g <- ggplot(actImpSum, aes(actImpSum[,2]), xlab = "", ylab = "")
g + geom_histogram(color = 1, binwidth = 2000, fill = 5) + theme_bw() + 
        geom_vline(xintercept = 9354.23, color = "red") + 
        geom_text(aes(x=9354.23, label="Mean = 9354.23 -->", y=15), colour="red", hjust = 1.1) +
        geom_vline(xintercept = 10395, color = "black") +
        geom_text(aes(x=10395, label="<-- Median = 10395", y=14.5), colour="black", hjust = -0.1) +
        xlab("Steps") +
        ylab("Frequency") +
        ggtitle("Histogram of Total Steps per Day with Imputed Missings")


actImp$day <- weekdays(actImp$date)
actImp$weekday[(actImp$day == "Saturday" | actImp$day == "Sunday")] <- "weekend"
actImp$weekday[!(actImp$day == "Saturday" | actImp$day == "Sunday")] <- "weekday"

actImp <- ddply(actImp, .(interval, weekday), transform, intAve = mean(steps))
actImp <- actImp[order(actImp$weekday, -actImp$intAve),]
highs <- actImp[!duplicated(actImp$weekday),]
end <- sqldf("select * from highs where weekday = 'weekend'")
day <- sqldf("select * from highs where weekday = 'weekday'")

g <- ggplot(actImp, aes(x = interval, y = intAve, color = weekday, group = weekday), 
            xlab = "", ylab = "")
g + geom_line(size = 1) + 
        theme_bw() +
        xlab("Time Interval (5 mins)") +
        ylab("Average Number of Steps") +
        ggtitle("Comparing Average Number of Steps per 5 Minute Interval (61 days) for Weekdays and Weekends") +
        scale_x_continuous(limits = c(0, 2355), 
                           breaks = round(seq(min(actImp$interval), 
                                              max(actImp$interval), by = 100),1)) +
        geom_vline(xintercept = end$interval, color = "turquoise", size = 1) +
        geom_text(data = actImp, aes(x=end$interval, 
                                   y=end$intAve, 
                                   label = paste("<-- Weekend Max Average Steps at Interval ", end$interval, sep = "")), 
                  hjust = -0.0, 
                  vjust = -10,
                  colour = "turquoise",
                  show_guide = FALSE) +
        geom_vline(xintercept = day$interval, color = "tomato", size = 1) +
        geom_text(data = actImp, 
                  aes(x=day$interval, y=day$intAve, 
                      label = paste("<-- Weekday Max Average Steps at Interval ", day$interval, sep = "")), 
                  hjust = 0.0, vjust = -10, colour = "tomato", show_guide = FALSE)
        
        
        
        

