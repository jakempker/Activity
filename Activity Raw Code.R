setwd("C:/Users/jkempke/Box Sync/Coursera/Activity")

#Download, unzip and read in raw data if it does not already exist in working directory
if (!file.exists("./activity.csv")){
    download.file(url = "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip", destfile = "./repdata_data_activity.zip", method = "auto")
    unzip("./repdata_data_activity.zip")  
}

act <- read.csv("./activity.csv", stringsAsFactors = FALSE)
#Need to do some work to put a Date-Time variable together
act$interval_char <- as.character(act$interval)

library(stringr) #this has str_pad() function since I need leading zeros for 4-character-length military time

act$interval_char <- str_pad(act$interval_char, 4, side = c("left"), pad ="0")
act$time_char <- paste(act$date, act$interval_char)
act$time <- as.POSIXct(strptime(act$time_char, format = "%Y-%m-%d %H%M"))

#To impute missing steps I want to take average of steps at same time of day over all other days with data
#so I factor the time interval so I can tapply() a mean() function over this factor
act$interval_char <- as.factor(act$interval_char)
means <- as.numeric(with(act, tapply(steps, INDEX=interval_char, mean, na.rm=TRUE)))

#few data steps to merge these means back to full set, matching by time interval factor level
interval_char<- levels(act$interval_char)
n <- data.frame(interval_char,means)
act <- merge(act, n, by = "interval_char")

library(dplyr)
act <- arrange(act, time) #Sorting is not necessary but helps me visualize
#dplyr:mutate() to create steps_imputed with ifelse statement
act <- mutate(act, steps_imputed = ifelse(is.na(steps), means, steps))

#now to create factor variable for weekend vs weekdays
act <- mutate(act, day = weekdays(time))
#the brackets in multiple ifelse statements with mutate are tricky, pay attention
act <- mutate(act, day_cat = ifelse(day=="Sunday", "weekend",
                                    ifelse(day == "Saturday", "weekend","weekday")))
act$day_cat <- as.factor(act$day_cat)
with(act, table(day, day_cat)) #verify this mutate worked

activity <- select(act, date, interval, time, steps, steps_imputed, day_cat) #Create a nice tiday dataset
remove(act, n) #clean up the workspace

#First question, mean steps per day.
#Sum up steps using tapply() or  aggrefate() over date coerced to factor, then take mean of this vector
#View(with(act, tapply(steps, date, sum, ra.nm = TRUE)))
steps_per_day <- aggregate(steps ~ date, data = activity, FUN = sum, na.action = na.pass)
View(steps_per_day)
library(ggplot2)
g <- ggplot(steps_per_day, aes(steps))+
    geom_histogram()
print(g)

mean(steps_per_day$steps, na.rm = TRUE)
median(steps_per_day$steps, na.rm = TRUE)

#Time series plot of average steps time interval across all days
avg_steps_int <- aggregate(steps ~ interval, data = activity, FUN = mean)
ggplot(avg_steps_int, aes(interval, steps))+
    geom_path()+
    labs(x = "5-minute time interval", y = "Average steps taken")

#Which 5-minute interval on average has mmost steps?
with(avg_steps_int, max(steps), na.rm = TRUE)
filter(avg_steps_int, steps == max(steps))

#Number of values for steps
table(is.na(activity$steps))

ggplot(activity, aes(steps_imputed))+
    geom_histogram()

#mean and median steps with imputation
mean(activity$steps_imputed)
median(activity$steps_imputed)

#looking at average steps per interval by weekday vs weekend
avg_steps_weekend <- aggregate(steps ~ day_cat + interval, data = activity, FUN = mean)

ggplot(avg_steps_weekend, aes(interval, steps))+
    geom_path()+
    facet_grid(. ~ day_cat)

ggplot(avg_steps_weekend, aes(interval, steps))+
    geom_path()+
    facet_grid( day_cat ~ .)

library(knitr)

knit
library(rmarkdown)
render("PA1_template.Rmd", "html_document")
