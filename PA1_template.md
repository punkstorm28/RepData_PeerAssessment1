REPRODUCIBLE RESEARCH COURSE PROJECT 1
================================
Loading and preprocessing the data
```{r}
setwd("C:/Users/mahe/Documents/data")
dataset <- read.csv("activity.csv")
head(dataset)
```
What is mean total number of steps taken per day?
```{r}
library(ggplot2)
stepsbyday <- tapply(dataset$steps, dataset$date, sum, na.rm=TRUE)
plot1<- qplot(stepsbyday, xlab='Total steps per day', ylab='Frequency')
print(plot1)
```
============
Mean and median of the total number of steps taken per day
```{r}
stepsByDayMean <- mean(stepsbyday)
stepsByDayMedian <- median(stepsbyday)
stepsByDayMean
stepsByDayMedian
```
What is the average daily activity pattern?
```{r}
averageStepsPerTime <- aggregate(x=list(meanSteps=dataset$steps), by=list(interval=dataset$interval), FUN=mean, na.rm=TRUE)
ggplot(data=averageStepsPerTime, aes(x=interval, y=meanSteps)) +
  geom_line() +
  xlab("5-minute interval") +
  ylab("average number of steps taken") 
```
==========
Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
```{r}
mostSteps <- which.max(averageStepsPerTime$meanSteps)
timeMostSteps <-  gsub("([0-9]{1,2})([0-9]{2})", "\\1:\\2", averageStepsPerTime[mostSteps,'interval'])
timeMostSteps
```
Imputing missing values
Total number of missing values in the dataset
```{r}
MissingValues <- length(which(is.na(dataset$steps)))
MissingValues
```
A new dataset that is equal to the original dataset but with the missing data filled in
```{r}
library(dplyr)
replace_with_mean <- function(num) replace(num, is.na(num), mean(num, na.rm = TRUE))
meanday <- (dataset %>% group_by(interval) %>% mutate(steps = replace_with_mean(steps)))
head(meanday)
sum(is.na(meanday))
new_dataset <- as.data.frame(meanday)
head(new_dataset)
```
Total number of steps taken each day without the missing values
```{r}
new_steps <- aggregate(new_dataset$steps, by = list(new_dataset$date), FUN = sum)
names(new_steps)[names(new_steps) == "x"] <- "Total"
names(new_steps)[names(new_steps) == "Group.1"] <- "Date"
hist2 <- ggplot(data = new_steps, aes(Total)) + 
  geom_histogram(binwidth = 1500, colour = "white") +
  xlab("Total Number of Steps Taken Each Day") +
  ylab("Count") +
  ggtitle("Histogram of the Total Number of Steps Taken Each Day with New Version Dataset")
print(hist2)
library(grid)
library(gridExtra)
grid.arrange(plot1, hist2, ncol = 2)
```
========
Mean and Median of the steps taken
```{r}
mean(new_steps$Total)
median(new_steps$Total)
```
Are there differences in activity patterns between weekdays and weekends?
```{r}
new_dataset$WeekendOrWeekday <- ifelse(weekdays(as.Date(new_dataset$date)) %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"), "Weekday", "Weekend")
head(new_dataset)
new_dataset <- (new_dataset %>% group_by(interval, WeekendOrWeekday) %>% summarise(Mean = mean(steps)))
ggplot(new_dataset, mapping = aes(x = interval, y = Mean)) + geom_line() +
  facet_grid(WeekendOrWeekday ~.) + xlab("Interval") + ylab("Mean of Steps") +
  ggtitle("Comparison of Average Number of Steps in Each Interval")
```

