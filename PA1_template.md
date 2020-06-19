---
title: "Reproducible Research - Week 2 - Course Project 1"
author: "Chandar"
date: "June 19, 2020"
output: html_document
---

This is an R Markdown document. Markdown is a simple formatting syntax for authoring HTML, PDF, and MS Word documents. For more details on using R Markdown see <http://rmarkdown.rstudio.com>.

When you click the **Knit** button a document will be generated that includes both content as well as the output of any embedded R code chunks within the document. You can embed an R code chunk like this:

# Load all packages used in this project
```{r, echo=TRUE}
library(knitr)

library(dplyr)
library(ggplot2)
library(magrittr)
library(data.table)
```


#1. Reading in the dataset and/or processing the data
```{r, echo=TRUE}
## Setup working directory
setwd('C:/New laptop/201 Course/301_DataScience_Specialization/Course5/repdata_data_activity')

# Loading data
data <- read.csv('activity.csv')
dim(data)
head(data)
tail(data)
str(data)
summary(data)

## Remove NA in the data
data2 = na.omit(data)
head(data2) 
```


#2. What is mean total number of steps taken per day?
```{r, echo=TRUE}

# For this part of the assignment, you can ignore the missing values in the dataset.
# Calculate the total number of steps taken per day
# If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day
# Calculate and report the mean and median of the total number of steps taken per day

daysteps = aggregate(x = data2$steps,                
                     by = list(data2$date),              
                     FUN = sum) 

dim(daysteps)
str(daysteps)
colnames(daysteps) = c("date", "tot_steps")
head(daysteps)


# Histogram of the total number of steps taken each day

hist(daysteps$tot_steps, xlab = "Total No. of Steps", main="Histogram of Total No. of Steps by day", breaks = 25)
# The shape of this histogram appears to be bell shaped, which indicaste that the total number of steps taken each day is approximately normal. 


# Mean and median number of steps taken each day
mean_steps = mean(daysteps$tot_steps)
mean_steps

median_steps = median(daysteps$tot_steps)
median_steps
```
#### The mean and median number of steps without missing values are 10766.19 and 10765 respectively.      



#3. What is the average daily activity pattern?
```{r, echo=TRUE}
# Make a time series plot (i.e., type = 'l') of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
# Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

# Time series plot of the average number of steps taken

datainterval = aggregate(x = data2$steps,                
                     by = list(data2$interval),              
                     FUN = mean) 

dim(datainterval)
head(datainterval)
colnames(datainterval) = c("interval", "meansteps")
ggplot(datainterval, aes(x=interval, y=meansteps))+ geom_line()


# The 5-minute interval that, on average, contains the maximum number of steps
mx = max(datainterval$meansteps)
mx
maxsteps = datainterval[datainterval$meansteps==mx,]
maxstepinterval = datainterval[datainterval$meansteps==mx,][1]
maxstepinterval
```
#### On average, the maximum number of steps is 206.1698 and the interval that contains the maximum number of steps is 835.


#4. Imputing missing values
```{r, echo=TRUE}
##Note that there are a number of days/intervals where there are missing values (coded as 'NA'). The presence of missing days may introduce bias into some calculations or summaries of the data.

##Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

##Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

##Create a new dataset that is equal to the original dataset but with the missing data filled in.

##Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

missing_vals <- sum(is.na(data))
missing_vals

print("Total number of missing values (i.e., NA's) in the data is 2304.") 


## Missing vlaue imputation
mean_replace <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))
mean_impute_data <- data.frame(data%>% 
                      group_by(interval) %>% 
                      mutate(steps= mean_replace(steps)))
head(mean_impute_data)
names(mean_impute_data)


# Histogram of the total number of steps taken each day after missing values are imputed

daysteps_imputed = aggregate(x = mean_impute_data$steps,                
                     by = list(mean_impute_data$date),              
                     FUN = sum)


dim(daysteps_imputed)
head(daysteps_imputed)
colnames(daysteps_imputed) = c("date", "tot_steps")

summary(daysteps_imputed)

hist(daysteps_imputed$tot_steps, xlab = "Total No. of Steps", main="Total No. of Steps by day", breaks = 25)

mean(daysteps$tot_steps)
mean(daysteps_imputed$tot_steps)

median(daysteps$tot_steps)
median(daysteps_imputed$tot_steps)
```
####The mean of the total number of steps without missing values (i.e., NA's) is 10766.19 and it remains same even after replacing NA's with respective mean of each interval. 

####However, the median number of steps without missing values is 10765, while it has been slightly increased to 10766.19 due to mean imputation. 




#5. Are there differences in activity patterns between weekdays and weekends?
```{r, echo=TRUE}
# For this part the 'weekdays()' function may be of some help here. Use the dataset with the filled-in missing values for this part.

# Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

# Make a panel plot containing a time series plot (i.e. \color{red}{\verb|type = "l"|}type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


# Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends

# Convert the character variable 'date' into Date format
mean_impute_data$date <- as.Date(mean_impute_data$date)

# Creating a new factor variable with 7 levels (from Monday to Sunday) using 'weekdays' function
mean_impute_data$weekday <- weekdays(mean_impute_data$date)

# Creating a new factor variable with two levels - 'Weekday' and 'weekend'
mean_impute_data$weekend <- ifelse(mean_impute_data$weekday=="Saturday" | mean_impute_data$weekday=="Sunday", "Weekend", "Weekday" )

head(mean_impute_data)
tail(mean_impute_data)


# Average number of steps taken, averaged across all weekday days or weekend days (y-axis) versus 5-minute inteval (x-axis)
meansteps_weekday_weekend <- aggregate(mean_impute_data$steps , by= list(mean_impute_data$weekend, mean_impute_data$interval), na.omit(mean))
colnames(meansteps_weekday_weekend) <- c("Weekend", "Interval", "MeanSteps")
head(meansteps_weekday_weekend)

ggplot(meansteps_weekday_weekend, aes(x=Interval, y=MeanSteps, color=Weekend)) + 
            geom_line()+ xlab("Interval") + ylab("Mean Number of Steps") + facet_grid(Weekend ~.) + 
            ggtitle("Comparison of Activity Patterns between Weekdays and Weekends")
```
