## 
setwd("~/RepData_PeerAssessment1")
library(stringr)
library(httr)
library(lubridate)
library(rJava)
library(xlsx)
library(XML)
library(jsonlite)
library(RMySQL)
library(plyr)
library(dplyr)
library(tidyr)
library(lattice)
library(ggplot2)
library(reshape2)
library(jpeg)

## limpa work area
rm(list=ls())

if(!file.exists('activity.csv')) {
        work <- tempfile()
        download.file("http://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip",work)
        unzip(work)
        unlink(work)
}


## get data
act  <- read.csv('activity.csv')

## examine data
summary(act)
nrow(act)
head(act)
tail(act)
str(act)

## coerse to data format
act$date <- as.Date(as.character(act$date, format ="%Y/%m/%d"))

## extract NA´s steps
actc <- subset(act, !is.na(steps))

## 1
steps_date <- aggregate(steps ~ date, actc, sum)
hist(steps_date$steps, 
     main = paste("Total Steps / Day"), 
     col=3, xlab="# Steps")
## 2
rmean <- mean(steps_date$steps)
rmedian <- median(steps_date$steps)

## 3
steps_inter <- aggregate(steps ~ interval, actc, mean)

plot(steps_inter$interval,
     steps_inter$steps, type="l", 
     xlab="Interval", ylab="# Steps",
     main="Average Steps /  Day / Interval")

## 4
inter_max <- steps_inter[which.max(steps_inter$steps),1]

## 5 - uses the original file with NAs
no_complete   <- sum(!complete.cases(act))
complete_data <- transform(act, steps = ifelse(is.na(act$steps), 
                steps_inter$steps[match(act$interval, 
                steps_inter$interval)], act$steps))


complete_data[as.character(complete_data$date) == "2012-10-01", 1] <- 0


## 6
steps_date_i <- aggregate(steps ~ date, complete_data, sum)
par(mfrow = c(1, 1))
hist(steps_date_i$steps, 
     main = paste("Steps / Day"), 
     col=1, xlab="# Steps")


#Create Histogram to show difference. 
hist(steps_date_i$steps, 
     main = paste(" Steps / Day"), 
     col=2, xlab="# Steps", add=T)
legend("topright", c("Complete", "No_Complete"), col=c(3, 4), lwd=10)



## Calculate new mean and median for imputed data.

rmean.i <- mean(steps_date_i$steps)
rmedian.i <- median(steps_date_i$steps)

## Calculate difference between imputed and non-imputed data.

mean_diff <- rmean.i - rmean
med_diff <- rmedian.i - rmedian


## Calculate total difference.

total_diff <- sum(steps_date_i$steps) - sum(steps_date$steps)




##
# Create a factor variable with two levels (weekday, weekend-day)
workLT <- as.POSIXlt(act$date, format = "%Y-%m-%d")
workWD <- workLT$wday
workWD[workWD == 0] = 0
workWD[workWD == 6] = 0
workWD[workWD != 0] = 1
workWDFactor <- factor(workWD, levels = c(0, 1))

# Add the factor variable to the data
act$WD <- workWDFactor

# Calculate the mean

stepsWD <- tapply(act$steps, list(act$interval, act$WD), mean, 
                              na.rm = T)

par(mfrow = c(2, 1))
# Display the 2 plots
with(act, {
        par(mai = c(0, 1, 1, 0))
        plot(stepsWD[, 1], type = "l", 
             main = ("Steps X Interval"), 
             xaxt = "n", ylab = "Week ends")
        title = ("# of Steps v.s. Interval")
        par(mai = c(1, 1, 0, 0))
        plot(stepsWD[, 2], type = "l", 
             xlab = "Interval", 
             ylab = "Week days")})



---
title: "markdownDemo"
author: "Carlosjunior4763"
date: "Saturday, September 19, 2015"
output: html_document
---

This is an R Markdown document. Markdown is a simple formatting syntax for authoring HTML, PDF, and MS Word documents. For more details on using R Markdown see <http://rmarkdown.rstudio.com>.

When you click the **Knit** button a document will be generated that includes both content as well as the output of any embedded R code chunks within the document. You can embed an R code chunk like this:

##```{r}
cars <- c(1,3,6,2,8,9,10)
...
summary(cars)
```

You can also embed plots, for example:

##```{r, echo=FALSE}
plot(cars)
```

Note that the `echo = FALSE` parameter was added to the code chunk to prevent printing of the R code that generated the plot.
