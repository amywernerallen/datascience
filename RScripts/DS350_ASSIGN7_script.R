##--------------------------------------------
##
##  Graph Networks - Assignment 7
##  Class: PCE Data Science Methods Class
##  Amy Werner-Allen
##  February 28th 2017
## 
## Load the jittered head count data and the Las Vegas hourly weather data.
## We will merge the data sets in 'Homework7_Start.RView in a new window'. 
## Once these are merged, create linear model factors for various time/date variables, 
## however you wish. You might also include an autoregressive variable, similar to the 
## Dow Jones example in class. 
## Report the Residual Standard Error and interpret this in a sentence. 
## You might also look at the average error by day, aggregating the headcounts 
## and predictions by day.
##
##--------------------------------------------

setwd("~/Documents/UW/Semester2/Assignments")

##-----Load Libraries-----
library(dplyr)
library(data.table)

##-----Load Data-----
headcount = read.csv('JitteredHeadCount.csv', stringsAsFactors = FALSE)
weather = read.csv('las_vegas_hourly_weather.csv', stringsAsFactors = FALSE)


##-----Format Data----
headcount$DateFormat = as.Date(headcount$DateFormat, format="%m/%d/%Y")
names(weather) = c('time','temp','dew_pt','humidity','pressure',
                   'visibility','wind_dir','wind_speed','gust_speed',
                   'precipitation','events','conditions',
                   'wind_dir_deg','date')
weather$datetime = paste(weather$date,weather$time)
weather$datetime = strptime(weather$datetime, format="%Y-%m-%d %I:%M %p")
weather$Hour = as.numeric(format(round(weather$datetime, units="hours"), format="%H"))

##----Drop Duplicates----
weather = weather[!duplicated(weather[c("date", 'Hour')]),]

##----Merge Data-----
weather$DateFormat = weather$date
weather$date = NULL
weather$DateFormat = as.Date(weather$DateFormat, format="%Y-%m-%d")

headcount = merge(headcount, weather, all.x=TRUE, by=c("DateFormat","Hour"))

##----Imputation for NAs in weather-----
numeric_cols = c(11:15, 17:19, 22)
# Linear Interpolation:
headcount[,numeric_cols] = apply(headcount[,numeric_cols], 2, function(x) approx(x, xout=1:length(x), rule=2)$y)

##---Drop character columns----
headcount$wind_dir = NULL
headcount$time = NULL
headcount$datetime = NULL

##-----Deal with events/conditions----
headcount$events[headcount$events == ""] = "None"
headcount$events[is.na(headcount$events)] = "None"
headcount$conditions[is.na(headcount$conditions)] = "None"

hc_model_full = lm(HeadCount ~ . - DateFormat, data = headcount)
summary(hc_model_full)
# Adjusted r-squared: 0.9163

######################################################
##----Format Data for Time Series Exploration-----
# Create many different types of factors
headcount$day_count = as.numeric(headcount$Date - min(headcount$Date))
headcount$week_count = floor(headcount$day_count/7.0)
headcount$month_count = floor(headcount$day_count/30.5)
headcount$year_count = as.numeric(format(headcount$Date, format="%Y")) - 1990

headcount$month = as.numeric(format(headcount$Date, format="%m"))
headcount$season = floor((headcount$month-1) / 3)
headcount$weekday = as.numeric(format(headcount$Date, format = "%w"))
headcount$week = as.numeric(format(headcount$Date, format = "%W"))

hc_model = lm(HeadCount ~ . - DateFormat, data = headcount)
summary(hc_model)
# Adjusted r-squared: 0.9164

######################################################
### AUTO-REGRESSION

# Adding in whether it rained yesterday
headcount$rain = 0
headcount$rain[headcount$events!="None"] = 1
rain_yesterday = sapply(1:nrow(headcount), function(x){
  if(x <= 1){
    return(headcount$rain[1])
  }else{
    return(headcount$rain[x-1])
  }
})
headcount$rain_yesterday = rain_yesterday

hc_model2 = lm(HeadCount ~ . - DateFormat, data = headcount)
summary(hc_model2)
# No improvement in R-squared
# RSE 3.602

# Adding in whether there was bad weather yesterday
headcount$badweather = 0
headcount$badweather[headcount$conditions!="None"] = 1
weather_yesterday = sapply(1:nrow(headcount), function(x){
  if(x <= 1){
    return(headcount$badweather[1])
  }else{
    return(headcount$badweather[x-1])
  }
})
headcount$weather_yesterday = weather_yesterday

hc_model3 = lm(HeadCount ~ . - DateFormat, data = headcount)
summary(hc_model3)
# No improvement in R-squared

# Rain last week
rain_lweek = sapply(1:nrow(headcount), function(x){
  if(x <= 7){
    return(headcount$rain[1])
  }else{
    return(headcount$rain[x-7])
  }
})
headcount$rain_lweek = rain_lweek

hc_model4 = lm(HeadCount ~ . - DateFormat, data = headcount)
summary(hc_model4)
# This new variable has some significance! 
# Still no improvement in r-squared

# Weather last week
weather_lweek = sapply(1:nrow(headcount), function(x){
  if(x <= 7){
    return(headcount$badweather[1])
  }else{
    return(headcount$badweather[x-7])
  }
})
headcount$weather_lweek = weather_lweek

hc_model5 = lm(HeadCount ~ . - DateFormat, data = headcount)
summary(hc_model5)

# Plot of all data
plot(headcount$DateFormat, headcount$HeadCount, type="l", lwd=2,
     xlab="Time", ylab="Head Counts")
lines(headcount$DateFormat, hc_model5$fitted.values, lwd=2, lty=8, col="blue")

# Zoomed in view: three months
plot(headcount$DateFormat, headcount$HeadCount, type="l", lwd=2, 
     xlab="Time", ylab="Head Count: Vegas",
     xlim=c(as.Date("2011-09-01"), as.Date("2011-12-31")))
lines(headcount$DateFormat, hc_model5$fitted.values, lwd=2, lty=8, col="red")
