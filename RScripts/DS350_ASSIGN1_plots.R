
##---------------------------
## Methods for Data Analysis
## Assigment 1 - Jan 11 2017
## Amy Werner-Allen
##
##---------------------------

library(lubridate)
library(zoo)
library(dplyr)

##---------------------------
#### Reading in data

setwd("~/Documents/UW/Semester2/Assignments")
data = read.csv("JitteredHeadCount.csv", header=TRUE, stringsAsFactors = FALSE)

# Convert dates to date format
data$date = as.Date(data$DateFormat, format="%m/%d/%Y")

# The following variables (with descriptions) exist in the data:
## GameCode: 2 digit game code.
## DateFormat: Date
## Hour: 0-23 of the day
## TabelsOcc: Number of Tables Occupied in the casino
## TablesOpen: Number of tables open in the casino (this number can be different than occupied because the pit boss can open a table and no one can be sitting at it)
## Tables Closed: Number of tables closed in the casino.
## HeadCount: Total Count of people at the hour, day, and table type
## DayOfWeek: 1-7.  1 = Monday, 7 = Sunday.
## DayNumber: Day of year (ot really needed, but note that the dates start in September)

##---------------------------
#### Game Code

# There are 22 different game codes; all except two have 8,788 counts. 
# Two game codes -- BA and MS -- have less than 200 counts:
table(data$GameCode)

# Checking out open tables only, then looking at weekend days
open = data[data$TablesOpen==1,]
openweek = aggregate(open$HeadCount, by=list(Category=open$weekdayf,open$GameCode), FUN=mean)
weekend = subset(openweek, Category %in% c('Fri','Sat', 'Sun'))
colnames(weekend) = c("Day", "Game", "Avg")

# Taking the top 15 to graph
head(arrange(weekend, desc(weekend$Avg)),10)
top = head(arrange(weekend, desc(weekend$Avg)),15)

ggplot(top, aes(x=Day, y=Avg, fill=Game)) +
  geom_bar(stat="identity") +
  xlab("Counts by Classification") +
  theme_bw() +
  scale_fill_brewer(palette="RdBu") 

##---------------------------
#### Dates

# To start off, a few data-manipulation tasks
# Making some date parts and factors
data$month = as.numeric(as.POSIXlt(data$date)$mon+1)
data$monthf = factor(data$month,levels=as.character(1:12),labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"),ordered=TRUE)
data$weekday = as.numeric(format(as.POSIXlt(data$date),"%u"))
data$weekdayf = factor(data$weekday,levels=rev(1:7),labels=rev(c("Mon","Tue","Wed","Thu","Fri","Sat","Sun")),ordered=TRUE)
data$monthdayf = as.factor(paste(data$monthf, as.numeric(format(as.POSIXlt(data$date),"%d"))))

# Looking at average head counts by month and day
# Interested to see if any holidays stick out
t = aggregate(data$HeadCount, by=list(Category=data$monthdayf), FUN=mean)
t = as.data.frame(t)
t$Month = substring(t$Category,1,3)
colnames(t) = c("Date", "Avg", "Month")
head(arrange(t, desc(t$Avg)),10)

# Distribution: check for normality
ggplot(t, aes(x=Avg, fill=Month)) +
  geom_histogram() +
  theme(panel.grid.minor = element_line(colour = "red", linetype = "dotted"), panel.background = element_rect(colour = "darkblue"))

### Is March special?
# It seems like there are a few outliers 
# Do a quick t-test to find out:

marchcounts = data[data$monthf=="Mar",]
othercounts = data[data$monthf!="Mar",]
t.test(marchcounts$HeadCount,othercounts$HeadCount)

##---------------------------
#### Hour

# Now let's look at the mean by hour
t = aggregate(data$HeadCount, by=list(Category=data$Hour), FUN=mean)
t = as.data.frame(t)
colnames(t) = c("Hour", "Avg")

# Create some factors for time of day
t$timeofday = "Morning"
t$timeofday[t$Hour>=12]<-"Afternoon"
t$timeofday[t$Hour>=18]<-"Evening"
t$timeofday[t$Hour>=23]<-"LateNight"
t$timeofday[t$Hour<=4]<-"LateNight"
t$Hour = as.factor(t$Hour)

# Rose plot
hours = ggplot(t, aes(x=Hour, y=Avg, fill=timeofday))+
  geom_bar(stat="identity",width=1,colour="black", size=0.1)+
  coord_polar(start=-0.15) +
  ylab("")+
  scale_x_discrete(breaks=seq(0,23,1))+
  scale_fill_brewer(palette="RdBu")+
  guides(fill=FALSE)+
  theme(panel.grid.minor = element_blank())
hours


