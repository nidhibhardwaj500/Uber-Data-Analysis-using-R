# Importing essential packages
install.packages("tidyverse")
install.packages("ggthemes")
install.packages("DT")
library(ggthemes)
library(lubridate)
library(dplyr)
library(tidyr)
library(DT)
library(scales)

# Importing data from csv files with data from April 2014 to September 2014
setwd('C:/Users/Nidhi Bhardwaj/OneDrive/Documents/google data analytics/Uber-dataset')
apr_data<- read_csv("uber-raw-data-apr14.csv")
may_data<- read_csv("uber-raw-data-may14.csv")
jun_data<- read_csv("uber-raw-data-jun14.csv")
jul_data<- read_csv("uber-raw-data-jul14.csv")
aug_data<- read_csv("uber-raw-data-aug14.csv")
sep_data<- read_csv("uber-raw-data-sep14.csv")

# Combining all the data in a single dataframe
data_2014<- rbind(apr_data,may_data,jun_data,jul_data,aug_data,sep_data)

# Formatting date time column and creating factors of time objects- day, month, year
data_2014$"Date/Time" <- as.POSIXct(data_2014$"Date/Time", format="%m/%d/%Y %H:%M:%S")
data_2014$Time<- format(as.POSIXct(data_2014$"Date/Time", format="%m/%d/%Y %H:%M:%S"),format="%H:%M:%S")
data_2014$"Date/Time" <- ymd_hms(data_2014$"Date/Time")
data_2014$day<- factor(day(data_2014$"Date/Time"))
data_2014$month<- factor(month(data_2014$"Date/Time", label = TRUE))
data_2014$year<- factor(year(data_2014$"Date/Time"))
data_2014$dayofweek<- factor(wday(data_2014$"Date/Time"))
data_2014$dayofweek<- factor(wday(data_2014$"Date/Time", label = TRUE))
data_2014$hour<-factor(hms(data_2014$Time))
data_2014$hour<-factor(hour(hms(data_2014$Time)))
data_2014$minute<-factor(minute(hms(data_2014$Time)))
data_2014$second<-factor(second(hms(data_2014$Time)))

#Plotting number of trips by hours in a day - use ggplot function to plot number of trips passengers had made in a day
hour_data<-data_2014 %>% group_by(hour) %>% dplyr::summarize(Total=n())
datatable(hour_data)
ggplot(hour_data,aes(hour,Total)) + geom_bar(stat="identity", fill="steelblue",color="red") + ggtitle("Trips every hour") + theme(legend.position = "none") + scale_y_continuous(labels=comma)
month_hour<-data_2014 %>% group_by(month,hour) %>% dplyr::summarize(Total=n())
ggplot(month_hour, aes(hour, Total,fill=month)) + geom_bar(stat="identity") + ggtitle("Trips by hour and month") + scale_y_continuous(labels=comma)

#Plotting by trips during every day of the month
day_group<- data_2014 %>% group_by(day) %>% dplyr::summarize(Total=n())
datatable(day_group)
ggplot(day_group, aes(day, Total)) + geom_bar(stat="identity", fill="steelblue") + ggtitle("Trips every day") + theme(legend.position="none") + scale_y_continuous(labels=comma)
day_month_group<- data_2014 %>% group_by(month,dayofweek) %>% dplyr::summarize(Total=n())
ggplot(day_month_group, aes(month, Total, fill=dayofweek)) + geom_bar(stat="identity") + ggtitle("Trips by day and month") + scale_y_continuous(labels=comma) + scale_fill_manual(values=c("#CC1011","#665555","#05a399","#cfcaca","#f5e840","#0683c9","#e075b0"))

#Finding Number of trips by bases
View(data_2014)
ggplot(data_2014, aes(Base)) + geom_bar(fill="darkred")+ scale_y_continuous(labels=comma)+ggtitle("Trips by base")
ggplot(data_2014, aes(Base, fill = month)) + 
  +     geom_bar(position = "dodge") +
  +     scale_y_continuous(labels = comma) +
  +     ggtitle("Trips by Bases and Month") +
  +     scale_fill_manual(values = c("#CC1011","#665555","#05a399","#cfcaca","#f5e840","#0683c9","#e075b0"))

#Creating hetamap visualisation of day, hour and month
day_and_hour<- data_2014%>% group_by(day,hour)%>%
  + dplyr::summarize(Total=n())
datatable(day_and_hour)
ggplot(day_and_hour, aes(day, hour, fill=Total)) + geom_tile(color="white") + ggtitle("Heat Map by day and hour")
ggplot(day_month_group, aes(dayofweek, month, fill=Total)) + geom_tile(color="White") + ggtitle("Heat Map by month and day")
month_base<- data_2014%>% group_by(Base,month)%>%  dplyr::summarize(Total=n())
dayofweek_base<- data_2014%>% group_by(Base,dayofweek)%>% dplyr::summarize(Total=n())
ggplot(month_base, aes(Base, month, fill=Total)) + geom_tile(color="white")+ ggtitle("Heat Map by month and bases")
ggplot(dayofweek_base, aes(Base, dayofweek, fill=Total)) + geom_tile(color="white")+ ggtitle("Heat Map by day of week and bases")

