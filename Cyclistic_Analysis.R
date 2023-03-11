#The director
#of marketing believes the company’s future success depends on maximizing the 
#number of annual memberships. Therefore,your team wants to understand how casual
#riders and annual members use Cyclistic bikes differently

#Install required packages
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("lubridate")
# install.packages("tidyr")

#loading package 
library(tidyverse)  #helps wrangle data
library(lubridate)  #helps wrangle date attributes
library(ggplot2)  #helps visualize data
library (dplyr)  # the be able to use the filter method
# 
# #understand what dictonary you on right now
getwd()
setwd()
# #=====================
# # STEP 1: COLLECT DATA
# #=====================
# # Upload Divvy datasets (csv files) here
Jul_2021 <- read_csv("202107-divvy-tripdata.csv")
Aug_2021 <- read_csv("202108-divvy-tripdata.csv")
Sep_2021 <- read_csv("202109-divvy-tripdata.csv")
Oct_2021 <- read_csv("202110-divvy-tripdata.csv")
Nov_2021 <- read_csv("202111-divvy-tripdata.csv")
Dec_2021 <- read_csv("202112-divvy-tripdata.csv")
Jan_2022 <- read_csv("202201-divvy-tripdata.csv")
Feb_2022 <- read_csv("202202-divvy-tripdata.csv")
Mar_2022 <- read_csv("202203-divvy-tripdata.csv")
Apr_2022 <- read_csv("202204-divvy-tripdata.csv")
May_2022 <- read_csv("202205-divvy-tripdata.csv")
June_2022 <- read_csv("202206-divvy-tripdata.csv")





#====================================================
# STEP 2: WRANGLE DATA AND COMBINE INTO A SINGLE FILE
#check for consistence in the column 
colnames(Jul_2021)
colnames(Aug_2021)
colnames(Sep_2021)
colnames(Oct_2021)
colnames(Nov_2021)
colnames(Dec_2021)
colnames(Jan_2022)
colnames(Feb_2022)
colnames(Mar_2022)
colnames(Apr_2022)
colnames(May_2022)
colnames(June_2022)
          



# Inspect the dataframes and look for in contingencies
# so all the column are of the same data type 
str(Jul_2021)
str(Aug_2021)
str(q3_2019)
str(Sep_2021)
# # Remove "bad" data where rows are NA or duplicate

#==================================
# Enter the data or CSV file you 
# will clean and merge below
#====================================

#create a new list to append our clean data 

df1<- bind_rows(
#enter below
Jul_2021,
Aug_2021,
Sep_2021 ,
Oct_2021 ,
Nov_2021,
Dec_2021,
Jan_2022,
Feb_2022,
Mar_2022,
Apr_2022,
May_2022,
June_2022
)
#change to data frame format
df2<-data.frame(df2)
# Apple na.omit method to remove all NA
df2<-na.omit(df1)

# To reassure that there is no NA value in your data use the 
# is.na and any method.if it return a Boolean TRUE than there are
# still some NA left hence if it returns FALSE, than all NA have been removed
any(is.na(df2))

#Next is not remove duplicate rows
#Apply the unique() or distinct() function for data frame in R
df2<-unique(df2)
head(df2)

#Add columns that list the date, month, day, 
# and year of each ride
#This will allow to do some aggregation of them
df2$started_at <- ymd_hms(df2$started_at)
df2$ended_at <- ymd_hms(df2$ended_at)
df2$duration <- as.numeric(difftime(df2$ended_at, df2$started_at, units="min"))
df2$month <- format(df2$started_at, format="%B")
df2$day_of_week <- format(df2$started_at, format="%A")
df2$hour <- format(df2$started_at, format="%H")

glimpse(df2)


df2 %>%
  filter(duration <= 0) %>%
  count()

df2<- df2 %>%
  filter(duration > 0)
head(df2)

# Compare average,middle minimum, and maximum time 
#members and casual users are using the service 

aggregate(df2$duration ~ df2$member_casual, FUN = mean)
aggregate(df2$duration ~ df2$member_casual, FUN = median)
aggregate(df2$duration ~ df2$member_casual, FUN = max)
aggregate(df2$duration ~ df2$member_casual, FUN = min)
summary(df2$duration)
view(max(duration))

# See the average ride time 
#by each day for members vs casual users
aggregate(df2$duration ~ df2$member_casual + df2$day_of_week, FUN = mean)

#rearrange to display chronically 
df2$day_of_week <- ordered(df2$day_of_week, 
                                    levels=c("Sunday", "Monday", "Tuesday", 
                                             "Wednesday", "Thursday", "Friday",
                                             "Saturday"))

aggregate(df2$duration ~ df2$member_casual + df2$day_of_week, FUN = mean)

#==================================
# Visualization  
# Using GGplot 2
#====================================
#show what days are both rider type use the service more
# or the number of rides per user type 
df2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>%
  group_by(member_casual, weekday) %>%
  summarise(number_of_rides = n()
            ,average_duration = mean(duration)) %>%
  arrange(member_casual, weekday)  %>% #sorts
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge") + 
  scale_fill_manual(values=c('#458B74','8B7355'), 
                    limits = c("member", "casual"))+ 
  guides(fill=guide_legend(title="Customer Type")) + ggtitle(" Number of rides by Usertype") +
  theme(plot.title = element_text(size = 15, face = "bold", hjust = 0.5) )
  

#=============================
#Monthly
#=============================



# Let's create a visualization for average duration
df2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(duration)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge") + scale_fill_manual(values=c('#458B74','8B7355'), 
                                                   limits = c("member", "casual")) +  ggtitle("Average Time spent by User Type") +
  theme(plot.title = element_text(size = 15, face = "bold", hjust = 0.5) )

#export
write.csv(df2,file = "cyclistic_data.csv")