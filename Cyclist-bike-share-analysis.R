# Prepare Phase of the Data Analysis 
# Install Packages and Load Packages
install.packages("ggmap")
install.packages("geosphere")
install.packages("lubridate")
library(tidyverse)
library(here)
library(skimr)
library(janitor)
library(ggmap)
library(geosphere)
library(lubridate)
library(dplyr)
# import Data into the dataframe
setwd("C:/Users/DELL/Documents/Cyclist-bike-share-analysis/Data")
jan23 <- read_csv("202301-divvy-tripdata.csv")
feb23 <- read_csv("202302-divvy-tripdata.csv")
mar23 <- read_csv("202303-divvy-tripdata.csv")
apr23 <- read_csv("202304-divvy-tripdata.csv")
may23 <- read_csv("202305-divvy-tripdata.csv")
jun23 <- read_csv("202306-divvy-tripdata.csv")
jul23 <- read_csv("202307-divvy-tripdata.csv")
aug23 <- read_csv("202308-divvy-tripdata.csv")
sep23 <- read_csv("202309-divvy-tripdata.csv")
oct23 <- read_csv("202310-divvy-tripdata.csv")
nov23 <- read_csv("202311-divvy-tripdata.csv")
dec23 <- read_csv("202312-divvy-tripdata.csv")
# Exoploring Charactersitics of the data 
colnames(jan23)
head(jan23)
skim_without_charts(jan23)
glimpse(jan23)
str(jan23)
as_tibble(jan23)

## merge individual monthly data frames into one large data frame
tripdata <- bind_rows(jan23, feb23, mar23, apr23, may23, jun23, jul23,
                      aug23, sep23, oct23, nov23, dec23)

# Process Phase of the Data Analysis 
## checking merged data frame
View(tripdata)
colnames(tripdata)  
head(tripdata)  
str(tripdata) 
summary(tripdata)
## Adding date, month, year, day of week columns
update_1 <- tripdata %>%
  mutate(year = format(as.Date(started_at), "%Y")) %>% # extracting the Year
  mutate(month = format(as.Date(started_at), "%B")) %>% # extracting the month
  mutate(date = format(as.Date(started_at), "%d")) %>%  # extracting the date
  mutate(day_of_week = format(as.Date(started_at), "%A")) %>%  # extracting the day of week 
  mutate(ride_length = difftime(ended_at, started_at)) %>% # Get the ride_length
  mutate(start_time = strftime(started_at, "%H")) # Get the start-time
View(update_1)
# converting 'ride_length' to numeric for calculation on data
update_2 <- update_1 %>% 
  mutate(ride_length = as.numeric(ride_length))
# is.numeric(tripdata$ride_length)
View(update_2)
# adding ride distance in km 
update_2$ride_distance <- distGeo(matrix(c(update_2$start_lng, update_2$start_lat), ncol = 2), 
                                  matrix(c(update_2$end_lng, update_2$end_lat), ncol = 2))

update_2$ride_distance <- update_2$ride_distance/1000 #distance in km

# Remove bad Data 
# The dataframe includes a few hundred entries when bikes were taken out of docks 
# and checked for quality by Divvy where ride_length was negative or 'zero'
tripdata_clean <- update_2[!(update_2$ride_length <= 0),]

# Analyse Phase of the Data Analysis 
# first lets check the cleaned data frame
str(tripdata_clean)
# lets check summarised details about the cleaned dataset 
summary(tripdata_clean)

## Conduct descriptive analysis
# descriptive analysis on 'ride_length'
# mean = straight average (total ride length / total rides)
# median = midpoint number of ride length array
# max = longest ride
# min = shortest ride
tripdata_clean %>% 
  summarise(average_ride_length = mean(ride_length), median_length = median(ride_length), 
            max_ride_length = max(ride_length), min_ride_length = min(ride_length))

# Compare the Causal users and members 
# members vs casual riders difference depending on total rides taken
tripdata_clean %>% 
  group_by(member_casual) %>% 
  summarise(ride_count = length(ride_id), ride_percentage = (length(ride_id) / nrow(tripdata_clean)) * 100)


ggplot(data = tripdata_clean) + geom_bar(mapping = aes(x = member_casual, fill = member_casual)) + 
  labs(x="Casuals vs Members", y="Number of Rides", title = "Casuals vs Members distribution")

# Compare Causal users and members based on ride length (mean, median, max and min)
tripdata_clean %>% group_by(member_casual) %>%
  summarise(average_ride_length = mean(ride_length), median_length = median(ride_length), 
            max_ride_length = max(ride_length), min_ride_length = min(ride_length))

# See total rides and average ride time by each day for members vs causal 
# lets fix the days of the week order.
tripdata_clean$day_of_week <- ordered(tripdata_clean$day_of_week, 
                                      levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))


#Visuals and Analysis For Total Rides by type and Day of the week
tripdata_clean %>% 
  group_by(member_casual, day_of_week) %>%  #groups by member_casual
  summarise(number_of_rides = n())   %>%  #calculates the number of rides and average duration
  arrange(member_casual, day_of_week) %>% #sort
  ggplot(aes(x = day_of_week, y = number_of_rides, fill = member_casual)) +
  labs(title ="Total rides by Members and Casual riders Vs. Day of the week") +
  geom_col(width=0.5, position = position_dodge(width=0.5)) +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))

# Visual and Analysis for Average ride time data by day of the week and type 
tripdata_clean %>%  
  group_by(member_casual, day_of_week) %>% 
  summarise(average_ride_length = mean(ride_length), .groups="drop") %>%
  ggplot(aes(x = day_of_week, y = average_ride_length, fill = member_casual)) +
  geom_col(width=0.5, position = position_dodge(width=0.5)) + 
  labs(title ="Average ride time by Members and Casual riders Vs. Day of the week")

# See total rides and average ride time by each month for members vs causal 
# lets fix the months in order.
tripdata_clean$month <- ordered(tripdata_clean$month, 
                                levels=c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))


#Visuals and Analysis For Total Rides by type and Month
tripdata_clean %>%  
  group_by(member_casual, month) %>% 
  summarise(number_of_rides = n(),.groups="drop") %>% 
  arrange(member_casual, month)  %>% 
  ggplot(aes(x = month, y = number_of_rides, fill = member_casual)) +
  labs(title ="Total rides by Members and Casual riders Vs. Month", x = "Month", y= "Number Of Rides") +
  theme(axis.text.x = element_text(angle = 45)) +
  geom_col(width=0.5, position = position_dodge(width=0.5)) +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))

#Visuals and Analysis For  average ride time by type and Month
tripdata_clean %>%  
  group_by(member_casual, month) %>% 
  summarise(average_ride_length = mean(ride_length),.groups="drop") %>%
  ggplot(aes(x = month, y = average_ride_length, fill = member_casual)) +
  geom_col(width=0.5, position = position_dodge(width=0.5)) + 
  labs(title ="Average ride length by Members and Casual riders Vs. Month") +
  theme(axis.text.x = element_text(angle = 30))

#Comparison between Members and Casual riders depending on ride distance
# Analysis and Visuals
tripdata_clean %>% 
  group_by(member_casual) %>% drop_na() %>%
  summarise(average_ride_distance = mean(ride_distance)) %>%
  ggplot() + 
  geom_col(mapping= aes(x= member_casual,y= average_ride_distance,fill=member_casual), show.legend = FALSE)+
  labs(title = "Mean travel distance by Members and Casual riders", x="Member and Casual riders", y="Average distance In Km")

#Analysis and visualization on cyclist's bike demand by hour in a day
tripdata_clean %>%
  ggplot(aes(start_time, fill= member_casual)) +
  labs(x="Hour of the day", title="Cyclistic's Bike demand by hour in a day") +
  geom_bar()

#Analysis and visualization on cyclistic's bike demand per hour by day of the week
tripdata_clean %>%
  ggplot(aes(start_time, fill=member_casual)) +
  geom_bar() +
  labs(x="Hour of the day", title="Cyclistic's bike demand per hour by day of the week") +
  facet_wrap(~ day_of_week)

#Analysis and visualization of Rideable type Vs. total rides by Members and casual riders
tripdata_clean %>%
  group_by(rideable_type) %>% 
  summarise(count = length(ride_id))

ggplot(tripdata_clean, aes(x=rideable_type, fill=member_casual)) +
  labs(x="Rideable type", title="Rideable type Vs. total rides by Members and casual riders") +
  geom_bar()

