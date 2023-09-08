setwd ("C:/Users/Nikki/Documents/DATA") 
##

library(tidyverse)
library(dplyr)
library(lubridate)

aggregated_data <- list.files(pattern = ".csv")
trips_data <- map_df(aggregated_data, read_csv)
write.csv (trips_data, "trips_data.csv", row.names = FALSE)
trips <- read_csv("trips_data.csv")
rm(trips_data)
rm(aggregated_data)
trips_data <- trips_data %>% 
  na.omit()
drop_na(trips_data)
trips_data [!duplicated(trips_data$ride_id),]
__
trips_data_clean <- trips_data %>%
  mutate(trip_length = as.numeric(difftime(ended_at, started_at, units = "mins")),
         weekday = format(as.Date(started_at), "%A")) %>%
  filter(trip_length >= 1, trip_length <= (24*60))
__
trips_data_clean$month <- (month.name[month (trips_data_clean$started_at)])
__
month_count <- trips_data_clean %>% 
  group_by(month,member_casual) %>% 
  summarize(row_count = n()) %>% 
  arrange(match(month,month.name))
__
weekday_count <- trips_data_clean %>% 
  group_by (weekday = weekday, member_casual = member_casual ) %>%
  summarize(row_count = n()) %>%
__
top_starting_station <- trips_data_clean %>% 
  group_by (start_station_name, member_casual) %>% 
  summarize (row_count =n()) %>%
  arrange (desc(row_count))
rm(tops_starting_station)
tops_end_station <- trips_data_clean %>%
  group_by (end_station_name, member_casual) %>% 
  summarize (row_count = n()) %>% 
  arrange (desc(row_count))
____
trip_length_data <- trips_data_clean %>% 
  group_by(member_casual) %>% 
  summarize(mean(trip_length))

