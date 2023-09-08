# Installing all the needed packages for the case study.

install.packages ("tidyverse")
install.pachages ("dplyr")
install.packages ("lubridate") 
library(tidyverse)
library(dplyr)
library(lubridate)

# given data was chosen from 12 files of ".csv" format.

aggregated_data <- list.files(pattern = ".csv")   ## All the files were aggregated into a value in Rtsudio. 
trips_data <- map_df(aggregated_data, read_csv)   ## The value named as aggregated data was mapped into data.frame with this command and named as trips_data.
write.csv (trips_data, "trips_data.csv", row.names = FALSE) ## Because the file type was way too large I aggregated the data and saved it as a new CSV to avoid looped commands in 12 .csv files.
trips <- read_csv("trips_data.csv") ## Function to read the new .csv file created. 
rm(trips_data) ## deleted the old data frames to make the working space neat and clean (OPTIONAL)
rm(aggregated_data) ## deleted the old data frames to make the working space neat and clean (OPTIONAL)

## Data Cleaning

trips_data <- trips_data %>%  ## created new trips_data from the previous trip_data and using the pipe function to chain the commands. 
  na.omit() ## Used the function to get rid of N/A values.
drop_na(trips_data) ## dropped any N?A values in the data.frame.
trips_data [!duplicated(trips_data$ride_id),] ## duplicated data was deleted with this function. (This function needs to be checked) 

trips_data_clean <- trips_data %>%  ## new data frame made from trips_data 
  mutate(trip_length = as.numeric(difftime(ended_at, started_at, units = "mins")), ## mutate function used to make new column called trip_length which was needed as a numeric no.          ## (difftime function used to get the difference between ended_at and started_at and the units were made to render in units as mins). 
         weekday = format(as.Date(started_at), "%A")) %>%   ## weekday column formed by using format function to format as.Date extracted from the started_at variable as a weekday in English letters ("%A" used in the format function is to put the weekday in words [Important]).  
  filter(trip_length >= 1, trip_length <= (24*60))   ## filtered the column trip_length as the data was needed of the customers driving bike for 24 hours only, so the mins data was filtered to contain with more than or equal to 1 and 24X60 mins which is 24 hours.  

## A new column was made for a month to analyze the use of bikes in different months according to casual users and members. 

trips_data_clean$month <- (month.name[month (trips_data_clean$started_at)]) ## in the data.frame truips_data_clean $ sign used to add a new column by the name of the month, month.name function used outside the column to give a letter name to the month extracted from the column started_at.  

## ANALYZATION IN TABLEAU 
## For analysis data.frames of different columns were rendered to process in Tableau and compare and find trends. 
month_count <- trips_data_clean %>% ## New adata.frame made for month_count usage by different customer types. 
  group_by(month,member_casual) %>% ## grouped the data by month the new column that was made before, and member_casual. 
  summarize(row_count = n()) %>%   ## summarize the same month and same customer types by row_count function 
 arrange(match(month,month.name))  ## arranged the data by matching the month column with the month.name function.  

## weekday_count data.frame rendered. 

weekday_count <- trips_data_clean %>%  ## new data.frame made.
  group_by (weekday = weekday, member_casual = member_casual ) %>% ## data grouped by weekdaty and member_casual 
  summarize(row_count = n()) %>% ## summarized with rows

## top_starting_station and end_station_name data.frames created.  

top_starting_station <- trips_data_clean %>% ## new data.frame made.
  group_by (start_station_name, member_casual) %>% ## group by start_station_name and member_casual. 
  summarize (row_count =n()) %>% ## summarize with counting the  rows
  arrange (desc(row_count)) ## arrange the row count with descending order as the topmost starting station destination name is needed.  

top_end_station <- trips_data_clean %>% ## new data.frame made. 
  group_by (end_station_name, member_casual) %>% ## grouped by end station name and member_casual
  summarize (row_count = n()) %>% ## sumarrized with row count
  arrange (desc(row_count)) ## arrange in descendting order

## trip_length average calculated to compare between member and casual users. 

trip_length_data <- trips_data_clean %>% ## new data.frame made. 
  group_by(member_casual) %>% ## grouped by memebr_casual
  summarize(mean(trip_length)) ## in summarize function mean calculated for the trip length. 

