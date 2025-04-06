# Project-1
Project 1 for CSIT165
Team Members: Alejandra and Wesley

Summary: This is Project 1 where we are analyzing: Data for 2019 Novel Coronavirus is operated by the John Hopkins University Center for Systems Science and Engineering (JHU CSSE). Data includes daily time series CSV summary tables, including confirmations, recoveries, and deaths. Country/region are countries/regions hat conform to World Health Organization (WHO). Lat and Long refer to coordinates references for the user. Date fields are stored in MM/DD/YYYY format.For this project, we will use global data sets for COVID-19 associated confirmations and deaths.
"Initialize README with group members"
***************I'm up to Objective 4***********************************************

---
title: "Project 1-New"
author: "Alejandra and Wes"
date: "2025-04-05"
output: html_document
---
# Libraries
if (!require("dplyr", quietly = TRUE)) install.packages("dplyr")
if (!require("readr", quietly = TRUE)) install.packages("readr")
if (!require("tidyr", quietly = TRUE)) install.packages("tidyr")

# Load the libraries if not already 
library(dplyr)
library(readr)
library(tidyr)

# Files COVID cases and deaths
covid_data_path <- "data/time_series_covid19_confirmed_global.csv"
deaths_data_path <- "data/time_series_covid19_deaths_global.csv"

# Read the data files
covid_data <- read_csv(covid_data_path)
deaths_cases <- read_csv(deaths_data_path)

# Cid they load correctly? checking for it here
head(covid_data)
head(deaths_cases)

# Objective 1: 
first_date_column <- 2  

confirmed_cases$first_day_sum <- rowSums(confirmed_cases[, first_date_column, drop = FALSE])
deaths_cases$first_day_sum <- rowSums(deaths_cases[, first_date_column, drop = FALSE])

# Combine the data for the first day to identify the origin
combined_first_day <- data.frame(
  Country_Region = confirmed_cases$`Country/Region`,
  Sum_Confirmed = confirmed_cases$first_day_sum,
  Sum_Deaths = deaths_cases$first_day_sum
)


combined_first_day$total_impact <- combined_first_day$Sum_Confirmed + combined_first_day$Sum_Deaths


origin <- combined_first_day[which.max(combined_first_day$total_impact),]
# Statement sentence
if (origin$Country_Region == "China") {
  print(paste("The origin is likely China based on the data. It had", origin$Sum_Confirmed, "confirmed cases and", origin$Sum_Deaths, "deaths on the first recorded day."))
} else {
  print(paste("The origin is likely in", origin$Country_Region, "with", origin$Sum_Confirmed, "confirmed cases and", origin$Sum_Deaths, "deaths on the first recorded day."))
}

#Objective 2
date_columns <- colnames(confirmed_cases)[2:ncol(confirmed_cases)]

# Initialize variables to store the most recent date and location of the first case
most_recent_date <- NA
most_recent_location <- NA
most_recent_country <- NA

# Loop through each date column to find the most recent first case
for (i in 2:(ncol(confirmed_cases) - 1)) {
  
  for (j in 1:nrow(confirmed_cases)) {
    if (confirmed_cases[j, i] == 0 && confirmed_cases[j, i + 1] > 0) {
      most_recent_date <- colnames(confirmed_cases)[i + 1]
      most_recent_location <- confirmed_cases[j, `Province/State`]
      most_recent_country <- confirmed_cases[j, `Country/Region`]
    }
  }
}

# Using an if statement
if (!is.na(most_recent_date)) {
  message <- sprintf("The most recent area to report its first confirmed case was on %s in %s, %s.",
                     most_recent_date, most_recent_location, most_recent_country)
  print(message)
} else {
  print("No new first cases found in the dataset.")
}
#installgeosphere library for objective 3
if (!require("geosphere", quietly = TRUE)) install.packages("geosphere")

library(geosphere)


origin_country <- origin$Country_Region  
most_recent_country <- most_recent_country  



origin_coords <- confirmed_cases %>% 
  filter(`Country/Region` == origin_country) %>%
  summarise(Lat = mean(Lat, na.rm = TRUE), Long = mean(Long, na.rm = TRUE))

# Extract coordinates for the most recent location with a first case
recent_coords <- confirmed_cases %>%
  filter(`Country/Region` == most_recent_country) %>%
  summarise(Lat = mean(Lat, na.rm = TRUE), Long = mean(Long, na.rm = TRUE))
#to miles and exact distance
# Calculate distance in meters
distance_meters <- distm(c(origin_coords$Long, origin_coords$Lat), c(recent_coords$Long, recent_coords$Lat), fun = distHaversine)


distance_miles <- distance_meters * 0.000621371


print(sprintf("%s is %.2f miles away from %s, %s.",
              most_recent_country,
              distance_miles,
              "Origin City",  # replace with actual city if known or use origin_country
              origin_country))
