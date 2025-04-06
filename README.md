# Project-1
Project 1 for CSIT165
Team Members: Alejandra and Wesley

Summary: This is Project 1 where we are analyzing: Data for 2019 Novel Coronavirus is operated by the John Hopkins University Center for Systems Science and Engineering (JHU CSSE). Data includes daily time series CSV summary tables, including confirmations, recoveries, and deaths. Country/region are countries/regions hat conform to World Health Organization (WHO). Lat and Long refer to coordinates references for the user. Date fields are stored in MM/DD/YYYY format.For this project, we will use global data sets for COVID-19 associated confirmations and deaths.
"Initialize README with group members"
***************I'm up to Objective 4***********************************************

---

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
if (!require("knitr", quietly = TRUE)) install.packages("knitr")
# Load the libraries if not already 
library(dplyr)
library(readr)
library(tidyr)
library(knitr)

covid_data_path <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"
deaths_data_path <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"

# Files COVID cases and deaths-originally used but they kept exiting me out//had me link to ~/OneDrive/Documents/Project-1 
#covid_data_path <- "data/time_series_covid19_confirmed_global.csv"
#deaths_data_path <- "data/time_series_covid19_deaths_global.csv"

# Read the data files
covid_data <- read_csv(covid_data_path)
deaths_cases <- read_csv(deaths_data_path)

# Cid they load correctly? checking for it here
head(covid_data)
head(deaths_cases)

# Objective 1: 
#5 = first date column in excel sheets
date_columns <- colnames(covid_data)[5:ncol(covid_data)]
first_date_column <- 5

# Calculate sums first day of all rehions 
covid_data$first_day_sum <- rowSums(covid_data[, first_date_column, drop = FALSE], na.rm = TRUE)
deaths_cases$first_day_sum <- rowSums(deaths_cases[, first_date_column, drop = FALSE], na.rm = TRUE)

# Combine the data to identify the origin
combined_first_day <- data.frame(
  Country_Region = covid_data$`Country/Region`,
  Sum_Confirmed = covid_data$first_day_sum,
  Sum_Deaths = deaths_cases$first_day_sum,
  Total_Impact = covid_data$first_day_sum + deaths_cases$first_day_sum
)

origin <- combined_first_day[which.max(combined_first_day$Total_Impact),]


# Statement sentence
if (origin$Country_Region == "China") {
  print(paste("The origin is likely China based on the data. It had", origin$Sum_Confirmed, "confirmed cases and", origin$Sum_Deaths, "deaths on the first recorded day."))
} else {
  print(paste("The origin is likely in", origin$Country_Region, "with", origin$Sum_Confirmed, "confirmed cases and", origin$Sum_Deaths, "deaths on the first recorded day."))
}

#Objective 2

# Identify the most recent non-zero increase 

most_recent_date <- NA
most_recent_location <- NA
most_recent_country <- NA

for (i in (ncol(covid_data)-1):5) {
  day_changes <- which(covid_data[, i] == 0 & covid_data[, i+1] > 0)
  if (length(day_changes) > 0) {
    most_recent_date <- colnames(covid_data)[i+1]
    most_recent_location <- covid_data$`Province/State`[day_changes]
    most_recent_country <- covid_data$`Country/Region`[day_changes]
    break  # Stop the loop once the most recent date is found
  }
}
#Objective 3
#installgeosphere library for objective 3 #miles=meters multiplied by 0.000621371 to be used later in this objective
if (!require("geosphere", quietly = TRUE)) install.packages("geosphere")

library(geosphere)

    distance_miles <- distance_meters * 0.000621371 
    print(sprintf("%s is %.2f miles away from %s, %s.",
                  most_recent_country,
                  distance_miles,
                  origin$`Province/State`, 
                  origin$Country_Region))
if (all(!is.na(c(origin_coords$Lat, origin_coords$Long, recent_coords$Lat, recent_coords$Long)))) {
    distance_meters <- distm(matrix(c(origin_coords$Long, origin_coords$Lat), nrow = 1),
                             matrix(c(recent_coords$Long, recent_coords$Lat), nrow = 1),
                             fun = distHaversine)
} else {
    print("Valid coordinates not available for distance calculation.")
}

#objective 4 
#filter cruise ships first 
valid_data <- covid_data %>%
  filter(Lat != 0, Long != 0, !is.na(Lat), !is.na(Long))

# Join confirmed cases with death cases
data <- valid_data %>%
  inner_join(deaths_cases, by = c("Province/State", "Country/Region", "Lat", "Long"))
risk_scores <- data %>%
  mutate(
    Latest_Confirmed = as.numeric(.data[[ncol(.)-3]]),  
    Latest_Deaths = as.numeric(.data[[ncol(.)]]),  
    Risk_Score = (Latest_Deaths / Latest_Confirmed) * 100
  ) %>%
  filter(!is.na(Risk_Score), Latest_Confirmed > 0)
#higher risk score area
highest_risk <- risk_scores %>%
  arrange(desc(Risk_Score), desc(Latest_Confirmed)) %>%
  slice_head(n = 1)

#lowest risk score
lowest_risk <- risk_scores %>%
  arrange(Risk_Score, desc(Latest_Confirmed)) %>%
  slice_head(n = 1)
#global risk score
global_risk_score <- sum(risk_scores$Latest_Deaths, na.rm = TRUE) / sum(risk_scores$Latest_Confirmed, na.rm = TRUE) * 100

print(paste("Highest risk area: ", highest_risk$`Country/Region`, "with a risk score of", highest_risk$Risk_Score))
print(paste("Lowest risk area: ", lowest_risk$`Country/Region`, "with a risk score of", lowest_risk$Risk_Score))
print(paste("Global risk score: ", global_risk_score))
  
  
  
#Objective 5

# Latest confirmed cases for each country
total_confirmed_by_country <- confirmed_cases %>%
  group_by(`Country/Region`) %>%
  summarise(Total_Confirmed = sum(as.numeric(.data[[ncol(.)]]), na.rm = TRUE))

# Latest deaths for each country
total_deaths_by_country <- deaths_cases %>%
  group_by(`Country/Region`) %>%
  summarise(Total_Deaths = sum(as.numeric(.data[[ncol(.)]]), na.rm = TRUE))


# Top 5 countries by confirmed cases
top_confirmed_countries <- total_confirmed_by_country %>%
  arrange(desc(Total_Confirmed)) %>%
  slice_head(n = 5)

# Top 5 countries by deaths
top_deaths_countries <- total_deaths_by_country %>%
  arrange(desc(Total_Deaths)) %>%
  slice_head(n = 5)


kable_confirmed <- kable(top_confirmed_countries, format = "markdown", caption = "Top 5 Countries by Confirmed Cases")
kable_deaths <- kable(top_deaths_countries, format = "markdown", caption = "Top 5 Countries by Deaths")

print(kable_confirmed)
print(kable_deaths)

Which area of the world currently has the lowest risk score (if more than one, display the one with the most confirmations)? 
Which area of the world currently has the highest risk score (if more than one, display the one with the most confirmations)? 
How do risk scores in these areas compare to global risk score? 
Why might it be helpful to calculate metrics like risk scores for different areas of the world and what would their limitations be (what assumptions does risk score make and what important variables might be left out)? 
COVID procedures varied around the world like in Australia where they are an island that can better control the population and COVID. The procedures would them mean that certain areas have different risk scores. Additionally socioeconomic factors play a pivotal role. In areas where communities have a low socioeconomic area they are likelier to display higher risk scores because people may not have the means to get access to good care, may have to work which would increase their risk of getting COVID, etc. 
