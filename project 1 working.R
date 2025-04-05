
---
  title: Project 1
authors: Alejandra and Wesley
output: html
---
  #confirmed cases
  covid_data <- read.csv("time_series_covid19_confirmed_global.csv")
  #deaths data
deaths_cases <- read.csv("time_series_covid19_deaths_global.csv")

#Objective 1
first_day <- names(covid_data)[5]  

if (first_day == names(deaths_cases)[5]) {
  
  origin_analysis <- covid_data 
    mutate(Deaths = deaths_cases[[first_day]]) 
    group_by(`Country.Region`) 
    summarize(Total_Confirmed = sum(`[[first_day]]`), Total_Deaths = sum(Deaths), .groups = 'drop') 
    arrange(desc(Total_Confirmed), desc(Total_Deaths))
  
 
  origin <- origin_analysis[1, ]
  
  if(origin$Total_Confirmed > 0) {
    cat("Origin is", origin$`Country.Region`, "with", origin$Total_Confirmed, "confirmed and", origin$Total_Deaths, "deaths on the first recorded day.\n")
  }
  
  #Objective 2
  recent_day <- tail(names(covid_data), 1)
  most_recent_area <- covid_data 
    mutate(Deaths = deaths_cases[[recent_day]]) 
    group_by(`Country.Region`) 
    summarize(Total_Confirmed = sum(`[[recent_day]]`), Total_Deaths = sum(Deaths), .groups = 'drop') 
    arrange(desc(Total_Confirmed), desc(Total_Deaths))
  print(most_recent_area)
}

#Objective3
origin_coords <- cbind(origin$Long, origin$Lat)
recent_coords <- cbind(most_recent_area$Long, most_recent_area$Lat)

distance_meters <- distm(origin_coords, recent_coords, fun = distVincentySphere)
distance_miles <- distance_meters / 1609.34  #tochangetomiles

cat(most_recent_area$`Province.State`, "in", most_recent_area$`Country.Region`, "is about", round(distance_miles), "miles away from", origin$`Country.Region`, "\n")

#Objective4


# the last column from each data frame
latest_confirmed <- confirmed[, ncol(confirmed)]
latest_deaths <- deaths[, ncol(deaths)]

#filtering out rows where Lat and Long are 0 or NA
valid_indices <- which(!(confirmed$Lat == 0 & confirmed$Long == 0 | is.na(confirmed$Lat) | is.na(confirmed$Long)))


confirmed_valid <- confirmed[valid_indices, ]
deaths_valid <- deaths[valid_indices, ]
latest_confirmed_valid <- latest_confirmed[valid_indices]
latest_deaths_valid <- latest_deaths[valid_indices]
risk_scores <- (latest_deaths_valid / latest_confirmed_valid) * 100

#lowest and highest risk score

min_risk_score <- min(risk_scores, na.rm = TRUE)
max_risk_score <- max(risk_scores, na.rm = TRUE)

# Indices of min and max risk scores
min_risk_indices <- which(risk_scores == min_risk_score)
max_risk_indices <- which(risk_scores == max_risk_score)


if (length(min_risk_indices) > 1) {
  min_risk_index <- min_risk_indices[which.max(latest_confirmed_valid[min_risk_indices])]
} else {
  min_risk_index <- min_risk_indices
}

if (length(max_risk_indices) > 1) {
  max_risk_index <- max_risk_indices[which.max(latest_confirmed_valid[max_risk_indices])]
} else {
  max_risk_index <- max_risk_indices
}

# Print results
cat("Area with the lowest risk score:", confirmed_valid$`Country.Region`[min_risk_index], "-", confirmed_valid$`Province.State`[min_risk_index], "with a risk score of", min_risk_score, "\n")
cat("Area with the highest risk score:", confirmed_valid$`Country.Region`[max_risk_index], "-", confirmed_valid$`Province.State`[max_risk_index], "with a risk score of", max_risk_score, "\n")

#Objective5
library(knitr)
total_confirmed_by_country <- aggregate(confirmed[, ncol(confirmed)] ~ `Country.Region`, data = confirmed, sum)
colnames(total_confirmed_by_country) <- c("Country.Region", "Total Confirmed")
total_deaths_by_country <- aggregate(deaths[, ncol(deaths)] ~ `Country.Region`, data = deaths, sum)
colnames(total_deaths_by_country) <- c("Country.Region", "Total Deaths")

#top 5
top_confirmed_countries <- total_confirmed_by_country[order(-total_confirmed_by_country$`Total Confirmed`),][1:5,]

top_death_countries <- total_deaths_by_country[order(-total_deaths_by_country$`Total Deaths`),][1:5,]

#tobuildinkable
confirmed_table <- kable(top_confirmed_countries, format = "markdown", caption = "Top 5 Countries COVID-19 Confirmed Cases")
deaths_table <- kable(top_death_countries, format = "markdown", caption = "Top 5 Countries COVID-19 Deaths")

#toprint/display
confirmed_table
deaths_table