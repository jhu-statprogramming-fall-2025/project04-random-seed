library(httr)
library(jsonlite)
library(tidyverse)
library(ggplot2)
library(scales)
library(knitr)
library(kableExtra)
library(usmap)
library(viridis)
library(plotly)
library(cluster)
library(gridExtra)


setwd("/Users/hanbo/Desktop/JHU_Assessment/Statistical_Programming_Paradigms/project_4/biostat777-project4-group-random-seed")

base_url <- "https://developer.nps.gov/api/v1"

# API key
api_key <- "6D5xk3VOhF3xDuBJBy8WBc7DaN3cUerHNxQHWThE"

# Function to make API calls
call_nps_api <- function(endpoint, params = list()) {
  params$api_key <- api_key

  response <- GET(
    url = paste0(base_url, endpoint),
    query = params
  )

  if (status_code(response) != 200) {
    stop(paste("API call failed with status:", status_code(response)))
  }

  content(response, "text") %>% fromJSON()
}

# Fetch all parks (limit set to 500 to get most parks)
parks_data <- call_nps_api("/parks", list(limit = 1000))

# Extract the data frame
parks_df <- parks_data$data

cat("Total parks fetched:", nrow(parks_df), "\n")
cat("API total:", parks_data$total, "\n")

# Check structure
glimpse(parks_df)

# Show first few parks
national_parks_df <- parks_df %>%
  filter(designation %in% c("National Park", "National Park & Preserve", "National Parks")) %>%
  select(fullName, states, designation, parkCode, topics, entranceFees, entrancePasses, latitude, longitude, latLong)

# Extract all topics for national parks
parks_topics_long <- national_parks_df %>%
  filter(map_int(topics, nrow) > 0) %>%
  select(parkCode, fullName, topics) %>%
  unnest(topics) %>%
  select(parkCode, fullName, topic_name = name)

# Define the specific topics we want to include
selected_topics <- c(
  "Unique Species", "Trails", "Night Sky", "Forests and Woodlands",
  "Rock Landscapes and Features", "Fossils and Paleontology",
  "Schools and Education", "Waterfalls", "Photography", "Ships and Shipwrecks",
  "Animals", "Wilderness", "Mountains", "Astronomy", "Lakes", "Scenic Views",
  "Fish", "Geology", "River and Riparian", "Explorers and Expeditions",
  "Endangered", "Glaciers", "Freshwater Springs", "Hot Springs", "Ruins",
  "Lighthouses", "Architecture and Building", "Colonization and Settlement",
  "Canyons and Canyonlands", "Volcanoes"
)

# Create binary matrix - wide format
parks_topics_matrix <- parks_topics_long %>%
  # Add a binary indicator
  mutate(present = 1) %>%
  # Keep only selected topics (case-insensitive matching)
  filter(topic_name %in% selected_topics) %>%
  # Pivot to wide format
  pivot_wider(
    id_cols = c(parkCode, fullName),
    names_from = topic_name,
    values_from = present,
    values_fill = 0
  ) %>%
  arrange(fullName)

# Set coordinates to numeric type
national_parks_df$latitude <- as.numeric(national_parks_df$latitude)
national_parks_df$longitude <- as.numeric(national_parks_df$longitude)

# Save to CSV
write_csv(parks_topics_matrix, "national_parks_topic_matrix.csv")
save(national_parks_df, file = "national_parks_df.Rdata")
