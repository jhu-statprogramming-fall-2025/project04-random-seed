library(tidyverse)
library(here)

standardize_park_name <- function(name) {
  name <- trimws(name)

  name <- gsub("ā", "a", name)
  name <- gsub("ʻ", "", name)
  name <- gsub(" National Park & Preserve$", " NP & PRES", name)
  name <- gsub(" National Park$", " NP", name)
  name <- gsub(" National Parks$", " NP", name)
  name <- gsub(" NP & Preserve$", " NP & PRES", name)
  name <- gsub(" Of", " of", name)
  name <- gsub(" The", " the", name)
  name <- gsub(" & Kings Canyon", "", name)
  name <- gsub("Wrangell - St Elias", "Wrangell-St. Elias", name)
  
  return(name)
}

# Load the topic matrix and monthly visitors data
parks_topics <- read_csv("SSS_dashboard/Data_Folder/national_parks_topic_matrix.csv")
df_combined_months <- read_csv("SSS_dashboard/Data_Folder/df_combined_months.csv")

# Get all 2024 monthly columns
month_cols_2024 <- grep("2024$", names(df_combined_months), value = TRUE)
month_cols_2024 <- month_cols_2024[!month_cols_2024 %in% c("Total2024")]

# Extract month abbreviations
month_abbr <- gsub(" 2024", "", month_cols_2024)

visitation_long <- df_combined_months %>%
  select(`National Park`, all_of(month_cols_2024)) %>%
  pivot_longer(
    cols = all_of(month_cols_2024),
    names_to = "Month",
    values_to = "Visitors"
  ) %>%
  mutate(
    Month = gsub(" 2024", "", Month),
    ParkNameStd = standardize_park_name(`National Park`)
  ) %>%
  # Remove rows with missing visitor data
  filter(!is.na(Visitors), Visitors > 0)


cleaned_names <- gsub(" ", "_", names(parks_topics))
names(parks_topics) <- cleaned_names

# standardize park names in topic data
parks_topics <- parks_topics %>%
  mutate(ParkNameStd = standardize_park_name(fullName))

# merge with topics data
ml_data <- visitation_long %>%
  left_join(
    parks_topics,
    by = "ParkNameStd"
  ) %>%
  filter(!is.na(parkCode))

# Create dummy variables for months
ml_data <- ml_data %>%
  mutate(
    Month_JAN = as.integer(Month == "JAN"),
    Month_FEB = as.integer(Month == "FEB"),
    Month_MAR = as.integer(Month == "MAR"),
    Month_APR = as.integer(Month == "APR"),
    Month_MAY = as.integer(Month == "MAY"),
    Month_JUN = as.integer(Month == "JUN"),
    Month_JUL = as.integer(Month == "JUL"),
    Month_AUG = as.integer(Month == "AUG"),
    Month_SEP = as.integer(Month == "SEP"),
    Month_OCT = as.integer(Month == "OCT"),
    Month_NOV = as.integer(Month == "NOV"),
    Month_DEC = as.integer(Month == "DEC")
  )

# data filtering
topic_cols <- names(parks_topics)[!names(parks_topics) %in% 
                                     c("parkCode", "fullName", "ParkNameStd")]
month_cols <- paste0("Month_", c("JAN","FEB","MAR","APR","MAY","JUN",
                                  "JUL","AUG","SEP","OCT","NOV","DEC"))

ml_data_final <- ml_data %>%
  select(
    parkCode,
    fullName = ParkNameStd,
    Month,
    Visitors,
    all_of(topic_cols),
    all_of(month_cols)
  ) %>%
  arrange(fullName, Month)

# save the data
write_csv(ml_data_final, "SSS_dashboard/Data_Folder/parks_complete_with_month.csv")
save(ml_data_final, topic_cols, month_cols, file = "SSS_dashboard/Data_Folder/ml_training_data.Rdata")
