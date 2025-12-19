library(tidyverse)

# Load the data from part 1
load("SSS_dashboard/Data_Folder/ml_training_data.Rdata")

# Calculate monthly proportions for each park
park_monthly_props <- ml_data_final %>%
  group_by(fullName) %>%
  mutate(
    TotalVisitors = sum(Visitors, na.rm = TRUE),
    MonthlyProportion = Visitors / TotalVisitors
  ) %>%
  ungroup() %>%
  select(fullName, parkCode, Month, MonthlyProportion, Visitors, TotalVisitors, all_of(topic_cols))

park_popularity <- park_monthly_props %>%
  group_by(fullName, parkCode) %>%
  summarise(
    TotalVisitors2024 = first(TotalVisitors),
    .groups = "drop"
  )

park_vectors <- park_monthly_props %>%
  select(fullName, parkCode, Month, MonthlyProportion, all_of(topic_cols)) %>%
  # Pivot monthly proportions to wide format
  pivot_wider(
    id_cols = c(fullName, parkCode, all_of(topic_cols)),
    names_from = Month,
    values_from = MonthlyProportion,
    names_prefix = "Prop_"
  ) %>%
  # Add total visitors
  left_join(park_popularity, by = c("fullName", "parkCode"))

# Ensure all months are present (in case any missing)
month_cols <- paste0("Prop_", c("JAN","FEB","MAR","APR","MAY","JUN",
                                 "JUL","AUG","SEP","OCT","NOV","DEC"))

for(col in month_cols) {
  if(!col %in% names(park_vectors)) {
    park_vectors[[col]] <- 0
  }
}

park_vectors <- park_vectors %>% 
  mutate(across(everything(), ~ replace_na(.x, 0)))

col_order <- c("parkCode", "fullName", topic_cols, month_cols, "TotalVisitors2024")
park_vectors <- park_vectors %>%
  select(all_of(col_order))

# Save as CSV
write_csv(park_vectors, "SSS_dashboard/Data_Folder/park_feature_vectors.csv")

# Save topic and month column names for reference
feature_metadata <- list(
  topic_cols = topic_cols,
  month_cols = month_cols,
  n_parks = nrow(park_vectors),
  n_features = ncol(park_vectors) - 3
)

save(feature_metadata, file = "SSS_dashboard/Data_Folder/feature_metadata.Rdata")