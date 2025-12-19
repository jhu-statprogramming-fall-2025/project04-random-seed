library(tidyverse)

# =================== LOAD PARK FEATURE VECTORS ===================

#' Load pre-computed park feature vectors
#' 
#' Loads park vectors from CSV file created by prepare_park_vectors.R
#' 
#' @param vector_file Path to park vectors CSV
#' @param metadata_file Path to feature metadata RData
#' @return List with park vectors and metadata
load_park_vectors <- function(vector_file = "Data_Folder/park_feature_vectors.csv",
                              metadata_file = "Data_Folder/feature_metadata.Rdata") {
  
  # Check if files exist
  if(!file.exists(vector_file)) {
    stop(paste("Park vectors file not found:", vector_file,
               "\nRun: source('prepare_park_vectors.R')"))
  }
  
  if(!file.exists(metadata_file)) {
    stop(paste("Metadata file not found:", metadata_file,
               "\nRun: source('prepare_park_vectors.R')"))
  }
  
  # Load park vectors
  park_vectors <- read_csv(vector_file, show_col_types = FALSE)
  
  # Load metadata
  load(metadata_file)
  
  list(
    park_vectors = park_vectors,
    topic_cols = feature_metadata$topic_cols,
    month_cols = feature_metadata$month_cols,
    parks = park_vectors$fullName,
    n_parks = nrow(park_vectors)
  )
}

# =================== RECOMMENDATION FUNCTION ===================

#' Generate park recommendations based on suitability and popularity
#' 
#' Recommendation Score: α × Popularity + (1-α) × Suitability
#' Popularity: normalized total 2024 visitors
#' Suitability: correlation between user vector and park vector
#' α: user preference weight (0 to 1)
#' 
#' @param park_data Park vectors from load_park_vectors()
#' @param selected_topics Character vector of user-selected topics
#' @param preferred_month Single month abbreviation
#' @param alpha Weight for popularity
#' @param top_n Number of recommendations to return
#' @return Data frame with park recommendations and scores
recommend_parks <- function(park_data,
                           selected_topics,
                           preferred_month,
                           alpha = 0.5,
                           top_n = 10) {
  
  # Validate inputs
  if(length(selected_topics) == 0) {
    stop("Please select at least one topic")
  }
  
  if(is.null(preferred_month) || preferred_month == "Any") {
    stop("Please select a specific month (not 'Any')")
  }
  
  if(alpha < 0 || alpha > 1) {
    stop("Alpha must be between 0 and 1")
  }
  
  # Get park vectors
  parks_df <- park_data$park_vectors
  
  # Create user preference vector
  # Topics: 1 if selected, 0 otherwise
  user_topics <- setNames(rep(0, length(park_data$topic_cols)), park_data$topic_cols)
  valid_topics <- intersect(selected_topics, park_data$topic_cols)
  user_topics[valid_topics] <- 1
  
  # Months: 1 for selected month, 0 for others
  user_months <- setNames(rep(0, length(park_data$month_cols)), park_data$month_cols)
  month_col <- paste0("Prop_", preferred_month)
  if(month_col %in% park_data$month_cols) {
    user_months[month_col] <- 1
  } else {
    stop(paste("Invalid month:", preferred_month))
  }
  
  # Combine into full user vector
  user_vector <- c(user_topics, user_months)
  
  # Calculate suitability (correlation) for each park
  feature_cols <- c(park_data$topic_cols, park_data$month_cols)
  
  suitability_scores <- apply(parks_df[, feature_cols], 1, function(park_vector) {
    # Calculate correlation
    if(sd(park_vector) == 0 || sd(user_vector) == 0) {
      return(0)  # If no variation, correlation is undefined
    }
    cor(user_vector, park_vector, method = "pearson")
  })
  
  suitability_normalized <- (suitability_scores + 1) / 2
  
  # Calculate popularity score
  popularity_scores <- parks_df$TotalVisitors2024
  popularity_normalized <- (popularity_scores - min(popularity_scores)) / 
                          (max(popularity_scores) - min(popularity_scores))
  
  # Calculate final recommendation score
  final_scores <- alpha * popularity_normalized + (1 - alpha) * suitability_normalized
  
  # Create results
  results <- data.frame(
    Park = parks_df$fullName,
    parkCode = parks_df$parkCode,
    Suitability = suitability_normalized,
    Popularity = popularity_normalized,
    FinalScore = final_scores,
    TotalVisitors = parks_df$TotalVisitors2024,
    Correlation = suitability_scores
  ) %>%
    arrange(desc(FinalScore)) %>%
    head(top_n) %>%
    mutate(
      Rank = row_number(),
      SuitabilityPct = round(Suitability * 100, 1),
      PopularityPct = round(Popularity * 100, 1),
      FinalScorePct = round(FinalScore * 100, 1)
    )
  
  # Add attributes
  attr(results, "alpha") <- alpha
  attr(results, "month") <- preferred_month
  attr(results, "topics_selected") <- valid_topics
  
  return(results)
}