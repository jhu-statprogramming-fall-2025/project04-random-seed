library(here)
library(readxl)
library(purrr)
library(dplyr)

all_months_list <- readRDS(here("data_clean", "all_months_clean.rds"))

# =================== LOAD DATA ====================

rds_path <- here("data_clean", "all_months_clean.rds")

# if file already exists, just load it. Otherwise read the 12 excel files and then save as RDS
if (file.exists(rds_path)) {
  all_months_list <- readRDS(rds_path)
  
} else {
  file_names <- c(
    "Summary_Report_January_23_24_clean.xlsx",
    "Summary_Report_February_23_24_clean.xlsx",
    "Summary_Report_March_23_24_clean.xlsx",
    "Summary_Report_April_23_24_clean.xlsx",
    "Summary_Report_May_23_24_clean.xlsx",
    "Summary_Report_June_23_24_clean.xlsx",
    "Summary_Report_July_23_24_clean.xlsx",
    "Summary_Report_August_23_24_clean.xlsx",
    "Summary_Report_September_23_24_clean.xlsx",
    "Summary_Report_October_23_24_clean.xlsx",
    "Summary_Report_November_23_24_clean.xlsx",
    "Summary_Report_December_23_24_clean.xlsx"
  )
  
  all_months_list <- map(
    file_names,
    ~ read_xlsx(here("data_clean", .x))
  )
  
  names(all_months_list) <- month_abbr
  
  # Save the list of 12 data frames as a single RDS object
  saveRDS(all_months_list, rds_path)
}


# ================== ANALYSIS ===================

month_abbr <- c("JAN","FEB","MAR","APR","MAY","JUN",
                "JUL","AUG","SEP","OCT","NOV","DEC")

# removes the last row for each df since last row contains total counts
upd_months_list <- map(all_months_list, ~ slice(.x, 1:(n() - 1)))
names(upd_months_list) <- month_abbr # renaming list


# ==== Combining Data into One Dataset

# function that takes in dataframe and columns of interest:
# Goal: for one monthly df, keep National Park and selected columns 

pick_cols <- function(df, name_of_col) {
  output <- df %>%
    select(
      `National Park`,
      !!paste0(name_of_col, " 2023"),
      !!paste0(name_of_col, " 2024"))
  output
}

# separate monthly counts from list
monthly_clean <- map2(upd_months_list, month_abbr, pick_cols)


# data frame with all 
df_combined_months <- reduce(monthly_clean, full_join, by = "National Park") %>%
  mutate(
    Total2023 = rowSums(across(ends_with(" 2023")), na.rm = TRUE),
    Total2024 = rowSums(across(ends_with(" 2024")), na.rm = TRUE),
    # calculate yearly difference
    TotalDiff = Total2024 - Total2023) %>%
  # order columns: 2023 months, total, 2024 months, total, diff
  select(
    `National Park`,
    matches("2023$"),
    Total2023,
    matches("2024$"),
    Total2024,
    TotalDiff)


### ==== Optional/Additional data

# last row is total counts of each column => this line stores last rows into their own df
last_rows <- map(all_months_list, ~ tail(.x, 1))
names(last_rows) <- month_abbr


# separate YTD's into another list
ytd_clean <- map(upd_months_list, ~ pick_cols(.x, "YTD"))
names(ytd_clean) <- month_abbr


