# Install packages (run once)
install.packages(c("tidyverse", "ggplot2", "dplyr"))

# Load libraries
library(tidyverse)
library(ggplot2)
library(dplyr) 

# Install baseballr if not already installed
if (!requireNamespace("baseballr", quietly = TRUE)) {
  install.packages("remotes")
  remotes::install_github("BillPetti/baseballr")
}

# Load required libraries
library(baseballr)
library(dplyr)

# Function to safely get MLB game data
get_mlb_data <- function(start_date, end_date) {
  tryCatch({
    # Fetch MLB game logs for the given date range
    data <- daily_batter_bref(start_date = start_date, end_date = end_date)
    
    if (is.null(data) || nrow(data) == 0) {
      stop("No data returned for the given date range.")
    }
    
    return(data)
  }, error = function(e) {
    message("Error fetching data: ", e$message)
    return(NULL)
  })
}

# Example: Get data for April 2023
mlb_data <- get_mlb_data("2023-04-01", "2023-04-30")

if (!is.null(mlb_data)) {
  
  # Calculate team batting averages
  team_batting <- mlb_data %>%
    group_by(Tm) %>%
    summarise(
      Games = n(),
      Avg_BA = round(mean(BA, na.rm = TRUE), 3),
      Total_HR = sum(HR, na.rm = TRUE)
    ) %>%
    arrange(desc(Avg_BA))
  
  cat("\n=== Team Batting Averages (April 2023) ===\n")
  print(team_batting)
  
  # Top 10 home run hitters
  top_hr_hitters <- mlb_data %>%
    group_by(Name) %>%
    summarise(Total_HR = sum(HR, na.rm = TRUE)) %>%
    arrange(desc(Total_HR)) %>%
    slice_head(n = 10)
  
  cat("\n=== Top 10 HR Hitters (April 2023) ===\n")
  print(top_hr_hitters)
}
