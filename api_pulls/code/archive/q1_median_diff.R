library(tidyverse)
library(here)

reddit <- read_csv(here("data", "coke_pepsi_reddit_all.csv"), show_col_types = FALSE)

posts <- reddit |> filter(type == "post")

medians <- posts |>
  group_by(group) |>
  summarise(med_score = median(score, na.rm = TRUE), .groups = "drop")

med_coke  <- medians$med_score[medians$group == "treatment"]
med_pepsi <- medians$med_score[medians$group == "control"]

cat(sprintf("Median Coke score:  %.0f\n", med_coke))
cat(sprintf("Median Pepsi score: %.0f\n", med_pepsi))
cat(sprintf("Difference (Coke - Pepsi): %+.0f upvotes\n", med_coke - med_pepsi))
