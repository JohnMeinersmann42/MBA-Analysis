# ── Libraries ───────────────────────────────────────────────────────────────── ----
# tidyverse: core data manipulation and plotting
# lubridate: date parsing helpers
# gridExtra: tableGrob for rendering tibbles as plot objects
library(here)
library(tidyverse)
library(lubridate)
library(gridExtra)
library(ggridges)
library(cowplot)
library(magick)


# ── Load Data ───────────────────────────────────────────────────────────────── ----

# Read pitch-level Statcast data; cast game_date to Date immediately
df <- read_csv(
  here("data", "statcast_aaa_2023_2024_2025_combined.csv"),
  show_col_types = FALSE
) |>
  mutate(game_date = as.Date(game_date))

# Read venue-game mapping; cast game_date to Date immediately
venue_games <- read_csv(
  here("data", "aaa_venue_games.csv"),
  show_col_types = FALSE
) |>
  mutate(game_date = as.Date(game_date))

# Player dimension: maps MLBAM IDs to full names; used for plot titles
players_dim <- read_csv(
  here("data", "players_dim_2023_2025.csv"),
  show_col_types = FALSE
)


# ── Filter: Regular Season, 2023-04-01 to 2024-06-24 ───────────────────────── ----

# Restrict Statcast data to the analysis window and regular season games only
# Two-way players share the same MLBAM ID in both batter and pitcher columns;
# exclude any batter ID that also appears as a pitcher ID
two_way_ids <- intersect(
  unique(df$batter),
  unique(df$pitcher)
)

filtered <- df |>
  filter(game_date >= as.Date("2023-04-01"),
         game_date <= as.Date("2024-06-24"),
         game_type == "R",
         !batter %in% two_way_ids) |>
  mutate(abs_type = if_else(wday(game_date, week_start = 1) <= 4, "ABS", "Challenge"))

# Apply the same date window to the venue-game data
venue_games_filtered <- venue_games |>
  filter(game_date >= as.Date("2023-04-01"),
         game_date <= as.Date("2024-06-24"))


# ── Dataset Overview ─────────────────────────────────────────────────────────── ----

# High-level record and coverage counts for the filtered dataset
cat("=== Date Range Summary ===\n")
cat("Records in range:    ", nrow(filtered), "\n")
cat("Total records:       ", nrow(df), "\n")
cat("Date min:            ", as.character(min(filtered$game_date)), "\n")
cat("Date max:            ", as.character(max(filtered$game_date)), "\n")
cat("Unique game dates:   ", n_distinct(filtered$game_date), "\n")
cat("Unique game PKs:     ", n_distinct(filtered$game_pk), "\n")
cat("Unique batters:      ", n_distinct(filtered$batter), "\n")
cat("Unique pitchers:     ", n_distinct(filtered$pitcher), "\n")
cat("Unique home teams:   ", n_distinct(filtered$home_team), "\n")
cat("Unique months:       ", n_distinct(floor_date(filtered$game_date, "month")), "\n")

# Collapse distinct entity counts into a single summary row
cat("\n=== Distinct Counts ===\n")
filtered |>
  summarise(
    pitchers = n_distinct(pitcher),
    batters  = n_distinct(batter),
    games    = n_distinct(game_pk),
    days     = n_distinct(game_date)
  ) |>
  print()

# Show how many pitch records fall in each season year
cat("\n=== Records per Game Year ===\n")
filtered |>
  count(game_year) |>
  print()


# ── Team & Venue Summaries ──────────────────────────────────────────────────── ----

# Pull every unique home team that appears in the filtered window, sorted A–Z
cat("\n=== Unique Home Teams ===\n")
filtered |>
  distinct(home_team) |>
  arrange(home_team) |>
  print(n = Inf)

# Count of distinct home teams present in the filtered venue-game data
cat("\n=== Unique Home Teams (venue_games_filtered) ===\n")
venue_games_filtered |>
  summarise(n_distinct(home_team)) |>
  print()

# Count of unique games hosted at venue 5445 during the 2023 season
cat("\n=== Games at Venue 5445 in 2023 ===\n")
venue_games |>
  filter(venue_id == 5445, season == 2023) |>
  summarise(games = n_distinct(game_pk)) |>
  print()


# ── Pitch-Level Breakdowns ──────────────────────────────────────────────────── ----

# Count pitches by type and compute each type's share of total pitches
cat("\n=== Pitch Type Breakdown ===\n")
filtered |>
  count(pitch_name, sort = TRUE) |>
  mutate(pct = round(n / sum(n) * 100, 1)) |>
  print(n = Inf)

# Count final plate appearance outcomes; excludes rows where events is blank/NA
cat("\n=== Event Breakdown (plate appearances) ===\n")
filtered |>
  filter(!is.na(events), events != "") |>
  count(events, sort = TRUE) |>
  print(n = Inf)


# ── At-Bat Level: Last Pitch per At-Bat ─────────────────────────────────────── ----

# Reduce to one row per at-bat by keeping the pitch with the highest pitch_number;
# with_ties = FALSE guarantees exactly one row even if pitch_number ties exist
last_pitch_per_ab <- filtered |>
  group_by(game_pk, at_bat_number) |>
  slice_max(pitch_number, n = 1, with_ties = FALSE) |>
  ungroup()


# ── Batter Event Counts ─────────────────────────────────────────────────────── ----

# Roll up counting stats per batter using standard baseball definitions:
#   games_played = distinct game_pks the batter appeared in
#   hits         = 1B + 2B + 3B + HR
#   walks        = BB + IBB
#   hbp          = hit by pitch
#   at_bats      = all PA outcomes that count as an AB (excludes BB, HBP, SF, SB)
#   sac_flies    = SF and SF that end a double play
event_summary <- last_pitch_per_ab |>
  group_by(batter, abs_type) |>
  summarise(
    games_played = n_distinct(game_pk),
    hits         = sum(events %in% c("single", "double", "triple", "home_run"), na.rm = TRUE),
    walks        = sum(events %in% c("walk", "intent_walk"), na.rm = TRUE),
    hbp          = sum(events == "hit_by_pitch", na.rm = TRUE),
    at_bats      = sum(events %in% c("single", "double", "triple", "home_run",
                                      "strikeout", "strikeout_double_play",
                                      "field_out", "force_out", "grounded_into_double_play",
                                      "double_play", "triple_play",
                                      "fielders_choice", "fielders_choice_out",
                                      "field_error"), na.rm = TRUE),
    sac_flies    = sum(events %in% c("sac_fly", "sac_fly_double_play"), na.rm = TRUE),
    .groups = "drop"
  )

# Print full batter-level table
print(event_summary)


# ── Summary Statistics of Batter Counts ────────────────────────────────────── ----

# Compute five summary stats (mean, median, SD, min, max) for each counting stat;
# across() applies the same set of functions to all selected columns at once
event_stats <- event_summary |>
  ungroup() |>
  select(games_played, hits, walks, hbp, at_bats, sac_flies) |>
  summarise(across(everything(), list(
    mean   = \(x) round(mean(x), 2),
    median = \(x) round(median(x), 2),
    sd     = \(x) round(sd(x), 2),
    min    = \(x) round(min(x), 2),
    max    = \(x) round(max(x), 2)
  ))) |>
  # pivot_longer collapses the wide stat columns into metric + stat name pairs
  pivot_longer(everything(),
               names_to  = c("metric", "stat"),
               names_sep = "_(?=[^_]+$)") |>
  # pivot_wider turns the stat names (mean, median, …) back into columns
  pivot_wider(names_from = stat, values_from = value) |>
  # final round ensures 2 decimal places survive the pivot transformations
  mutate(across(where(is.numeric), \(x) round(x, 2)))

# Convert all values to character so tableGrob renders exact decimal strings
table_data <- setNames(
  as.data.frame(lapply(event_stats, as.character)),
  names(event_stats)
)

# Render the summary table as a ggplot object to avoid console truncation
ggplot() +
  theme_void() +
  annotation_custom(
    tableGrob(table_data, rows = NULL,
              theme = ttheme_minimal(base_size = 12)),
    xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf
  )


# ── Histograms of Batter Counting Stats ────────────────────────────────────── ----

# Lookup table mapping raw column names to human-readable facet labels
metric_labels <- c(
  games_played = "Games Played",
  hits         = "Hits",
  walks        = "Walks",
  hbp          = "Hit By Pitch",
  at_bats      = "At Bats",
  sac_flies    = "Sacrifice Flies"
)

# Pivot to long format and apply human-readable labels before plotting
hist_long <- event_summary |>
  select(games_played, hits, walks, hbp, at_bats, sac_flies) |>
  pivot_longer(everything(), names_to = "metric", values_to = "value") |>
  mutate(metric = recode(metric, !!!metric_labels))

# Pre-compute mean and median per metric to use as reference lines in each facet
hist_refs <- hist_long |>
  group_by(metric) |>
  summarise(mean   = mean(value),
            median = median(value))

# Sparse metrics (small integer ranges) use binwidth = 1 to avoid empty-bin gaps;
# all other metrics use bins = 30 for a smoother distribution shape
sparse_metrics <- c("Hit By Pitch", "Sacrifice Flies")

hist_long |>
  ggplot(aes(x = value)) +
  geom_histogram(data = ~ filter(.x, !metric %in% sparse_metrics),
                 bins = 30, fill = "steelblue", color = "white") +
  geom_histogram(data = ~ filter(.x, metric %in% sparse_metrics),
                 binwidth = 1, fill = "steelblue", color = "white") +
  # vertical line for the mean; drawn from hist_refs so it aligns per facet
  geom_vline(data = hist_refs, aes(xintercept = mean, color = "Mean"),
             linewidth = 0.8, linetype = "dashed") +
  # vertical line for the median
  geom_vline(data = hist_refs, aes(xintercept = median, color = "Median"),
             linewidth = 0.8, linetype = "dotted") +
  scale_color_manual(name = "", values = c(Mean = "red", Median = "orange")) +
  facet_wrap(~ metric, scales = "free", ncol = 3) +
  labs(title = "Distribution of Batter Count Stats",
       x     = "Count",
       y     = "Number of Batters") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5))


# ── On-Base Percentage Distribution ────────────────────────────────────────── ----

# OBP = (H + BB + HBP) / (AB + BB + HBP + SF); exclude batters with no PA
obp_data <- event_summary |>
  mutate(obp = (hits + walks + hbp) / (at_bats + walks + hbp + sac_flies)) |>
  filter(!is.nan(obp))  # drop batters with zero denominator

# Append OBP row to the existing event_stats summary table and re-render
obp_row <- obp_data |>
  summarise(
    metric = "OBP",
    mean   = round(mean(obp), 4),
    median = round(median(obp), 4),
    sd     = round(sd(obp), 4),
    min    = round(min(obp), 4),
    max    = round(max(obp), 4)
  )

event_stats_with_obp <- bind_rows(event_stats, obp_row)

# Cast to character so tableGrob renders exact decimal strings without rounding
table_data_with_obp <- setNames(
  as.data.frame(lapply(event_stats_with_obp, as.character)),
  names(event_stats_with_obp)
)

# Render updated summary table including the OBP row
ggplot() +
  theme_void() +
  annotation_custom(
    tableGrob(table_data_with_obp, rows = NULL,
              theme = ttheme_minimal(base_size = 12)),
    xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf
  )

# Pre-compute overall mean and median for reference lines on the histogram
obp_refs <- obp_data |>
  summarise(mean   = mean(obp),
            median = median(obp))

# Overall OBP histogram across all batters and treatments combined
obp_data |>
  ggplot(aes(x = obp)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  geom_vline(data = tibble(val = c(obp_refs$mean, obp_refs$median),
                            stat = c("Mean", "Median")),
             aes(xintercept = val, color = stat),
             linewidth = 0.8) +
  scale_color_manual(name = "", values = c(Mean = "red", Median = "orange")) +
  scale_x_continuous(labels = scales::number_format(accuracy = 0.0001)) +
  labs(title = "On-Base Percentage Distribution (2023-04-01 to 2024-06-24)",
       x     = "OBP",
       y     = "Number of Batters") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5))


# ── OBP Distribution by ABS Type ───────────────────────────────────────────── ----

# Violin shows full distribution shape; narrow boxplot inside marks median and IQR
obp_data |>
  ggplot(aes(x = abs_type, y = obp, fill = abs_type)) +
  geom_violin(trim = FALSE, alpha = 0.3) +
  geom_boxplot(width = 0.1, fill = "white", outlier.shape = NA) +
  scale_fill_manual(values = c(ABS = "steelblue", Challenge = "tomato")) +
  labs(title = "OBP Distribution by ABS Type (2023-04-01 to 2024-06-24)",
       x     = "ABS Type",
       y     = "OBP") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5))


# ── OBP Density by ABS Type ─────────────────────────────────────────────────── ----

# Overlapping density curves make it easy to compare the shape of ABS vs Challenge
obp_data |>
  ggplot(aes(x = obp, fill = abs_type, color = abs_type)) +
  geom_density(alpha = 0.4) +
  scale_fill_manual(values  = c(ABS = "steelblue", Challenge = "tomato")) +
  scale_color_manual(values = c(ABS = "steelblue", Challenge = "tomato")) +
  labs(title = "OBP Density by ABS Type (2023-04-01 to 2024-06-24)",
       x     = "OBP",
       y     = "Density",
       fill  = "ABS Type",
       color = "ABS Type") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5))


# ── OBP Difference by Player (ABS vs Challenge) ─────────────────────────────── ----

# Pivot obp_data wide so each batter has one column per abs_type, then compute
# the difference (ABS - Challenge); drop batters missing either condition
obp_diff <- obp_data |>
  select(batter, abs_type, obp) |>
  pivot_wider(names_from = abs_type, values_from = obp) |>
  filter(!is.na(ABS), !is.na(Challenge)) |>
  mutate(obp_diff = ABS - Challenge) |>
  arrange(desc(obp_diff))

print(obp_diff)

# Density of per-batter OBP difference; dashed line at 0 = no difference
obp_diff |>
  ggplot(aes(x = obp_diff)) +
  geom_density(fill = "steelblue", color = "steelblue", alpha = 0.4) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  labs(title = "Density of OBP Difference by Player (ABS - Challenge)",
       x     = "OBP Difference (ABS - Challenge)",
       y     = "Density") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(hjust = 0.5))


# ── Pitch Mix by Treatment ──────────────────────────────────────────────────── ----

# Pitch-level ball/strike rates from the `type` column (B = ball, S = strike);
# strikeout rate computed at the at-bat level from last_pitch_per_ab
pitch_mix_by_treatment <- filtered |>
  group_by(abs_type,batter) |>
  summarise(
    total_pitches  = n(),
    ball_pct       = round(mean(type == "B", na.rm = TRUE) * 100, 2),
    strike_pct     = round(mean(type == "S", na.rm = TRUE) * 100, 2),
    in_play_pct    = round(mean(type == "X", na.rm = TRUE) * 100, 2)
  ) |>
  left_join(
    last_pitch_per_ab |>
      group_by(abs_type) |>
      summarise(
        total_abs    = n(),
        strikeout_pct = round(mean(events %in% c("strikeout", "strikeout_double_play"),
                                   na.rm = TRUE) * 100, 2)
      ),
    by = "abs_type"
  )

print(pitch_mix_by_treatment)

# Ridgeline plot: distribution of each pitch mix % per batter, split by treatment;
# quantile_lines = TRUE draws a median line through each ridge
pitch_mix_by_treatment |>
  select(batter, abs_type, ball_pct, strike_pct, in_play_pct) |>
  pivot_longer(cols = c(ball_pct, strike_pct, in_play_pct),
               names_to  = "metric",
               values_to = "pct") |>
  mutate(metric = recode(metric,
                         ball_pct    = "Ball %",
                         strike_pct  = "Strike %",
                         in_play_pct = "In Play %")) |>
  ggplot(aes(x = pct, y = metric, fill = abs_type)) +
  geom_density_ridges(alpha = 0.5, scale = 1, rel_min_height = 0.01,
                      quantile_lines = TRUE, quantiles = 2) +
  scale_fill_manual(values = c(ABS = "steelblue", Challenge = "tomato")) +
  scale_y_discrete(expand = expansion(mult = c(0.05, 0.4))) +
  coord_cartesian(clip = "off") +
  labs(title = "Distribution of Pitch Mix % by Treatment",
       x     = "Percentage",
       y     = NULL,
       fill  = "ABS Type") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.6))

# Pivot wide to get one ABS and one Challenge column per metric per batter,
# then subtract to get the per-batter difference; drop batters missing either side
pitch_mix_diff <- pitch_mix_by_treatment |>
  select(batter, abs_type, ball_pct, strike_pct, in_play_pct) |>
  pivot_wider(names_from = abs_type,
              values_from = c(ball_pct, strike_pct, in_play_pct)) |>
  filter(!is.na(ball_pct_ABS),    !is.na(ball_pct_Challenge),
         !is.na(strike_pct_ABS),  !is.na(strike_pct_Challenge),
         !is.na(in_play_pct_ABS), !is.na(in_play_pct_Challenge)) |>
  mutate(
    ball_diff    = ball_pct_ABS    - ball_pct_Challenge,
    strike_diff  = strike_pct_ABS  - strike_pct_Challenge,
    in_play_diff = in_play_pct_ABS - in_play_pct_Challenge
  ) |>
  select(batter, ball_diff, strike_diff, in_play_diff)

# Ridgeline plot of per-batter differences; dashed line at 0 = no difference between treatments
pitch_mix_diff |>
  pivot_longer(cols = c(ball_diff, strike_diff, in_play_diff),
               names_to  = "metric",
               values_to = "diff") |>
  mutate(metric = recode(metric,
                         ball_diff    = "Ball %",
                         strike_diff  = "Strike %",
                         in_play_diff = "In Play %")) |>
  ggplot(aes(x = diff, y = metric, fill = metric)) +
  geom_density_ridges(alpha = 0.5, scale = 1, rel_min_height = 0.01,
                      quantile_lines = TRUE, quantiles = 2) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  scale_fill_manual(values = c("Ball %" = "steelblue",
                               "Strike %" = "tomato",
                               "In Play %" = "seagreen")) +
  scale_y_discrete(expand = expansion(mult = c(0.05, 0.4))) +
  coord_cartesian(clip = "off") +
  labs(title = "Distribution of Pitch Mix Difference by Batter (ABS - Challenge)",
       x     = "Difference (pp)",
       y     = NULL) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5))

# ──────────────────────────────────────────────────────────────────────────────
# ── Player-Month-ABS Dataset ────────────────────────────────────────────────── ----

# Attach calendar month to the at-bat level data so we can group by month
last_pitch_per_ab_month <- last_pitch_per_ab |>
  mutate(month = month(game_date))

# Aggregate to (batter, season, month, abs_type): one row per player per month
# per ABS environment. Counting stats follow the same definitions used above.
# pa = total plate appearances = AB + BB + HBP + SF (denominator of OBP)
# obp = NA when a player has zero plate appearances in a cell
player_month_abs <- last_pitch_per_ab_month |>
  group_by(batter, game_year, month, abs_type) |>
  summarise(
    games_played = n_distinct(game_pk),
    hits         = sum(events %in% c("single", "double", "triple", "home_run"),
                       na.rm = TRUE),
    walks        = sum(events %in% c("walk", "intent_walk"), na.rm = TRUE),
    hbp          = sum(events == "hit_by_pitch", na.rm = TRUE),
    at_bats      = sum(events %in% c("single", "double", "triple", "home_run",
                                      "strikeout", "strikeout_double_play",
                                      "field_out", "force_out", "grounded_into_double_play",
                                      "double_play", "triple_play",
                                      "fielders_choice", "fielders_choice_out",
                                      "field_error"), na.rm = TRUE),
    sac_flies    = sum(events %in% c("sac_fly", "sac_fly_double_play"), na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    pa  = at_bats + walks + hbp + sac_flies,
    obp = if_else(pa > 0, (hits + walks + hbp) / pa, NA_real_)
  )

# Quick coverage check: row count and distribution of PA across cells
cat("=== Player-Month-ABS Dataset Overview ===\n")
cat("Total rows:        ", nrow(player_month_abs), "\n")
cat("Unique batters:    ", n_distinct(player_month_abs$batter), "\n")
cat("Unique months:     ", n_distinct(player_month_abs$month), "\n")
cat("ABS type counts:\n")
player_month_abs |>
  count(abs_type) |>
  print()

# Distribution of plate appearances per cell — informs where to set the PA floor
cat("\nPA quantiles per player-month-ABS cell:\n")
quantile(player_month_abs$pa, probs = c(0, .10, .25, .50, .75, .90, 1)) |>
  round(1) |>
  print()

# Require at least 15 PA in a cell to produce stable OBP estimates;
# report how many rows and batters are dropped by this floor
PA_MIN <- 15L

player_month_abs_filtered <- player_month_abs |>
  filter(pa >= PA_MIN,
         !(game_year == 2024 & month == 3))

cat("\n=== PA Filter Summary (min", PA_MIN, "PA per cell) ===\n")
cat("Rows before: ", nrow(player_month_abs), "\n")
cat("Rows after:  ", nrow(player_month_abs_filtered), "\n")
cat("Rows dropped:", nrow(player_month_abs) - nrow(player_month_abs_filtered), "\n")
cat("Pct dropped: ", round((1 - nrow(player_month_abs_filtered) / nrow(player_month_abs)) * 100, 1), "%\n")
cat("Batters before:", n_distinct(player_month_abs$batter), "\n")
cat("Batters after: ", n_distinct(player_month_abs_filtered$batter), "\n")


# ── OBP Histograms by Year-Month ────────────────────────────────────────────── ----

# Build a labelled year-month factor so facets sort chronologically and display
# as "Apr 2023", "May 2023", etc. rather than raw integers
player_month_abs_filtered <- player_month_abs_filtered |>
  mutate(
    year_month = factor(
      format(as.Date(paste(game_year, month, "01", sep = "-")), "%b %Y"),
      levels = format(
        seq(as.Date("2023-04-01"), as.Date("2024-06-01"), by = "month"),
        "%b %Y"
      )
    )
  )

# Ridgeline plot: each year-month is its own ridge, ABS and Challenge overlaid
# in different colors; median line (quantiles = 2) drawn through each ridge
ggplot(player_month_abs_filtered,
       aes(x = obp, y = fct_rev(year_month), fill = abs_type)) +
  geom_density_ridges(alpha = 0.5, scale = 0.9, rel_min_height = 0.01,
                      quantile_lines = TRUE, quantiles = 2) +
  scale_fill_manual(values = c(ABS = "steelblue", Challenge = "tomato")) +
  labs(
    title = "OBP Distribution by Year-Month and ABS Type",
    x     = "OBP",
    y     = NULL,
    fill  = "ABS Type"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "bottom",
    plot.title      = element_text(hjust = 0.5)
  )


# ── OBP Summary Table by Year-Month and ABS Type ───────────────────────────── ----

# Compute mean, median, SD, min, max of OBP for each year-month × ABS type cell;
# n = number of player-month observations in each cell
obp_month_summary <- player_month_abs_filtered |>
  group_by(year_month, abs_type) |>
  summarise(
    n      = n(),
    mean   = round(mean(obp,   na.rm = TRUE), 4),
    median = round(median(obp, na.rm = TRUE), 4),
    sd     = round(sd(obp,     na.rm = TRUE), 4),
    min    = round(min(obp,    na.rm = TRUE), 4),
    max    = round(max(obp,    na.rm = TRUE), 4),
    .groups = "drop"
  ) |>
  arrange(year_month, abs_type)

# Render as a ggplot table so it displays inline alongside the other plots
obp_month_table_data <- setNames(
  as.data.frame(lapply(obp_month_summary, as.character)),
  names(obp_month_summary)
)

ggplot() +
  theme_void() +
  annotation_custom(
    tableGrob(obp_month_table_data, rows = NULL,
              theme = ttheme_minimal(base_size = 10)),
    xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf
  ) +
  labs(title = "OBP Summary Statistics by Year-Month and ABS Type") +
  theme(plot.title = element_text(hjust = 0.5, size = 12))


# ── Within-Player OBP Difference by Month ──────────────────────────────────── ----

# Pivot wide so each (batter, season, month) row has one OBP column per ABS type,
# then compute the difference (ABS - Challenge); drop player-months where the
# player does not meet the PA floor in both environments, as those produce one-
# sided NAs and cannot be compared
obp_diff_month <- player_month_abs_filtered |>
  select(batter, game_year, month, year_month, abs_type, obp) |>
  pivot_wider(names_from  = abs_type,
              values_from = obp) |>
  filter(!is.na(ABS), !is.na(Challenge)) |>
  mutate(obp_diff = ABS - Challenge) |>
  arrange(game_year, month, batter)

cat("=== Within-Player OBP Difference Dataset ===\n")
cat("Player-months with both environments: ", nrow(obp_diff_month), "\n")
cat("Unique batters:                        ", n_distinct(obp_diff_month$batter), "\n")
print(obp_diff_month)


# ── Distribution of Within-Player OBP Difference (ABS − Challenge) ──────────── ----

# Density plot of obp_diff; dashed line at 0 = no difference between environments;
# mean and median reference lines show whether the distribution is centered on 0
obp_diff_refs <- obp_diff_month |>
  summarise(mean = mean(obp_diff), median = median(obp_diff))

obp_diff_month |>
  ggplot(aes(x = obp_diff)) +
  geom_density(fill = "steelblue", color = "steelblue", alpha = 0.4) +
  geom_vline(xintercept = 0,
             linetype = "dashed", color = "black", linewidth = 0.8) +
  geom_vline(data = tibble(val  = c(obp_diff_refs$mean, obp_diff_refs$median),
                            stat = c("Mean", "Median")),
             aes(xintercept = val, color = stat),
             linewidth = 0.8) +
  scale_color_manual(name = "", values = c(Mean = "red", Median = "orange")) +
  labs(
    title = "Distribution of Within-Player OBP Difference (ABS − Challenge)",
    x     = "OBP Difference (ABS − Challenge)",
    y     = "Density"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    plot.title      = element_text(hjust = 0.5)
  )


# ── OBP Difference Ridgeline by Year-Month ──────────────────────────────────── ----

# Ridgeline of obp_diff per year-month; dashed line at 0 marks no difference;
# median line through each ridge shows whether that month tilts ABS or Challenge
obp_diff_month |>
  ggplot(aes(x = obp_diff, y = fct_rev(year_month))) +
  geom_density_ridges(fill = "steelblue", alpha = 0.5, scale = 0.9,
                      rel_min_height = 0.01,
                      quantile_lines = TRUE, quantiles = 2) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", linewidth = 0.25) +
  labs(
    title = "Within-Player OBP Difference by Month (ABS − Challenge)",
    x     = "OBP Difference (ABS − Challenge)",
    y     = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(hjust = 0.5))


# ── Edge Pitch Classification ───────────────────────────────────────────────── ----

# Constants defining the strike zone boundaries.
# Horizontal: home plate is 17 inches wide, centered at 0 → ±8.5 inches = ±0.708 ft
# Vertical:   sz_top and sz_bot are pitch-level Statcast measurements in feet
# Edge threshold: 2 inches = 2/12 ft from any boundary, inside or outside the zone
EDGE_FT  <- 2 / 12
SZ_LEFT  <- -(17 / 2) / 12
SZ_RIGHT <-  (17 / 2) / 12

# For each pitch, compute distance from pitch center to the nearest point on the
# strike zone perimeter. Two cases:
#   Inside zone:  distance = min of the four distances to each edge
#   Outside zone: Euclidean distance to the nearest corner/edge of the rectangle,
#                 found by clamping (plate_x, plate_z) to the zone rectangle
filtered_edge <- filtered |>
  filter(!is.na(plate_x), !is.na(plate_z), !is.na(sz_top), !is.na(sz_bot)) |>
  mutate(
    abs_type = if_else(wday(game_date, week_start = 1) <= 4, "ABS", "Challenge"),

    in_zone = plate_x >= SZ_LEFT  & plate_x <= SZ_RIGHT &
              plate_z >= sz_bot   & plate_z <= sz_top,

    dist_to_boundary = if_else(
      in_zone,
      # Inside: smallest gap to any of the four edges
      pmin(plate_x - SZ_LEFT,  SZ_RIGHT - plate_x,
           plate_z - sz_bot,   sz_top   - plate_z),
      # Outside: Euclidean distance to nearest point on the rectangle boundary,
      # obtained by clamping the pitch location to the zone box
      sqrt((plate_x - pmax(SZ_LEFT, pmin(SZ_RIGHT, plate_x)))^2 +
           (plate_z - pmax(sz_bot,  pmin(sz_top,   plate_z)))^2)
    ),

    is_edge  = dist_to_boundary <= EDGE_FT,

    # swing = batter offered at the pitch (excludes called balls/strikes, fouls counted)
    is_swing = description %in% c("hit_into_play", "swinging_strike",
                                   "swinging_strike_blocked", "foul",
                                   "foul_tip", "foul_bunt", "missed_bunt")
  )

# Coverage check: pitch counts by zone and edge classification
cat("=== Edge Pitch Classification Summary ===\n")
filtered_edge |>
  count(in_zone, is_edge) |>
  mutate(pct = round(n / sum(n) * 100, 1)) |>
  print()

cat("\nEdge pitches as share of all pitches: ",
    round(mean(filtered_edge$is_edge) * 100, 1), "%\n")
cat("Edge pitches inside zone:             ",
    round(mean(filtered_edge$is_edge[filtered_edge$in_zone])  * 100, 1), "%\n")
cat("Edge pitches outside zone:            ",
    round(mean(filtered_edge$is_edge[!filtered_edge$in_zone]) * 100, 1), "%\n")

# Distinct pitch description values and their frequency — used to validate the
# is_swing classification above
filtered_edge |>
  count(description, sort = TRUE) |>
  print(n = Inf)


# ── Pitch Location Plot by Description ──────────────────────────────────────── ----

# Restrict to edge pitches only, normalize plate_z to each batter's personal zone,
# then sample. Normalization maps sz_bot → 0 and sz_top → 1 so all batters share
# the same vertical reference frame regardless of their individual zone height.
set.seed(42)
filtered_edge_sample <- filtered_edge |>
  filter(is_edge) |>
  mutate(plate_z_norm = (plate_z - sz_bot) / (sz_top - sz_bot)) |>
  slice_sample(n = 25000)

# Normalized zone: bottom = 0, top = 1; horizontal edges unchanged (fixed plate width)
# Edge band in normalized space: EDGE_FT / mean zone height converts inches to zone units
sz_mean_height <- mean(filtered_edge$sz_top - filtered_edge$sz_bot, na.rm = TRUE)
edge_norm <- EDGE_FT / sz_mean_height

# Scatter of edge pitch locations; y-axis is normalized zone position
filtered_edge_sample |>
  ggplot(aes(x = plate_x, y = plate_z_norm, color = description)) +
  # normalized strike zone rectangle
  annotate("rect",
           xmin = SZ_LEFT, xmax = SZ_RIGHT,
           ymin = 0, ymax = 1,
           fill = NA, color = "black", linewidth = 0.8) +
  # 2-inch edge band in normalized units
  annotate("rect",
           xmin = SZ_LEFT  - EDGE_FT, xmax = SZ_RIGHT + EDGE_FT,
           ymin = -edge_norm, ymax = 1 + edge_norm,
           fill = NA, color = "grey50", linewidth = 0.5, linetype = "dashed") +
  geom_point(alpha = 0.3, size = 0.6) +
  coord_fixed() +
  labs(
    title  = "Edge Pitch Locations by Description (normalized zone, sample n = 20,000)",
    x      = "Horizontal Position (ft, catcher's view)",
    y      = "Normalized Zone Position (0 = bottom, 1 = top)",
    color  = "Description"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "right",
    plot.title      = element_text(hjust = 0.5)
  ) +
  guides(color = guide_legend(override.aes = list(alpha = 1, size = 2)))


# ── Pitch Zone Schematic with Batter and Pitcher ─────────────────────────────── ----

# Build a schematic diagram of the catcher's-view strike zone with a geometric
# batter (right-handed, left of plate) and pitcher (small, in the background).
# Coordinates use the same normalized vertical axis as the scatter plot above:
#   y = 0 → bottom of strike zone, y = 1 → top, y < 0 → below zone (plate level)

# Home plate pentagon: 17" wide at front, tapered to a point at the back
# ── Calibration ───────────────────────────────────────────────────────────────
# Drop the image into images/batter.png (or any format magick supports).
# Open it in any image editor, hover over the four strike zone corners, and
# record the pixel coordinates. Fill them in below, then re-run this section.
#
#   ZONE_LEFT_PX  / ZONE_RIGHT_PX : pixel x at the left / right plate edge
#   ZONE_BOT_PX   / ZONE_TOP_PX   : pixel y at the bottom / top of the zone
#                                    (pixel y increases downward in most images)

IMG_PATH     <- here("images", "batter.png")
ZONE_LEFT_PX  <- 323   # ← update after measuring the image
ZONE_RIGHT_PX <- 423   # ← update
ZONE_BOT_PX   <- 355   # ← update  (larger number = lower on screen)
ZONE_TOP_PX   <- 169   # ← update  (smaller number = higher on screen)

# Load image and get dimensions; magick handles PNG/JPG/etc.
batter_img  <- image_read(IMG_PATH)
img_info    <- image_info(batter_img)
img_w       <- img_info$width
img_h       <- img_info$height

# Linear maps: pitch coords → pixel coords
# plate_x  : SZ_LEFT  → ZONE_LEFT_PX,  SZ_RIGHT → ZONE_RIGHT_PX
# plate_z_norm: 0 (bot) → ZONE_BOT_PX, 1 (top)  → ZONE_TOP_PX
# pixel y is inverted relative to ggplot y, so bot pixel > top pixel is expected
px_scale_x  <- (ZONE_RIGHT_PX - ZONE_LEFT_PX)  / (SZ_RIGHT - SZ_LEFT)
px_off_x    <- ZONE_LEFT_PX  - px_scale_x * SZ_LEFT

px_scale_z  <- (ZONE_TOP_PX  - ZONE_BOT_PX)    / (1 - 0)
px_off_z    <- ZONE_BOT_PX

# Transform pitch sample into pixel space; ggplot treats pixel y as increasing
# upward, so we flip by mapping to (img_h - pixel_y) to correct the inversion
filtered_edge_px <- filtered_edge_sample |>
  mutate(
    px_x = px_scale_x * plate_x      + px_off_x,
    px_z = img_h - (px_scale_z * plate_z_norm + px_off_z)
  )

# Zone and edge-band corners transformed to pixel space for annotation lines
zone_px <- list(
  left  = px_scale_x * SZ_LEFT  + px_off_x,
  right = px_scale_x * SZ_RIGHT + px_off_x,
  bot   = img_h - ZONE_BOT_PX,
  top   = img_h - ZONE_TOP_PX,
  edge_left  = px_scale_x * (SZ_LEFT  - EDGE_FT) + px_off_x,
  edge_right = px_scale_x * (SZ_RIGHT + EDGE_FT) + px_off_x,
  edge_bot   = img_h - (px_scale_z * (-edge_norm) + px_off_z),
  edge_top   = img_h - (px_scale_z * (1 + edge_norm) + px_off_z)
)

# ── Edge Pitch Plot Function ─────────────────────────────────────────────────── ----

# Plots edge pitches for a single batter overlaid on the batter image.
# Relies on calibration variables (px_scale_x, px_off_x, px_scale_z, px_off_z,
# img_w, img_h, batter_img, zone_px) computed in the section above.
#
# Arguments:
#   player_id       : MLBAM batter ID (integer)
#   abs_type_filter : "ABS", "Challenge", or NULL (default) for both environments
#   n_sample        : max pitches to plot; all are used if fewer exist (default 5000)
plot_edge_pitches <- function(player_id,
                              abs_type_filter = NULL,
                              n_sample        = 5000) {

  # Descriptions that represent a genuine batter decision:
  #   called ball/strike → batter took the pitch
  #   any swing variant  → batter offered
  # Excluded: automatic_ball / automatic_strike (ABS system, no decision),
  #           pitchout (catcher-side play), intent_ball (intentional walk)
  plate_discipline_desc <- c(
    "ball", "blocked_ball",
    "called_strike",
    "swinging_strike", "swinging_strike_blocked",
    "foul", "foul_tip", "foul_bunt", "missed_bunt",
    "hit_into_play"
  )

  # Filter to player's edge pitches, optionally restricting to one ABS environment
  player_data <- filtered_edge |>
    filter(batter == player_id, is_edge,
           description %in% plate_discipline_desc)

  if (!is.null(abs_type_filter)) {
    player_data <- filter(player_data, abs_type == abs_type_filter)
  }

  if (nrow(player_data) == 0) {
    stop("No edge pitches found for batter ", player_id,
         if (!is.null(abs_type_filter)) paste0(" with abs_type = '", abs_type_filter, "'"))
  }

  # Normalize plate_z to [0, 1] within each batter's personal zone, then sample
  set.seed(42)
  player_px <- player_data |>
    mutate(
      plate_z_norm = (plate_z - sz_bot) / (sz_top - sz_bot),
      px_x         = px_scale_x * plate_x      + px_off_x,
      px_z         = img_h - (px_scale_z * plate_z_norm + px_off_z)
    ) |>
    slice_sample(n = min(n_sample, nrow(player_data)))

  # Look up the player's full name; fall back to the raw ID if not found
  player_name  <- players_dim$full_name[match(player_id, players_dim$player_id)]
  player_label <- if (length(player_name) == 1 && !is.na(player_name)) player_name else as.character(player_id)

  title_str <- paste0(
    "Edge Pitch Locations — ", player_label,
    if (!is.null(abs_type_filter)) paste0("  [", abs_type_filter, "]") else "  [ABS + Challenge]"
  )

  ggplot(player_px, aes(x = px_x, y = px_z, color = description)) +
    annotation_raster(as.raster(batter_img),
                      xmin = 0, xmax = img_w, ymin = 0, ymax = img_h) +
    # semi-transparent fill across the edge band
    annotate("rect",
             xmin = zone_px$edge_left, xmax = zone_px$edge_right,
             ymin = zone_px$edge_bot,  ymax = zone_px$edge_top,
             fill = "grey", alpha = 0.15, color = NA) +
    # outer edge boundary (dashed)
    annotate("rect",
             xmin = zone_px$edge_left, xmax = zone_px$edge_right,
             ymin = zone_px$edge_bot,  ymax = zone_px$edge_top,
             fill = NA, color = "grey", linewidth = 1.0, linetype = "dashed") +
    # inner strike zone boundary
    annotate("rect",
             xmin = zone_px$left, xmax = zone_px$right,
             ymin = zone_px$bot,  ymax = zone_px$top,
             fill = NA, color = "black", linewidth = 1.2) +
    geom_point(alpha = 0.5, size = 1.0) +
    scale_x_continuous(limits = c(0, img_w), expand = c(0, 0)) +
    scale_y_continuous(limits = c(0, img_h), expand = c(0, 0)) +
    coord_fixed() +
    labs(title = title_str, x = NULL, y = NULL, color = "Description") +
    theme_void() +
    theme(plot.title      = element_text(hjust = 0.5, size = 12),
          legend.position = "right") +
    guides(color = guide_legend(override.aes = list(alpha = 1, size = 2)))
}

# Example calls:
# plot_edge_pitches(660644)                        # all environments
# plot_edge_pitches(123456, abs_type_filter = "ABS")
# plot_edge_pitches(123456, abs_type_filter = "Challenge")


# ── Swing Rate Heatmap Function ──────────────────────────────────────────────── ----

# Shows HOW a batter behaves at each pitch location, not where pitches are thrown.
# Each 2D bin is colored by swing rate (swings / pitches seen in that bin), so the
# plot answers: "does this batter swing more or less when the pitch lands here?"
#
# Uses all plate-discipline pitches (not just edge) so the full zone picture is
# visible — high swing rates inside, low outside, transition at the edges.
#
# Bins with fewer than min_pitches observations are rendered as NA (transparent)
# to suppress noisy single-pitch cells at the periphery.
#
# Arguments:
#   player_id       : MLBAM batter ID (integer)
#   abs_type_filter : "ABS", "Challenge", or NULL (default) for both environments
#   bins            : number of bins along each axis for stat_summary_2d (default 12)
#   min_pitches     : minimum pitches in a bin to display a rate; sparser bins → NA (default 2)
plot_swing_rate_heatmap <- function(player_id,
                                    abs_type_filter = NULL,
                                    bins            = 12,
                                    min_pitches     = 2) {

  # Same plate-discipline filter as plot_edge_pitches: only pitches where the
  # batter made a genuine decision (took or swung); excludes ABS auto-calls,
  # pitchouts, and intentional balls
  plate_discipline_desc <- c(
    "ball", "blocked_ball",
    "called_strike",
    "swinging_strike", "swinging_strike_blocked",
    "foul", "foul_tip", "foul_bunt", "missed_bunt",
    "hit_into_play"
  )

  # Pull all plate-discipline pitches for this batter (not restricted to edge);
  # using the full zone gives a meaningful inside-vs-outside contrast
  player_data <- filtered_edge |>
    filter(batter == player_id,
           description %in% plate_discipline_desc)

  if (!is.null(abs_type_filter)) {
    player_data <- filter(player_data, abs_type == abs_type_filter)
  }

  if (nrow(player_data) == 0) {
    stop("No pitches found for batter ", player_id,
         if (!is.null(abs_type_filter)) paste0(" with abs_type = '", abs_type_filter, "'"))
  }

  # Normalize plate_z to the batter's personal zone ([0,1]) then project into the
  # same pixel coordinate system used by the batter background image
  player_px <- player_data |>
    mutate(
      plate_z_norm = (plate_z - sz_bot) / (sz_top - sz_bot),
      px_x         = px_scale_x * plate_x       + px_off_x,
      px_z         = img_h - (px_scale_z * plate_z_norm + px_off_z),
      # cast is_swing to numeric so stat_summary_2d can compute mean() → rate
      swing        = as.numeric(is_swing)
    )

  # Look up the player's full name; fall back to the raw ID if not found
  player_name  <- players_dim$full_name[match(player_id, players_dim$player_id)]
  player_label <- if (length(player_name) == 1 && !is.na(player_name)) player_name else as.character(player_id)

  title_str <- paste0(
    "Swing Rate by Location — ", player_label,
    if (!is.null(abs_type_filter)) paste0("  [", abs_type_filter, "]") else "  [ABS + Challenge]"
  )

  ggplot(player_px, aes(x = px_x, y = px_z, z = swing)) +
    # batter image as background raster
    annotation_raster(as.raster(batter_img),
                      xmin = 0, xmax = img_w, ymin = 0, ymax = img_h) +
    # stat_summary_2d divides the plot area into a bins × bins grid and applies
    # fun to all z values in each cell; mean(swing) = swing rate for that cell.
    # Binwidth is set explicitly from the pitch-occupied pixel region (edge band
    # plus a small margin) rather than the full image canvas, so all bins fall
    # where pitches actually land and none of the bin budget is wasted on empty
    # image area. Cells with fewer than min_pitches return NA → transparent.
    stat_summary_2d(
      fun      = \(z) if (length(z) >= min_pitches) mean(z) else NA_real_,
      binwidth = c(
        (zone_px$edge_right - zone_px$edge_left) * 1.5 / bins,
        (zone_px$edge_bot   - zone_px$edge_top)  * 1.5 / bins
      ),
      alpha    = 0.78
    ) +
    # inner strike zone boundary
    annotate("rect",
             xmin = zone_px$left, xmax = zone_px$right,
             ymin = zone_px$bot,  ymax = zone_px$top,
             fill = NA, color = "black", linewidth = 1.2) +
    # outer edge band (dashed)
    annotate("rect",
             xmin = zone_px$edge_left, xmax = zone_px$edge_right,
             ymin = zone_px$edge_bot,  ymax = zone_px$edge_top,
             fill = NA, color = "grey50", linewidth = 0.5, linetype = "dashed") +
    # diverging scale centred at 0.5: blue = rarely swings, red = almost always swings
    scale_fill_gradient2(
      name     = "Swing Rate",
      low      = "royalblue", mid = "white", high = "firebrick",
      midpoint = 0.5,
      limits   = c(0, 1),
      labels   = scales::percent_format(accuracy = 1),
      na.value = NA
    ) +
    scale_x_continuous(limits = c(0, img_w), expand = c(0, 0)) +
    scale_y_continuous(limits = c(0, img_h), expand = c(0, 0)) +
    coord_fixed() +
    labs(title = title_str, x = NULL, y = NULL) +
    theme_void() +
    theme(plot.title      = element_text(hjust = 0.5, size = 12),
          legend.position = "right")
}

# Example calls:
# plot_swing_rate_heatmap(660644)
# plot_swing_rate_heatmap(123456, abs_type_filter = "ABS")
# plot_swing_rate_heatmap(123456, abs_type_filter = "Challenge", bins = 20, min_pitches = 10)

plot_swing_rate_heatmap(670224, abs_type_filter = "ABS")
plot_swing_rate_heatmap(670224, abs_type_filter = "Challenge")


plot_swing_rate_heatmap(681679)
     681679    

plot_edge_pitches(670224, abs_type_filter = "ABS")
plot_edge_pitches(670224, abs_type_filter = "Challenge")

# ── Most Edge Pitches in filtered_edge_sample ───────────────────────────────── ----

cat("=== Most Edge Pitches in filtered_edge_sample ===\n")
filtered_edge |>
  count(batter, sort = TRUE) |>
  slice_head(n = 1) |>
  print()


# ── Swing Discipline Metrics by Player-Season-Month-ABS Type ────────────────── ----

# Source: filtered_edge, which contains all pitches with valid plate_x, plate_z,
# sz_top, and sz_bot. It already carries:
#   in_zone  : TRUE if the pitch crossed inside the strike zone rectangle
#   is_swing : TRUE if the batter offered at the pitch (any swing event)
#   abs_type : "ABS" or "Challenge" based on day of week

# Three metrics we compute per (batter, game_year, month, abs_type) cell:
#
#   chase_rate   – out-of-zone swing rate
#                  = swings on pitches where in_zone == FALSE
#                    / total pitches where in_zone == FALSE
#                  A batter who chases frequently expands the zone and gives
#                  pitchers an advantage on put-away pitches.
#
#   zone_swing_pct – in-zone swing rate (contact aggressiveness inside the zone)
#                  = swings on pitches where in_zone == TRUE
#                    / total pitches where in_zone == TRUE
#                  Low values signal a patient hitter who lays off even hittable
#                  strikes; very high values can mean a free swinger.
#
#   take_rate    – overall rate at which the batter does NOT swing
#                  = (total pitches − swings) / total pitches
#                  Complements the two zone-split rates and summarises overall
#                  passivity at the plate.

swing_discipline <- filtered_edge |>
  # Add calendar month so we can mirror the granularity of player_month_abs
  #mutate(month = month(game_date)) |>

  # Group to the desired unit of analysis
  group_by(batter, abs_type) |>

  summarise(
    # ── pitch counts that serve as denominators ──────────────────────────────
    # n_pitches: all pitches seen in this cell (used for take_rate denominator)
    n_pitches      = n(),

    # n_inzone / n_outzone: zone-split pitch counts (denominators for the
    # zone-specific rates); using sum() on a logical column counts TRUE rows
    n_inzone       = sum(in_zone,  na.rm = TRUE),
    n_outzone      = sum(!in_zone, na.rm = TRUE),

    # ── swing counts that serve as numerators ────────────────────────────────
    # swings_inzone:  swings that occurred on in-zone pitches
    # swings_outzone: swings that occurred on out-of-zone pitches
    swings_inzone  = sum(is_swing &  in_zone,  na.rm = TRUE),
    swings_outzone = sum(is_swing & !in_zone,  na.rm = TRUE),

    # ── derived rates ────────────────────────────────────────────────────────
    # Chase rate: NA when the batter saw zero out-of-zone pitches in the cell
    chase_rate     = if_else(n_outzone > 0,
                             swings_outzone / n_outzone,
                             NA_real_),

    # Zone swing %: NA when the batter saw zero in-zone pitches in the cell
    zone_swing_pct = if_else(n_inzone  > 0,
                             swings_inzone  / n_inzone,
                             NA_real_),

    # Take rate: total pitches not swung at over all pitches; always defined
    # as long as n_pitches > 0 (guaranteed by the group having at least one row)
    take_rate      = (n_pitches - (swings_inzone + swings_outzone)) / n_pitches,

    .groups = "drop"
  ) |>

  # Round the three rate columns to four decimal places for readability
  mutate(across(c(chase_rate, zone_swing_pct, take_rate),
                \(x) round(x, 4)))

# Coverage check: row count and a glance at the distribution of pitch-count cells
cat("=== Swing Discipline Dataset Overview ===\n")
cat("Total rows:        ", nrow(swing_discipline), "\n")
cat("Unique batters:    ", n_distinct(swing_discipline$batter), "\n")
cat("\nSummary of pitches per cell:\n")
summary(swing_discipline$n_pitches) |> print()

cat("\nSummary of chase_rate:\n")
summary(swing_discipline$chase_rate) |> print()

cat("\nSummary of zone_swing_pct:\n")
summary(swing_discipline$zone_swing_pct) |> print()

cat("\nSummary of take_rate:\n")
summary(swing_discipline$take_rate) |> print()

# Print the full dataset
print(swing_discipline)


# ── Swing Discipline Difference by Player (ABS − Challenge) ─────────────────── ----

# Collapse swing_discipline to the player level first by summing pitch and swing
# counts across all months and seasons within each abs_type. Working from raw
# counts (not averaging rates) avoids weighting bias from cells with very few
# pitches; rates are then recomputed from the aggregated numerators/denominators.
swing_discipline_player <- swing_discipline |>
  group_by(batter, abs_type) |>
  summarise(
    n_pitches      = sum(n_pitches),
    n_inzone       = sum(n_inzone),
    n_outzone      = sum(n_outzone),
    swings_inzone  = sum(swings_inzone),
    swings_outzone = sum(swings_outzone),
    .groups = "drop"
  ) |>
  mutate(
    chase_rate     = if_else(n_outzone > 0, swings_outzone / n_outzone, NA_real_),
    zone_swing_pct = if_else(n_inzone  > 0, swings_inzone  / n_inzone,  NA_real_),
    take_rate      = (n_pitches - (swings_inzone + swings_outzone)) / n_pitches
  )

# Pivot wide so each batter has one column per abs_type for each rate, then
# subtract ABS − Challenge to get the per-player treatment difference.
# Batters missing either condition are dropped (NA in both sides can't be compared).
swing_discipline_diff <- swing_discipline_player |>
  select(batter, abs_type, chase_rate, zone_swing_pct, take_rate) |>
  pivot_wider(
    names_from  = abs_type,
    values_from = c(chase_rate, zone_swing_pct, take_rate)
  ) |>
  filter(
    !is.na(chase_rate_ABS),     !is.na(chase_rate_Challenge),
    !is.na(zone_swing_pct_ABS), !is.na(zone_swing_pct_Challenge),
    !is.na(take_rate_ABS),      !is.na(take_rate_Challenge)
  ) |>
  mutate(
    # Positive diff = higher rate under ABS than under Challenge
    chase_diff      = round(chase_rate_ABS     - chase_rate_Challenge,     4),
    zone_swing_diff = round(zone_swing_pct_ABS - zone_swing_pct_Challenge, 4),
    take_diff       = round(take_rate_ABS      - take_rate_Challenge,      4)
  ) |>
  select(batter, chase_diff, zone_swing_diff, take_diff) |>
  arrange(desc(abs(chase_diff)))

cat("=== Swing Discipline Difference by Player (ABS − Challenge) ===\n")
cat("Players with both environments: ", nrow(swing_discipline_diff), "\n\n")
print(swing_discipline_diff)

swing_discipline_diff <- swing_discipline_diff |>
  mutate(abs_chase_diff  = abs(chase_diff )
         ,abs_zone_swing_diff = abs(zone_swing_diff)
         ,abs_take_diff  = abs(take_diff ) )


swing_discipline_diff |> 
  slice_max(abs_zone_swing_diff, n = 10)


# ── Histograms: Swing Discipline Differences (ABS − Challenge) ──────────────── ----

# Human-readable labels for each diff column, used as facet titles
diff_labels <- c(
  chase_diff      = "Chase Rate Diff",
  zone_swing_diff = "Zone Swing % Diff",
  take_diff       = "Take Rate Diff"
)

# Pivot to long format so all three diffs share a single ggplot call;
# apply human-readable labels immediately so they flow through to the plot
diff_long <- swing_discipline_diff |>
  pivot_longer(cols      = c(chase_diff, zone_swing_diff, take_diff),
               names_to  = "metric",
               values_to = "diff") |>
  mutate(metric = recode(metric, !!!diff_labels))

# Pre-compute summary stats per metric for reference lines and callouts
diff_refs <- diff_long |>
  group_by(metric) |>
  summarise(
    n      = sum(!is.na(diff)),
    mean   = mean(diff,   na.rm = TRUE),
    median = median(diff, na.rm = TRUE),
    sd     = sd(diff,     na.rm = TRUE),
    iqr    = IQR(diff,    na.rm = TRUE),
    min    = min(diff,    na.rm = TRUE),
    max    = max(diff,    na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    # 95% CI on the mean via t-distribution: mean ± t*(0.975, df=n-1) * SE
    # qt() gives the critical value; SE = sd / sqrt(n)
    se    = sd / sqrt(n),
    t_crit = qt(0.975, df = n - 1),
    ci_lo  = mean - t_crit * se,
    ci_hi  = mean + t_crit * se,

    # Build a single multi-line annotation string per facet, placed in the
    # top-right corner via x = Inf / y = Inf with right/top justification
    label = paste0(
      "n:            ", n,                            "\n",
      "Mean:      ", sprintf("%+.4f", mean),          "\n",
      "95% CI:   [", sprintf("%+.4f", ci_lo), ", ",
                     sprintf("%+.4f", ci_hi),  "]",  "\n",
      "Median:  ", sprintf("%+.4f", median),          "\n",
      "SD:          ", sprintf("%.4f",  sd),          "\n",
      "IQR:        ", sprintf("%.4f",  iqr),          "\n",
      "Min:         ", sprintf("%+.4f", min),         "\n",
      "Max:        ", sprintf("%+.4f", max)
    )
  )

diff_long |>
  ggplot(aes(x = diff)) +
  # shaded band spanning the 95% CI of the mean; drawn before the histogram
  # so bars render on top and the band is visible only in the margins
  geom_rect(data = diff_refs,
            aes(xmin = ci_lo, xmax = ci_hi, ymin = -Inf, ymax = Inf),
            fill = "tomato", alpha = 0.15, inherit.aes = FALSE) +
  geom_density(fill = "steelblue", color = "steelblue", alpha = 0.4) +
  # solid black line at 0: no treatment difference
  geom_vline(xintercept = 0,
             linetype = "solid", color = "black", linewidth = 0.6) +
  # mean reference line per facet
  geom_vline(data = diff_refs,
             aes(xintercept = mean, color = "Mean"),
             linewidth = 0.8, linetype = "dashed") +
  # median reference line per facet
  geom_vline(data = diff_refs,
             aes(xintercept = median, color = "Median"),
             linewidth = 0.8, linetype = "dotted") +
  # summary stats callout anchored to top-right corner of each facet panel
  geom_text(data = diff_refs,
            aes(x = Inf, y = Inf, label = label),
            hjust = 1.05, vjust = 1.1,
            size = 4, family = "mono", inherit.aes = FALSE) +
  scale_color_manual(name = "", values = c(Mean = "red", Median = "orange")) +
  coord_cartesian(xlim = c(-0.33, 0.33)) +
  facet_wrap(~ metric, scales = "free_y", ncol = 1) +
  labs(
    title = "Distribution of Swing Discipline Differences by Player (ABS − Challenge)",
    x     = "Difference (ABS − Challenge)",
    y     = "Density"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    plot.title      = element_text(hjust = 0.5)
  )
