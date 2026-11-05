# ============================================================
# Write your name
# ============================================================

# Name: John Meinersmann


# ============================================================
# Setup: Load libraries and data
# ============================================================

library(tidyverse)

court_data <-
  str_c(
    "https://www.dropbox.com/scl/fi/fpflbmmij98dcddtny9uz/",
    "judges_data.csv?rlkey=pk4h7c2apcurk5j5stgs700iu&dl=1"
  ) |>
  read_csv()


# ============================================================
# Setup: Completed code from Exercises 1-9
# ============================================================
#
# In Part 1 (hw_italian_judges_1.R), you modified court_data three times.
# Because R does not save changes between sessions, those modifications are
# gone the moment you close R. So before you can pick up where you left off,
# you need to re-apply those same three changes to court_data here.
#
# The three modifications were:
#
#   Exercise 1  — added a month_year column
#   Exercise 3  — added a treated column
#   Exercise 5  — removed the two injunction case types
#
# (The other exercises in Part 1 were checks and counts that produced output
# but did not change court_data, so there is nothing to re-run for them.)
#
# Run this entire setup block before starting Exercise 10.
# ============================================================

# Exercise 1: Add a month_year column (first day of each hearing's month)
court_data <-
  court_data |>
  mutate(month_year = floor_date(hdate, "month"))

# Exercise 3: Add a treated column (TRUE when judge belongs to Collegio 5)
court_data <-
  court_data |>
  mutate(treated = collegio == 5)

# Exercise 5: Remove injunction cases — these follow a different scheduling
# system and should not be mixed with ordinary appeals
court_data <-
  court_data |>
  filter(!ctype %in% c("Ingiunzione lavoro", "Ingiunzione previdenza"))

# Note: Exercises 7-9 revealed a bug — the total number of distinct judges
# is less than the sum of treated + control judges, which means some judges
# appear in both groups. Exercise 10 is where you fix that bug.


# ============================================================
# Exercise 10
# ============================================================

# English code: Take court_data and remove any judges who appear under both
# treated = TRUE and treated = FALSE hearings. Group the data .by = jid and
# keep only judges for whom n_distinct(treated) equals 1, meaning all of their
# hearings belong to a single group (either fully treated or fully control).
# Save the result back to court_data.

court_data <-
  court_data |>
  filter(
    n_distinct(treated) == 1,
    .by = jid
  )


# ============================================================
# Exercise 11
# ============================================================

# English code: 
# Going forward, use the .by syntax for grouping and use the |> syntax instead of %>% where they apply in coding
# Take court_data and summarize the count of distinct treated values by jid  in a field n_distinct
# then filter down to the jid's that have an n_distinct value that is not 1. 
#
#

# R code:
court_data |>
  summarize(
    n_distinct = n_distinct(treated),
    .by = jid
  )|>
  filter(
    n_distinct != 1
  )

# ============================================================
# Exercise 12
# ============================================================

# English code:
# Make a tibble called case_filings using court_data that by cid, summarizes its min month_year value into a 
# field called first_month and a field closed that captures any(closed)
#
#

# R code:
case_filings <- court_data |>
  summarize(
    first_month = min(month_year),
    closed = any(closed),
    .by = cid
  )

# ============================================================
# Exercise 13
# ============================================================

# English code:
# Using case_filings, plot a table that summarizes by first_month the mean of closed records. 
# Have the x axis be the first_month and the y be the mean of closed records.
# Make the graph a scatter plot.
#

# R code:
case_filings |>
  summarize(mean_closed = mean(closed), .by = first_month) |>
  ggplot(aes(x = first_month, y = mean_closed)) +
  geom_point()

# ============================================================
# Exercise 14
# ============================================================

# English code:
# Rebuild the case_filings tibble with the first_month field being the min value of month_year, closed field being any(closed), 
# and the max_hdate field being the max(hdate). group by the cid field. finally, filter for items where the max_hdate is greater 
# than the variable cutoff_date, which is ymd("2024-4-30") - years(2), or the record has a closed value of true.
# filter the court_data to only records that have cid's in court_data and case_filings

# R code:
cutoff_date <- ymd("2024-4-30") - years(2)

case_filings <- 
  court_data |>
  summarize(
    first_month = min(month_year),
    closed = any(closed),
    max_hdate = max(hdate),
    .by = cid
  )|>
  filter(
    max_hdate > cutoff_date | closed
  )

court_data <- court_data |>
  filter(cid %in% case_filings$cid)

# ============================================================
# Exercise 15
# ============================================================

# English code:
# Using the definition of court_data earlier, print out all the cases with max(hdate) < cutoff_date and filter also that closed is FALSE
#
#

# R code:
cutoff_date <- ymd("2024-4-30") - years(2)

court_data |>
  summarize(
    first_month = min(month_year),
    closed = any(closed),
    max_hdate = max(hdate),
    .by = cid
  )|>
  filter(max_hdate < cutoff_date & !closed)

# ============================================================
# Exercise 16
# ============================================================

# English code:
# Add a field to court_data called last_hearing that is true when the hid for a cid is the max value and closed is TRUE and false otherwise.
#
#

# R code:
court_data <- court_data |>
  mutate(last_hearing = hid == max(hid) & closed, .by = cid)

# ============================================================
# Exercise 17
# ============================================================

# English code:
# using court_data, build a validation printout that that every cid that has closed =TRUE has just one last_hearing =TRUE and every open case has no last_hearing = TRUE
#
#

# R code:
court_data |>
  summarize(
    closed = any(closed),
    n_last_hearing = sum(last_hearing),
    .by = cid
  ) |>
  filter(
    (closed & n_last_hearing != 1) | (!closed & n_last_hearing != 0)
  ) #shows zero rows - success!



# ============================================================
# Exercise 18
# ============================================================

# English code:
# Using court_data, create a tibble called hazard_rates that summarizes the mean of last_hearing in a field called hazard_rate. Group the summary by treated and month_year
#
#

# R code:

hazard_rates <- court_data |>
  summarize(
    hazard_rate = mean(last_hearing),
    .by = c(treated, month_year)
  )

# ============================================================
# Exercise 19
# ============================================================

# English code:
# filter out from hazard_rates tibble the records with hazard_rate <.05
#
#

# R code:
hazard_rates <- hazard_rates |>
  filter(hazard_rate >= .05)

# ============================================================
# Exercise 20
# ============================================================

# English code:
# show me the min hazard rate in hazard_rates
#
#

# R code:
min(hazard_rates$hazard_rate)

# ============================================================
# Exercise 21
# ============================================================

# English code:
# Add a column called post_period to hazard_rates that is TRUE if and only if the month_year is 2021-01-01 or later
# create a tibble called post_period_data filtering where post_period = TRUE
# create the scatterplot by running hazard_rate_plot + post_period_data.

# R code:


hazard_rates <- hazard_rates |>
  mutate(post_period = month_year >= ymd("2021-01-01"))

post_period_data <- hazard_rates |>
  filter(post_period)


hazard_rate_plot <-
ggplot() +
aes(
x = month_year,
y = hazard_rate,
color = treated,
linetype = post_period
) +
geom_point() +
geom_smooth(method = "lm", formula = y ~ x) +
theme_minimal()

hazard_rate_plot %+% post_period_data

# ============================================================
# Exercise 22
# ============================================================

# English code:
# create the scatterplot by running hazard_rate_plot + hazard_rates
#
#

# R code:
hazard_rate_plot %+% hazard_rates

# ============================================================
# Exercise 23
# ============================================================

# This exercise asks for a short written response, not R code. Please write
# your answer as a comment below. Make sure to explain the
# difference-in-differences idea, interpret the plot, and give a
# recommendation.

# Response:
# This figure illustrates the proportion of hearings that close a case both before and after the intervention of moving to case-level FIFO.
# The grapgh shows that a difference in difference of the treated (case-level FIFO) and control groups (hearing-level FIFO). 
# A difference-in-differences (DiD) graph plots outcomes over time for two groups — a treated group and a control group — separated by a treatment cutoff date.
# The solid line for each group shows the graphing of hazard rate before intervention and the dashed after intervention. The graph here shows significant deviation of 
# the hazard rate change from pre to post iuntervention for treated items vs non-treated shows a significant deviation.    
