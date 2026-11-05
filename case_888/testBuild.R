library(tidyverse)

tile_sample <-
  str_c(
    "https://www.dropbox.com/scl/fi/kospnc09qscm43inp0jgk/",
    "hfc.rds?rlkey=a14kkpiha47hhjuyvn4ffswnj&dl=1"
  ) %>%
  read_rds()

tile_sample <-
  str_c(
"https://www.dropbox.com/scl/fi/kospnc09qscm43inp0jgk/",
"hfc.rds?rlkey=a14kkpiha47hhjuyvn4ffswnj&dl=1"))
  ) %>%
  read_rds() 


tile_sample_non_idiot <-
  tile_sample %>%
  pivot_longer(
    cols = starts_with("store_"),
    names_to = "raw_name",
    values_to = "value"
  ) %>%
  mutate(
    measure = str_extract(raw_name, "(inv|order|ship)$"),
    store_id = raw_name %>%
      str_remove("^store_") %>%
      str_remove("_(inv|order|ship)$")
  ) %>%
  select(-raw_name) %>%
  pivot_wider(
    names_from = measure,
    values_from = value
  ) %>%
  mutate(
    inv = as.numeric(inv),
    order = as.numeric(order),
    ship = as.numeric(ship)
  )

tile_sample_non_idiot %>% head()