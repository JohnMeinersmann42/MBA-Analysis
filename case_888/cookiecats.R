library(tidyverse)

cookie_cats <-
  str_c(
    "https://www.dropbox.com/scl/fi/ijrsa2j6h6yim0v3el4kx/",
    "cookie_cats.rds?rlkey=ahppbjinihbhszodv366x59vi&dl=1"
  ) %>%
  read_rds()