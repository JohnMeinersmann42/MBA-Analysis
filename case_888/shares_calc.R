library(tidyverse)

cookie_cats <- read_rds(str_c(
  "https://www.dropbox.com/scl/fi/ijrsa2j6h6yim0v3el4kx/",
  "cookie_cats.rds?rlkey=ahppbjinihbhszodv366x59vi&dl=1"
))

sex_shares <- cookie_cats %>%
  count(version, sex) %>%
  group_by(version) %>%
  mutate(share = round(n / sum(n), 4)) %>%
  ungroup()

device_shares <- cookie_cats %>%
  count(version, device) %>%
  group_by(version) %>%
  mutate(share = round(n / sum(n), 4)) %>%
  ungroup()

cat("=== Sex Shares by Version ===\n")
print(sex_shares)
cat("\n=== Device Shares by Version ===\n")
print(device_shares)
