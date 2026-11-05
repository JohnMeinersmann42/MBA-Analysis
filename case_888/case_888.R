library(tidyverse)
tile_sample <-
str_c(
"https://www.dropbox.com/scl/fi/46nc6ft8wz27ei8xysurp/",
"tile_sample.rds?rlkey=njr08gogjmvodtg37vf2lwyv3&dl=1"
) %>%
read_rds

tile_sample %>%head
