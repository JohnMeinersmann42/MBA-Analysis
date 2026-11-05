# Name:       John Meinersmann
# Homework:   hw_italian_judges_1
# Class:      AIML 451
# Date:       2026-04-13

# Reading in packages
library(tidyverse)

#Read in Data from dropbox source online
  court_data <-
                str_c(
                "https://www.dropbox.com/scl/fi/fpflbmmij98dcddtny9uz/",
                "judges_data.csv?rlkey=pk4h7c2apcurk5j5stgs700iu&dl=1"
                ) |>
                read_csv()

#Print Metadata and see values in dataset
  court_data |> glimpse()

#Exercise 1 - Add month_year to court_data

  court_data <- court_data |>
                mutate(month_year = floor_date(hdate, unit = "month"))

# Exercise 2 - look at random sample of data

  court_data |> slice_sample(n =10)

# Exercise 3 - Adding a treated column

  court_data <- court_data |>
                mutate(treated = collegio == 5)

# Exercise 4 - filtering for treated and nontreated and confirming distinct collegio listing. 
  
  # treated
    court_data |>
      filter(treated) |>
      distinct(collegio) # returns tibble with value 5
  
  # not treated 
    court_data |>
      filter(!treated) |>
      distinct(collegio) # returns tibble with values 1,2,3,4,6,7,8

# Exercise 5 - filter out irrelevant data.

  # filter out "Ingiunzione lavoro" and "Ingiunzione previdenza"
    court_data <- court_data |>
      filter(!ctype %in% c("Ingiunzione lavoro","Ingiunzione previdenza"))

# Exercise 6 - Double check values are removed from court_data
    court_data |>
      filter(ctype %in% c("Ingiunzione lavoro","Ingiunzione previdenza")) # No records shown

# Exercise 7 - treated count
    #TREATED
    court_data |>
      filter(treated) |>
      summarise(n_distinct(jid)) # 10 judges

# Exercise 8 - Not treated count
    #Not treated
    court_data |>
      filter(!treated) |>
      summarise(n_distinct(jid)) # 58 judges

# Exercise 9 - All judges
    #Total
    court_data |>
      summarise(n_distinct(jid)) # 65 - shows an overlap of 3 judges
