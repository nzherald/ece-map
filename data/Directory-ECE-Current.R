suppressPackageStartupMessages(library(tidyverse))

schools <- read_csv(here::here("data/Directory-ECE-Current.csv"), skip=10,
      locale = locale(encoding = 'ISO-8859-1'),
      col_types = cols(
        .default = col_character(),
        `Community of Learning: ID` = col_double(),
        Longitude = col_double(),
        Latitude = col_double(),
        `All Children` = col_double(),
        `Under 2's` = col_double(),
        `Age 0` = col_double(),
        `Age 1` = col_double(),
        `Age 2` = col_double(),
        `Age 3` = col_double(),
        `Age 4` = col_double(),
        `Age 5` = col_double(),
        `Total Roll` = col_double(),
        Maori = col_double(),
        Pacific = col_double(),
        `Asian*` = col_double(),
        `European Pakeha*` = col_double(),
        `Other*` = col_double()
    )) %>%
  janitor::clean_names() 

schools %>%
  write_csv(here::here("data/Directory-ECE-Current.clean.csv"), na="")

schools %>%
  sample_n(10) %>%
  write_csv(here::here("data/Directory-ECE-Current.clean.sub.csv"))
