library(tidyverse)
library(r2d3)
library(jsonlite)
library(forcats)


rank <- read_csv("data/ranked-schools.csv")
schools <- read_csv("data/Directory-ECE-Current.clean.csv",
                    col_types = cols(
                      .default = col_character(),
                      number = col_character(),
                      community_of_learning_id = col_double(),
                      longitude = col_double(),
                      latitude = col_double(),
                      all_children = col_double(),
                      under_2s = col_double(),
                      age_0 = col_double(),
                      age_1 = col_double(),
                      age_2 = col_double(),
                      age_3 = col_double(),
                      age_4 = col_double(),
                      age_5 = col_double(),
                      total_roll = col_double(),
                      maori = col_double(),
                      pacific = col_double(),
                      asian = col_double(),
                      european_pakeha = col_double(),
                      other = col_double()
                    ))
rated <- schools %>%
  left_join(rank %>%
  arrange(desc(rankedDate)) %>%
  group_by(rankedId) %>%
  mutate(row = row_number()) %>%
  ungroup() %>%
  filter(row == 1) %>%
  select(-row) %>%
  mutate(number=as.character(rankedId)), by=c("number"))

sorted <- rated %>%
  filter(institution_type != "Casual-Education and Care", 
         institution_type != "Hospital Based") %>%
  mutate(type = case_when(
    institution_type == "Education and Care Service" & authority == "Community based" ~ "Education and Care\nCommunity based",
    institution_type == "Education and Care Service" & authority == "Privately owned" ~ "Education and Care\nPrivately owned",
    TRUE ~ institution_type
  ),
  rank=as_factor(coalesce(str_to_sentence(rankedRank), "Report unavailable")),
  rank=fct_relevel(rank, c(
    "Very well placed",
    "Well placed",
    "Needs further development",
    "Not well placed",
    "No rating",
    "Report unavailable"
  )),
  ero = coalesce(as.numeric(as.factor(rank)),6)
  )

geodata <- sorted %>%
  select(number, ero, latitude, longitude, total_roll, type) %>%
  filter(!is.na(latitude), !is.na(longitude)) %>%
  mutate(type = as.numeric(as.factor(type)) ) %>%
  sf::st_as_sf(coords = c("longitude","latitude"))

geodata %>% sf::st_write("interactive/src/assets/ece.geojson", delete_dsn=T)

sorted %>%
  mutate(color = as.numeric(as.factor(type)) ) %>%
  count(y=type, color, name="x") %>%
  arrange(desc(x)) %>%
  jsonlite::write_json("interactive/src/types.json", auto_unbox=T)

sorted %>%
  count(y=rank, color=ero, name="x") %>%
  arrange(desc(color)) %>%
  jsonlite::write_json("interactive/src/rating.json", auto_unbox=T)

sorted %>%
  mutate(definition=if_else(definition=="Not Applicable", NA_character_,definition),
          type_idx = as.numeric(as.factor(type)),
         rankedDate = format(as.Date(rankedDate), "%d %B, %Y")
         ) %>%
  select(number, name, type=institution_type, type_idx, authority, street,
         suburb, town_city, definition, rating=rank, erolink=rankedUrl,
         erodate=rankedDate, total_roll, under_2s
         ) %>%
  split(.$number) %>%
  map(unbox) %>%
  as_d3_data() %>%
  write_json("interactive/src/assets/details.json", auto_unbox=T)
  
