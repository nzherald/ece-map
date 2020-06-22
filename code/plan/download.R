

school_details <- function(d) {
  url <- here(glue("data/profiles/{d$schoolId}.html"))
  print(d$name)
  details <- read_html(url) %>% html_node(".profile-details")
  t <- details %>% html_nodes(".title") %>% html_text() %>% str_remove(':')
  tibble(schoolId=d$schoolId, detail=t,
    value=details %>% html_nodes('dd') %>% html_text() %>% str_trim()
  )
}


get_ero_rating <- function(d) {
  print(d$name)
  url <- glue("https://www.ero.govt.nz/{d$link}")
  txt <- read_html(url) %>% html_nodes('table') %>% html_nodes('b') %>% html_text()
  tibble(schoolId=d$schoolId, name=d$name,
    text=txt)
}


download_reports <- function(details, details_file) {
    processx::run("/Users/chris.knox/.local/bin/stack", c("exec", "ece-map", "--", "details", "-f", file_in(details_file)))
    details
}

download_plan <- drake_plan(
    ece_directory = read_csv(here("data/Directory-ECE-Current.csv"), skip=10,
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
    )) %>% rename_all(make.names),
    ece_locations = ece_directory %>% 
      select(schoolId=Number,name=Name,latitude=Latitude,longitude=Longitude),
    ece_locations_write = write_csv(ece_locations, file_out(here("data/ece_locations.csv")), na=""),
    ece_reports = processx::run("/Users/chris.knox/.local/bin/stack",
      c("exec", "ece-map", "--", "ero", "-f", file_in(here("data/ece_locations.csv")))),
    ece = ece_directory %>% 
      filter(!is.na(Latitude), !is.na(Longitude)) %>% 
      st_as_sf(coords = c("Longitude", "Latitude")) %>%
      st_write("PG:dbname=ece", "ece", layer_options="OVERWRITE=true") %>%
      fastdigest()
      
    #ece_rating = target(get_ero_rating(ece_reports), dynamic = map(ece_reports))
  )
