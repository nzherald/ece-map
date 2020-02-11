

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


download_details <- function(details, details_file) {
    write_csv(details, file_out(details_file))
    processx::run("/Users/chris.knox/.local/bin/stack", c("exec", "ece-map", "--", "details", "-f", file_in(details_file)))
    details
}

download_plan <- drake_plan(
    ece_locations = fromJSON("https://www.educationcounts.govt.nz/js-content/ece-geo-data.json?v=0.1.5")$schools,
    write_ece_locations = download_details(ece_locations, here("data/ece_locations.csv")),
    ece_details = target(school_details(write_ece_locations), dynamic = map(write_ece_locations)),
    ece = ece_details %>% 
      filter(detail %in% c("Maximum children", "Maximum under 2 year olds")) %>%
      pivot_wider(names_from = detail, values_from = value) %>%
      inner_join(ece_locations, by="schoolId") %>%
      st_as_sf(coords = c("longitude", "latitude")) %>%
      st_write("PG:dbname=ece", "ece", layer_options="OVERWRITE=true") %>%
      fastdigest()
      
    #ece_rating = target(get_ero_rating(ece_reports), dynamic = map(ece_reports))
  )
