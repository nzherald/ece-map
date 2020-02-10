

school_details <- function(d) {
  url <- here(glue("data/profiles/{d$schoolId}.html"))
  print(d$name)
  details <- read_html(url) %>% html_node(".profile-details")
  t <- details %>% html_nodes(".title") %>% html_text() %>% str_remove(':')
  tibble(schoolId=d$schoolId, detail=t,
    value=details %>% html_nodes('dd') %>% html_text() %>% str_trim()
  )
}

get_ero_reports <- function(d) {
  url <- glue("https://www.ero.govt.nz/report-view?id={d$schoolId}")
  print(d)
  reports <- read_html(url) %>% html_nodes('article') %>% html_node('a')
  dates <- reports %>% html_text() %>% parse_date_time('d/m/Y')
  urls <- reports %>% html_attr('href')
  tibble(schoolId=d$schoolId, name=d$name,
    date=dates,
    link=urls
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
    processx::run("/Users/chris.knox/.local/bin/stack", c("exec", "ece-map", "--", file_in(details_file)))
    details
}

download_plan <- drake_plan(
    ece_locations = fromJSON("https://www.educationcounts.govt.nz/js-content/ece-geo-data.json?v=0.1.5")$schools,
    write_ece_locations = download_details(ece_locations, here("data/ece_locations.csv")),
    read_details = target(school_details(write_ece_locations), dynamic = map(write_ece_locations)),
    # ece_reports = target(get_ero_reports(ece_locations), dynamic = map(ece_locations)),
    #ece_rating = target(get_ero_rating(ece_reports), dynamic = map(ece_reports))
  )
