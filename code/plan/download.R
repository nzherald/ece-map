

download_school <- function(d) {
  url <- glue("https://www.educationcounts.govt.nz/find-an-els/els/profile-and-contact-details?ece={d$schoolId}")
  print(d$name)
  details <- read_html(url) %>% html_node(".profile-details")
  t <- details %>% html_nodes(".title") %>% html_text() %>% str_remove(':')
  tibble(schoolId=d$schoolId, name=d$name, detail=t,
    value=details %>% html_nodes('dd') %>% html_text() %>% str_trim()
  )
}

download_plan <- drake_plan(
    ece_locations = fromJSON("https://www.educationcounts.govt.nz/js-content/ece-geo-data.json?v=0.1.5")$schools,
    fetch_details = target(download_school(ece_locations), dynamic = map(ece_locations))
  )
