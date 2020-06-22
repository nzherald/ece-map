
data_plan <- drake_plan(
  eceDirectory = read_csv(file_in(here("data/Directory-ECE-Current.csv", skip=10))) %>%
    filter(!is.na(Number))
)
