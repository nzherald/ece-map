source(here("code/plan/download.R"))
source(here("code/plan/gis.R"))




pages_plan <- drake_plan(
  index_wfr = wflow_publish(
    files=c(
      knitr_in(here("analysis/index.Rmd")),
      file_in(here("analysis/_site.yml"))), view=F, verbose=T)

)

plan <- bind_plans(
  download_plan,
  gis_plan,
  pages_plan
)
