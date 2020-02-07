source(here("code/plan/download.R"))




pages_plan <- drake_plan(
  site_wfr = file_in(here("analysis/_site.yml")),
  index_wfr = wflow_publish(knitr_in(here("analysis/index.Rmd")), view=F, verbose=T)

)

plan <- bind_plans(
  download_plan,
  pages_plan
)
