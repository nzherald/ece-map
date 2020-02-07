source(here::here("code/packages.R"))
# source("code/functions.R")
source(here("code/plan.R"))
options(clustermq.scheduler = "multicore") # optional parallel computing

drake_config(plan, verbose = 2)
