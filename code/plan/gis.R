
gis_plan <- drake_plan(
  sau2 = st_read(file_in(here("data/gis/statistical-area-2-2018-clipped-generalised.shp"))),
  sau2db = fastdigest(st_write(sau2, "PG:dbname=ece", "sau2", layer_options = "OVERWRITE=true"))
  )
