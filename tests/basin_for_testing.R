
# settings <-  WaterGAPLite::init.settings()
# start <- "01.01.1980"
# end <- "31.12.1982"
# root <- r"(C:\Users\jenny\MyProject_sciebo\WaterGAPlite)"
#
# example_basin <- WaterGAPLite::init.model(
#   grdc_number = 6340600,
#   lat = 51.54166,
#   long = 12.62499,
#   cont = "eu",
#   root)
#
# climate <- WaterGAPLite::init.climate(
#     basin_object = example_basin,
#     start = start,
#     end = end,
#     force2read = TRUE,
#     climate_format = "global")
#
# water_use <- WaterGAPLite::init.wateruse(
#     basin_object = example_basin,
#     sim_start = start,
#     sim_end = end,
#     wateruse_setting = settings[1])
#
# model <- WaterGAPLite::basin.prepare_run(
#     basin_object = example_basin,
#     climate_object = climate,
#     wateruse_object = water_use)
#
# saveRDS(model, "./tests/testthat/example_basin.rds")


