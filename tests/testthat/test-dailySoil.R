#devtools::load_all()

testthat::test_that("daily soil routine",
{
  model <- readRDS("example_basin.rds") # ./tests/testthat/example_basin.rds
  initModel(model)

  dailyEffPrec <- rep(5, model$array_size)
  immediate_runoff <- rep(0, model$array_size)
  dailySoilPET <- rep(0, model$array_size)
  dailyCanopyEvapo <- rep(0, model$array_size)
  dailySnowEvapo <- rep(0, model$array_size)

  G_soilWaterContent <- rep(50, model$array_size)
  dailyAET <- rep(0, model$array_size)
  daily_runoff <- rep(0, model$array_size)
  soil_water_overflow <- rep(0, model$array_size)
  
  dailySoil(
    dailyEffPrec,
    immediate_runoff,
    dailySoilPET,
    dailyCanopyEvapo,
    dailySnowEvapo,
    G_soilWaterContent,
    dailyAET,
    daily_runoff,
    soil_water_overflow
  )

  expect_equal(sum(daily_runoff), 2.58414754)

})


testthat::test_that("daily soil routine - lower threshold",
{ 
  model <- readRDS("example_basin.rds") # ./tests/testthat/example_basin.rds
  model$lower_threshold_soil <- 50
  initModel(model)
  
  dailyEffPrec <- rep(5, model$array_size)
  immediate_runoff <- rep(0, model$array_size)
  dailySoilPET <- rep(0, model$array_size)
  dailyCanopyEvapo <- rep(0, model$array_size)
  dailySnowEvapo <- rep(0, model$array_size)
  
  G_soilWaterContent <- rep(0, model$array_size)
  dailyAET <- rep(0, model$array_size)
  daily_runoff <- rep(0, model$array_size)
  soil_water_overflow <- rep(0, model$array_size)
  
  dailySoil(
    dailyEffPrec,
    immediate_runoff,
    dailySoilPET,
    dailyCanopyEvapo,
    dailySnowEvapo,
    G_soilWaterContent,
    dailyAET,
    daily_runoff,
    soil_water_overflow
  )
  expect_equal(sum(daily_runoff), 0)
})
