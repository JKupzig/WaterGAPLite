#devtools::load_all()

testthat::test_that("estimate longwave simple",
{
   model <- readRDS("example_basin.rds") # ./tests/testthat/example_basin.rds
   initModel(model)

   longwave <- dailyEstimateLongwave(
       n = 1, DOY = 100, dailyTempC = 15, dailyShortWave = 10000)

   expect_equal(longwave, -2.7319262)
})
