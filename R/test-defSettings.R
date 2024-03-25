#devtools::load_all()

testthat::test_that("defSetting_bad",
{
  settings <- c(0,1,0,1,0)
  testthat::expect_error(WaterGAPLite:::defSettings(setting))

})

# testthat::test_that("defSetting_good",
# {
#   settings <- c(0,1,0,1,0,0,0,0,0,0)
#   testthat::expect_no_error(WaterGAPLite:::defSettings(settings))
# 
# })
