# to run the test interactively please run the command below and then go
# inside the test and run it line by line
# devtools::load_all()

# to run (potentially all) test automatically use
# devtools::test()

testthat::test_that("test-defSettings in initModel.cpp",
{ 
  good_settings <- rep(0, 8)
  defSettings(good_settings)
  
  bad_settings <- rep(0, 9)
  testthat::expect_error(defSettings(bad_settings),
                         "Settings should be a vector of length 8")
  
  error_list <- c(
    "WaterUseType should be 0, 1 or 2",
    "WaterUseAllocationType should be 0, 1 or 2",
    "flowVelocityType should be 0 or 1",
    "GapYearType should be 0 or 1",
    "ReservoirType should be 0 or 1",
    "splitttingFactor parameter should be 0 or 1",
    "calculation LongWave parameter should be 0 or 1",
    "useSystemVals should be 0, 1, 2 or 3"
  )
  
  for (bad_setting in 1:length(error_list)){
    bad_settings <- good_settings
    bad_settings[bad_setting] <- -1
    testthat::expect_error(defSettings(bad_settings),
                           error_list[bad_setting])
  }

})