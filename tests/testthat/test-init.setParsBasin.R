#devtools::load_all()

test_that("seting pars for basin", {
  
  init.classes()
  
  NUMBER_OF_CELLS <- 4
  
  basin_object <- new("Basin",

                      GLCT = rep(1, NUMBER_OF_CELLS),
                      G_ARID_HUMID = rep(1, NUMBER_OF_CELLS),
                      G_BATJES = rep(10, NUMBER_OF_CELLS),
                      
                      G_TEXTURE = rep(15, NUMBER_OF_CELLS),
                      G_SLOPE_CLASS = rep(25, NUMBER_OF_CELLS),
                      G_PERMAGLAC = rep(0, NUMBER_OF_CELLS), 
                      G_AQ_FACTOR = rep(0, NUMBER_OF_CELLS),
                      
                      G_GLOLAK = rep(0, NUMBER_OF_CELLS),
                      G_GLOWET = rep(0, NUMBER_OF_CELLS),
                      G_LOCLAK = rep(0, NUMBER_OF_CELLS),
                      G_LOCWET = rep(0, NUMBER_OF_CELLS))
  
  lai_info <- matrix(1, nrow=18, ncol=7)
  lct_info <- matrix(1, nrow=18, ncol=6)
    
  partially_initialized_basin <- 
    init.set_pars_basin(
      basin_object, lai_info, lct_info
  ) 
  
  expect_equal(partially_initialized_basin@landfrac, rep(1, NUMBER_OF_CELLS))
  expect_equal(partially_initialized_basin@G_gwFactor, rep(0, NUMBER_OF_CELLS))
  expect_equal(partially_initialized_basin@G_Smax, rep(10, NUMBER_OF_CELLS))
  
  expect_equal(partially_initialized_basin@alphaPT, rep(1.74, 4))
  expect_equal(partially_initialized_basin@maxDailyPET, rep(10, 4))
  
  expect_equal(partially_initialized_basin@Splitfactor, rep(1, 4))
  
  expect_equal(partially_initialized_basin@LAI_max, rep(1, 4))
  expect_equal(partially_initialized_basin@initDays, rep(1, 4))
  expect_equal(partially_initialized_basin@LAI_min, rep(0.1, 4))
  
  expect_equal(partially_initialized_basin@degreeDayFactor, rep(1, 4))
  expect_equal(partially_initialized_basin@albedoSnow, rep(1, 4))
  expect_equal(partially_initialized_basin@albedo, rep(1, 4))
  expect_equal(partially_initialized_basin@emissivity, rep(1, 4))
  expect_equal(partially_initialized_basin@rootingDepth, rep(1, 4))
  
})
