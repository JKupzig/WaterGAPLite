devtools::load_all()

testthat::test_that("estimate shortwave simple",
{

  ARRAY_SIZE <- 10
  NDAYS <- 4
  TEMPERATURE <- 10
  SUNSHINE_HOURS <- 5
  
  
  SimDates <- seq(as.Date("2010-1-1"), 
                  as.Date(sprintf("2010-1-%i", NDAYS)), 
                  by = "days")
  
  TempC <- matrix(TEMPERATURE, nrow = ARRAY_SIZE, ncol = NDAYS)
  Sunshine <- matrix(SUNSHINE_HOURS, nrow = ARRAY_SIZE, ncol = NDAYS)
  GR <- seq(1000 ,1000 + NDAYS, 1)
  cor_row <- 1
  
  shortwave <- dailyEstimateShortwave(SimDates, 
                                      TempC, 
                                      Sunshine, 
                                      GR, 
                                      cor_row)
  

  expect_equal(sum(shortwave), 7125.8114)
  expect_equal(nrow(shortwave), ARRAY_SIZE)
  expect_equal(ncol(shortwave), NDAYS)
  
})
