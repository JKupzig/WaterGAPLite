# devtools::load_all()

testthat::test_that("daily soil routine",
{
  model <- readRDS("example_basin.rds") # ./tests/testthat/example_basin.rds
  initModel(model)
  Settings <- init.settings()
  defSettings(Settings);
  initializeModel();
  
  array_size <- model$array_size
  SimPeriod <- model$SimPeriod[1:20]
  surfaceRunoff <- matrix(1, nrow=length(SimPeriod), ncol=array_size)
  GroundwaterRunoff <-  matrix(1, nrow=length(SimPeriod), ncol=array_size)
  PETw <- matrix(3, nrow=length(SimPeriod), ncol=array_size)
  Prec <- matrix(1, nrow=length(SimPeriod), ncol=array_size)
  
  routing_result <- routing(SimPeriod, 
                            surfaceRunoff, 
                            GroundwaterRunoff, 
                            PETw, 
                            Prec)
  
  expect_equal(sum(routing_result$River$Discharge), 31.023392)
})
                      