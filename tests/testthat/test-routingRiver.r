#devtools::load_all()

testthat::test_that("routing river (modified)",
{ 
  # note that the two vectors are modified by reference, so 
  # they must be defined in every function call from scratch 
  # thus, not variable can be used as for cell, velocity_in_d and inflow!
  
  cell <- 1
  velocity_in_d <- 2
  inflow <- 2

  routed_water <- WaterGAPLite:::routingRiver(
    cell, velocity_in_d, inflow, c(1,1,1,1), c(0,0,0,0))
  
  routed_water_slow <- WaterGAPLite:::routingRiver(
    cell, velocity_in_d + 0.1, inflow, c(1,1,1,1), c(0,0,0,0))
  
  routed_water_more_in <- WaterGAPLite:::routingRiver(
    cell, velocity_in_d, inflow + 0.1, c(1,1,1,1), c(0,0,0,0))
  
  routed_water_more_prestep <- WaterGAPLite:::routingRiver(
    cell, velocity_in_d, inflow, c(1,1,1,1) + c(0,0.1,0,0), c(0,0,0,0))
  
  routed_water_more_storage <- WaterGAPLite:::routingRiver(
    cell, velocity_in_d, inflow, c(1,1,1,1), c(0,0,0,0)*2)
  
  expect_equal(routed_water, 1.39346934)
  expect_gt(routed_water, routed_water_slow)
  expect_gt(routed_water_more_in, routed_water)
  expect_gt(routed_water_more_prestep, routed_water)
  expect_equal(routed_water, routed_water_more_storage)
  
})

testthat::test_that("estimate PET from river",
{ 

  bankfull_flow <- 20
  river_length_in_km <- 8. # in km
  pet <- 5 # in mm 
  
  pet_from_river <- WaterGAPLite:::estimate_pet_from_river(
    bankfull_flow, river_length_in_km, pet
  )
  
  pet_from_river_less_length <- WaterGAPLite:::estimate_pet_from_river(
    bankfull_flow, river_length_in_km-0.01, pet
  )
  
  pet_from_river_less_bankfull <- WaterGAPLite:::estimate_pet_from_river(
    bankfull_flow-1, river_length_in_km, pet
  )

  
  expect_equal(pet_from_river, 0.49749924)
  expect_gt(pet_from_river, pet_from_river_less_length)
  expect_gt(pet_from_river, pet_from_river_less_bankfull)

})
