#devtools::load_all()

test_that("get routing info (namibia)", {
  
  init.classes()
  
  africa <- new("Continent",
                   corRow=629, 
                   corCol=1855,
                   ncol=1237, 
                   DataDir="../data", 
                   contName="af")
  
  basin_object <- new("Basin",
                      location=c(16.12436505, -20.20835755),
                      cont=africa)
  

  basin_object <- basin.def_basin(basin_object)
  basin_object <- basin.get_routing_info(basin_object)
  
  expect_equal(max(basin_object@outflow), 99)
  expect_equal(max(basin_object@routeOrder), 25)
  expect_equal(length(basin_object@outflow), 103)

})

