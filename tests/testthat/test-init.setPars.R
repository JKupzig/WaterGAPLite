#devtools::load_all()

test_that("format pars table", {
  par_table <- init.setPars()
  expect_equal(nrow(par_table), 22)
  expect_equal(ncol(par_table), 2)
})

