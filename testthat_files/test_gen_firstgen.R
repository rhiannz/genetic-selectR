## Tests for gen_firstgen

test_that("gen_firstgen works with normal input", {
  expect_length(gen_firstgen(C = 5, P = 10), 10)
  expect_type(gen_firstgen(C = 5, P = 10), 'character')
})

test_that("gen_firstgen works with large input", {
  expect_length(gen_firstgen(C = 250, P = 10000), 10000)
  expect_type(gen_firstgen(C = 250, P = 10000), 'character')
})

test_that("gen_firstgen outputs error message for non-numeric integer input", {
  expect_error(gen_firstgen(C = "five", P = 100),
               "The number of covariates C must be a numeric integer", fixed = TRUE)
  expect_error(gen_firstgen(C = 15, P = "150"),
               "The population size P must be a numeric integer", fixed = TRUE)
})
