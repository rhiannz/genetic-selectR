## Tests for calc_fit

test_that("calc_fit works with standard input", {
  formula <- mpg ~ cyl+disp + hp +drat+wt+ qsec
  covariates_string <- as.character(formula)[3]
  response <- as.character(formula)[2]

  data("mtcars")
  covariates <- str_split(gsub("\\s", "", covariates_string), pattern = fixed("+"), simplify = TRUE)
  subsetted_data <- mtcars[ , c(response, covariates)]

  expect_length(calc_fit(data = subsetted_data, model = "000011", response = response), 1)
  expect_type(calc_fit(data = subsetted_data, model = "000011", response = response), 'double')
})

test_that("calc_fit outputs error with non-standard input", {
  data("mtcars")
  expect_error(calc_fit(data = matrix(0, ncol = 10, nrow = 5), model = "100", response = "mpg"))
  expect_error(calc_fit(data = mtcars, model = mpg ~ cars, response = "mpg"))
  expect_error(calc_fit(data = mtcars, model = "000011", response = 23))
})

test_that("calc_fit works with other objective function", {
  data("mtcars")
  expect_length(calc_fit(data = mtcars, model = "000011", response = "mpg",
                         method = "other", other.method = logLik), 1)
  expect_type(calc_fit(data = mtcars, model = "000011", response = "mpg",
                       method = "other", other.method = logLik), 'double')
})

test_that("calc_fit outputs error with other objective function not specified", {
  data("mtcars")
  expect_error(calc_fit(data = mtcars, model = "000011", response = "mpg",
                        method = "other"),
               "Must specify `other.method` if using other fitness function.")
  expect_warning(calc_fit(data = mtcars, model = "000011", response = "mpg",
                          other.method = logLik),
                 "Default objective function is AIC. Set `method` to 'other' to use other objective function.")
  expect_error(calc_fit(data = mtcars, model = "000011", reponse = "mpg",
                        method = "random"), "The default objective function is AIC. If using another objective function, set `method` to 'other' and specify function in `other.method`")
})
