## Tests for select_parents

test_that("select_parents works with standard input", {
  # Example inputs
  C <- 6
  P <- 10
  formula <- mpg ~ cyl+disp + hp +drat+wt+ qsec
  # Get string of covariates: "cyl+disp + hp +drat+wt+ qsec"
  covariates_string <- as.character(formula)[3]
  # Get string of response variable: "mpg"
  response <- as.character(formula)[2]

  # Generate first generation
  test_gen <- gen_firstgen(C = C, P = P)
  # Get vector of covariate names
  covariates <- str_split(gsub("\\s", "", covariates_string), pattern = fixed("+"), simplify = TRUE)
  # Subset data to just the reponse variable and possible covariates
  subsetted_data <- mtcars[ , c(response, covariates)]
  # Get fitness values for each model in first generation
  fitness_test <- sapply(test_gen, FUN = calc_fit, data = subsetted_data, response = response, method = "AIC")

  expect_length(select_parents(test_gen, fitness_test, P, method = "prop_rand"), P)
  expect_type(select_parents(test_gen, fitness_test, P, method = "prop_rand"), "list")

  expect_length(select_parents(test_gen, fitness_test, P, method = "tournament"), 2*P)
  expect_type(select_parents(test_gen, fitness_test, P, method = "tournament"), "list")
})

test_that("select_parents outputs error with invalid input", {
  # Example inputs
  C <- 6
  P <- 10
  formula <- mpg ~ cyl+disp + hp +drat+wt+ qsec
  # Get string of covariates: "cyl+disp + hp +drat+wt+ qsec"
  covariates_string <- as.character(formula)[3]
  # Get string of response variable: "mpg"
  response <- as.character(formula)[2]

  # Generate first generation
  test_gen <- gen_firstgen(C = C, P = P)
  # Get vector of covariate names
  covariates <- str_split(gsub("\\s", "", covariates_string), pattern = fixed("+"), simplify = TRUE)
  # Subset data to just the reponse variable and possible covariates
  subsetted_data <- mtcars[ , c(response, covariates)]
  # Get fitness values for each model in first generation
  fitness_test <- sapply(test_gen, FUN = calc_fit, data = subsetted_data, response = response, method = "AIC")

  expect_warning(select_parents(test_gen, fitness_test, P, method = "random"),
                 "`method` input is not valid.")
  expect_error(select_parents(c(100, 10, 1, 0), fitness_test, P, method = "prop_rand"))
  expect_error(select_parents(test_gen, c("010101", "38472"), P, method = "prop_rand"))
  expect_error(select_parents(test_gen, fitness_test, P = "ten", method = "prop_rand"))
})
