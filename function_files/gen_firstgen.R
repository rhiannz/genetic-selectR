#' @title gen_firstgen
#' @description Randomly generates the first generation of models
#' @param C number of covariates in inputted model
#' @param P population size
#' @return vector of size P of strings that represent the models
#' @examples
#'  gen_firstgen(C = 6, P = 10)

gen_firstgen <- function(C = num_covariates, P = pop_size){
  
  ## Assertions
  assert_that(is.numeric(C), msg = "The number of covariates C must be a numeric integer")
  assert_that(is.numeric(P), msg = "The population size P must be a numeric integer")
  
  # Simulate P model/genes of length C
  chromosome_sample <- replicate(n = P, simulate_gene(C), simplify = FALSE)
  
  # Convert each row in chromosome_sample into a string
  first_gen <- sapply(chromosome_sample, FUN = paste, collapse = "")
  
  first_gen
}