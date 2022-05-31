#' @title simulate_gene
#' @description Helper function of gen_firstgen that simulates a single random model (excludes all 0's model)
#' @param C number of covariates in inputted model
#' @return a string of length C of 0's and 1's representing a random model
#' @examples
#'  simulate_gene(5)

simulate_gene <- function(C){
  
  ## Assertions
  assert_that(is.numeric(C), msg = "The number of covariates C must be a numeric integer")
  
  # Sample from (0, 1) C times
  gene <- replicate(C, sample(c(0, 1), 1))
  
  # If the simulated model/gene is a string of 0's
  # generate another model/gene until it is no longer a string of 0's
  while(identical(gene, rep(0, C))){
    gene <- replicate(C, sample(c(0, 1), 1))
  }
  gene
}