#' @title mutate_genes
#' @description chooses each allele of a gene with an independent probability
#' of mutating and assigns a new allele at random
#' @param gene string of a crossed over gene that represents a model
#' @param mutation_rate numeric value between 0 and 1
#' @param C integer value indicating how many characters are in our input string
#' @return string of mutated gene that represents a model
#' @examples
#' mutate_genes(gene = c("1001", "0001", "1011"),
#' mutation_rate = 0.02, C = 4)

mutate_genes <- function(gene, mutation_rate = 0.01, C) {
  # Assert that all of our inputs are what we expect them to be
  assert_that((is.character(gene)),
              msg = "gene must a character string")

  assert_that(is.numeric(mutation_rate),
              msg = "mutation_rate must be a numeric value")

  assert_that((mutation_rate >= 0 & mutation_rate <= 1),
              msg = "mutation_rate must be between or equal to 0 and 1")

  assert_that(is.numeric(C),
              msg = "The number of covariates C must be a numeric integer")



  # Initialize our variable so that we enter our while loop
  all_zero <- TRUE


  # Split up each allele our gene so that we can iterate over them separately
  split_gene <- strsplit(gene, '')[[1]]


  # Exit our while loop if all alleles in our gene are 0
  while (all_zero == TRUE) {


    # Determine the locus at which our gene while be mutated
    rand <- runif(C)
    indices <- which(rand < mutation_rate)


    # Replace the selected loci with a randomly chosen allele
    for (i in indices) {
      split_gene[i] <- sample(c(0,1), replace = TRUE, size = 1)
    }


    # Check if all alleles in our gene are 0
    all_zero <- (sum(split_gene == "0") == C)

  }

  # Put split up alleles back together
  mutated_gene <- paste(split_gene, collapse = '')


  return(mutated_gene)

}

