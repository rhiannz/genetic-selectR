#' @title find_best_model
#' @description finds the best model of a given generation of genes
#' @param gene string of a crossed over gene that represents a model
#' @param fitness_vec vector of fitness values where each value corresponds to
#' the gene with the same index in the genes vector
#' @return a vector showing of the most fit genes and its fitness value
#' @examples
#' \dontrun{
#' test_genes <- c("1001", "0001", "1011")
#' test_fitness <- c(-1.52, 2.06, -3.77)
#' find_best_model(test_genes, test_fitness)
#' }

find_best_model <- function(genes, fitness_vec) {

  # Find the index of out fittest model
  most_fit_index <- which.min(fitness_vec)

  # Find this generation's best fitness value
  most_fit_value <- fitness_vec[most_fit_index]

  # Find this generation's most fit gene
  most_fit_gene <- genes[most_fit_index]

  return(c(most_fit_gene, most_fit_value))

}

