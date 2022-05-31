#' @title select_parents
#' @description Carries out parent selection process according to the selection method
#' @param method The selection method. Can be either "prop_rand" or "tournament".
#' "prop_rand" chooses one parent proportional to the fitness function and the other parent randomly.
#' "tournament" chooses parents based on tournament selection
#' @param genes A vector of different 0-1 model strings
#' @param fitness_vec A vector of the corresonding fitness of the models in `genes`
#' @param P population size
#' @return a list of parent pairs where each element has the model of the parent pair
#' @examples
#'  \dontrun{select_parents(test_gen, fitness_test, 10, method = "prop_rand")}

select_parents <- function(method = "prop_rand", genes, fitness_vec, P){

  ## Assertions
  assert_that(is.character(genes))
  assert_that(is.numeric(fitness_vec))
  assert_that(is.numeric(P), msg = "The population size P must be a numeric integer")

  # Initializes an empty list for parent pairs
  pairs_list <- rep(list(matrix(nrow = 2, ncol = nchar(genes[1]))), P)

  ## If selection method is "prop_rand"
  if(method != "prop_rand" & method != "tournament"){
    warning("`method` input is not valid.")
  }

  if(method == "prop_rand"){

    # Scale our fitness values to be positive and non-zero
    scaled_fitness <- fitness_vec + 1 + abs(min(fitness_vec))

    # Get the sum of our scaled fitness values
    total_fitness <- sum(scaled_fitness)

    # Get the proportion of each scaled fitness value over the total
    prop_fitness <- scaled_fitness / total_fitness

    # The lower the AIC, the higher weight we give to that model, so
    # now the lower prop_fitness correspond to higher weight
    weight <- 1 - prop_fitness

    # Scale the weights so they sum to 1
    scaled_weight <- weight / sum(weight)
    assert_that(all.equal(sum(scaled_weight), 1), msg = "Weights do not sum to 1")

    ## Select P pairs of parents
    indices <- seq(1, P)

    for(i in indices){

      # Select parent 1 according to the weights, where higher fitness models
      # are more likely to be chosen
      parent_1_index <- sample(indices, size = 1, prob = scaled_weight)

      # Select parent 2 randomly
      parent_2_index <- sample(indices[-parent_1_index], size = 1)

      # Insert our parent pair into pairs_list
      pairs_list[[i]] <- matrix(c(as.numeric(str_split(genes, "")[[parent_1_index]]),
                                  as.numeric(str_split(genes, "")[[parent_1_index]])),
                                nrow = 2, byrow = TRUE)
    }
  }

  if(method == "tournament"){

    # Initialize an empty vector
    parents <- c()

    # We want to select two times as many parents as our
    n_parents_made <- 2 * P

    # Initialize a variable so that we enter our while loop
    continue = TRUE

    while (continue == TRUE) {

      # Split up our genes into groups of five by going through our vector of
      # genes five at time
      group <- 1:5

      itr <- 0

      # Exit our while loop when the highest index of our group is higher than
      # the total population of genes
      while (group[5] <= P) {

        # Find the index of the most fit gene in each subset
        fittest_index <- which.min(fitness_vec[group])

        # Add the most fit gene to our vector of parents
        parents <- c(parents, genes[fittest_index + itr])

        # Increase our group index by 5 so that we will be looking at the next
        # 5 genes in our vector
        group <- group + 5

        # Add five to our itr variable so that the fittest_index corresponds
        # to the correct gene in our vector during each round of the while loop
        itr <- itr + 5
      }

      # Once we have chosen at least 2*P parents, return the first 2*P parents
      if (length(parents) >= n_parents_made) {
        parents <- parents[1:n_parents_made]

        ## Select P pairs of parents
        indices <- seq(1, 2*P)
        for(i in indices){

          # Select parent 1 according to the weights, where higher fitness models
          # are more likely to be chosen
          parent_1_index <- sample(indices, size = 1)

          # Select parent 2 randomly
          parent_2_index <- sample(indices[-parent_1_index], size = 1)

          # Insert our parent pair into pairs_list
          pairs_list[[i]] <- matrix(c(as.numeric(str_split(parents, "")[[parent_1_index]]),
                                      as.numeric(str_split(parents, "")[[parent_2_index]])),
                                    nrow = 2, byrow = TRUE)
        }



        return(pairs_list)
        break
      }

      # If we have not chosen enough parents yet, put our genes and fitness
      # values in the same new random order and repeat our process again
      new_order <- sample(1:P, P, replace = FALSE)
      genes <- genes[new_order]
      fitness_vec <- fitness_vec[new_order]

    }
  }

  pairs_list
}
