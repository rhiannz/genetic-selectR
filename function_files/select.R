#' @export
#' @title select
#' @description finds the most fit subset of covariates to model the given
#' response variable
#' @param dataset data frame with one or more
#' @param formula linear formula in the format "response ~ covariate1 +
#' covariate2 + ..." using the columns names of the dataset
#' @param regression string specifying the type of regression used on our model
#' @param pop_scale numeric value that will determine the population size of
#' each generation of genes based off of the number of covariates in the user's
#' formula
#' @param objective_function string "AIC" or "other" specifying the type of
#' fitness that will be used in our objective function to determine the
#' fitness of a given model
#' @param other_objective function name that must be provided by the user if
#' they input "other" as their objective function and will be used instead of
#' the "AIC" method. If specifying the a different objective function, make sure
#' that it is a function you want to minimize (i.e. it should behave like AIC)
#' @param selection string "prop_rand" or "tournament" specifying the selection
#' method that will be used to choose the parents of a generation of genes
#' @param cross_points integer value indicating the number of points at which
#' each gene will be crossed over
#' @param mutation_rate numeric value between 0 and 1 indicating the rate at
#' which alleles of a genes will be selected for mutation
#' @param max_itr integer value indicating the maximum number of generations
#' that our function will make before breaking and returning our current values
#' @param diversity_threshold numeric value specifying how low our threshold
#' for diversity should be
#' @return plot comparing best and average fitness values throughout the
#' generations of genes that have been created
#' @return data frame showing our overall best model, its fitness score,
#' the generation that it was created, and the average fitness of that
#' generation of genes
#' @examples
#' \dontrun{
#' library(MASS)
#' data("Boston")
#' select(Boston, crim ~ zn + indus + age + dis + rad + tax + ptratio, selection = "tournament")
#'
#' select(Boston, crim ~ zn + indus + age + dis + rad + tax + ptratio, objective_function = "other",
#' other_objective = logLik, selection = "tournament", cross_points = 3,
#' mutation_rate = 0.2, pop_scale = 2)
#' }



select <- function(dataset, formula, regression = "gaussian", pop_scale = 1.5,
                   objective_function = "AIC", other_objective = NULL,
                   selection = "prop_rand", cross_points = 1,
                   mutation_rate = 0.01,
                   max_itr = 50, diversity_threshold = 0.001){


  # Assert that all of our inputs are what we expect them to be
  assert_that((is.data.frame(dataset)),
              msg = "dataset input must be a data frame")

  assert_that((inherits(formula, "formula")),
              msg = "formula input must be a formula class")

  assert_that((is.numeric(pop_scale)),
              msg = "pop_scale input must be numeric")

  assert_that(is.numeric(max_itr),
              msg = "max_itr must be a numeric value")


  assert_that(is.numeric(diversity_threshold),
              msg = "diversity_threshold must be a numeric value")
  # Initiate a data frame to hold information about our best models in each
  # generation
  top_models <- data.frame(matrix(ncol = 4, nrow = 0))


  # Set chromosome length
  covariates_string <- as.character(formula)[3]
  C <- str_count(covariates_string, fixed("+")) + 1
  P <- ceiling(pop_scale * C)

  # Determine our response variable
  response <- as.character(formula)[2]


  # Subset our data to the columns that we will be using
  covariates <- str_split(gsub("\\s", "", covariates_string),
                          pattern = fixed("+"), simplify = TRUE)
  subsetted_data <- dataset[ , c(response, covariates)]



  # Generate our first generation of genes
  genes <- gen_firstgen(C = C, P = P)


  # Initiate a counter to keep track of how many generations we have
  n_gen <- 1



  # Initiate our diversity so that we enter our while loop
  diversity <- 10
  n_low <- 0


  # We will stop creating new generations if the coefficient of variation
  # reaches below 1 in any generation of genes
  continue = TRUE
  while(continue == TRUE) {

    # Calculate the fitness of each of our genes
    fitness_vec <- sapply(genes, FUN = calc_fit, data = subsetted_data,
                          response = response, method = objective_function,
                          other.method = other_objective,
                          regression_type = regression)


    # Find the best model and its fitness our current generation
    best_model <- find_best_model(genes, fitness_vec)

    # Find the overall average fitness of our current generation
    avg_fitness <- mean(fitness_vec)

    # Put together all the information that we will be plotting
    gen_info <- c(n_gen, best_model, avg_fitness)

    # Add our current generation's info to the previous generations' info
    top_models <- rbind(top_models, gen_info)


    #### Begin creating our next generation ###

    # Select the parents of our current generation
    parents <- select_parents(method = selection, genes = genes, fitness_vec = fitness_vec, P = P)

    # Cross over our genes
    crossed_genes <- crossover_p_split(parents, cross_points)


    # Mutate our crossed genes. These genes are now current generation
    genes <- sapply(X = crossed_genes, FUN = mutate_genes,
                    mutation_rate = mutation_rate, C = C)


    # Calculate the diversity of our new generation
    # We are using a scaled relative error between the best fitness score of our
    # current generation and the best fitness score of our previous generation
    # We first let our function run through three generations before beginning
    # to check our diversity scores
    if (n_gen > 3) {
      prev_best_fitness <- as.numeric(top_models[n_gen-1, 3])
      best_fitness <- as.numeric(best_model[2])


      diversity <- ((abs(best_fitness - prev_best_fitness) * 100) /
                      abs(prev_best_fitness))


      # Once our diversity is lower than the threshold for 3 consecutive
      # generations, our function will break and return our current values
      if(diversity < diversity_threshold){
        n_low <- n_low + 1
      } else {
        n_low <- 0
      }

      if(n_low >= 3){
        break
      }

    }


    # Update our generation counter
    n_gen <- n_gen + 1


    # Exit our while loop when one of our criterion is reached
    if (n_gen > max_itr) {

      warning("Warning: Maximum number of generations were created before
              correlation of covariance reached below 1.")
      break
    }



  }

  # Label our data frame's column names
  colnames(top_models) <- c("Generation", "Best_Model",
                            "Best_Fitness_Value", "Average_Fitness")


  # Convert all columns except the models column to a numeric values
  top_models[, -2] <- apply(top_models[, -2], MARGIN = 2,
                            function(x) as.numeric(x))


  # Get the indices of the variables that are included in the model
  # (i.e. get the indices of the 1's in the model string)
  index_locate <- str_locate_all(top_models$Best_Model, '1')
  index_list <- lapply(index_locate, function(x){x[, 'start']})


  # Subsetting the data to just include the columns of the possible covariates
  covariates_data <- subsetted_data[ , c(-1)]


  # Getting the names of the covariates included in the model
  model_covariates <- lapply(index_list,
                             function(x){
                               names(subset(covariates_data, select = x))})

  # Pasting the response and covariates into model formula format
  covariates_formula <- lapply(model_covariates,
                               function(x) {paste(x, collapse = " + ")})
  regression_formula <- sapply(covariates_formula,
                               function(x) {paste(response, x, sep = " ~ ")})
  # Adding to top_models dataframe
  top_models$Best_Model_String <- regression_formula


  # Find the model with the best fitness value
  best_model_index <- which.min(top_models$Best_Fitness_Value)
  overall_best_model <- top_models[best_model_index, ]



  # Plot the best vs average fitness value of each generation
  print(
    ggplot(top_models, aes(x = Generation)) +
      geom_line(aes(y = Best_Fitness_Value, color = "best fitness values")) +
      geom_line(aes(y = Average_Fitness, color = "average fitness")) +
      geom_point(aes(x = overall_best_model[, 1], y = overall_best_model[, 3],
                     color = "overall best point"), size = 3) +
      scale_color_manual(values = c("best fitness values" = "blue4",
                                    "average fitness" = "red",
                                    "overall best point" = "cyan1"),
                         guide = guide_legend(override.aes = list(
                           linetype = c(1, 1, 0),
                           shape = c(NA, NA, 16)))) +
      labs(y = "Fitness Values", color = "Legend",
           title = "Comparing Best and Average Fitness Values")

  )


  return(overall_best_model)


}
