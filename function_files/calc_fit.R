#' @title calc_fit
#' @description calculates the objective/fitness of a model
#' @param model string of 0's and 1's that represent the model
#' @param response name of response variable as a string
#' @param regression_type type of regression; same as family argument in glm()
#' @param method objective/fitness function; takes in either "AIC" or "other
#' @param other.method other objective/fitness function to evaluate model
#' @return fitness value of the model
#' @examples
#' \dontrun{calc_fit(data = mtcars, model = "000011", response = "mpg")}

calc_fit <- function(model, response, regression_type = "gaussian", data, method = "AIC",  other.method = NULL, ...){

  ## Assertions
  assert_that(method == "AIC" | method == "other",
              msg = "The default objective function is AIC. If using another objective function, set `method` to 'other' and specify function in `other.method`")

  assert_that(is.data.frame(data))
  assert_that(is.string(model))
  assert_that(is.string(response))


  ## Converting 0-1 model string to model formula

  # Get the indices of the variables that are included in the model
  # (i.e. get the indices of the 1's in the model string)
  index_variables <- c(str_locate_all(model, '1')[[1]][, 1])

  # Subsetting the data to just include the columns of the possible covariates
  covariates_data <- data[ , c(-1)]

  # Getting the names of the covariates included in the model
  model_covariates <- names(subset(covariates_data, select = index_variables))

  # Pasting the response and covariates into model formula format
  variable_names <- paste(model_covariates, collapse = " + ")
  regression_formula <- paste(response, variable_names, sep = " ~ ")

  # Creating a regression_type glm model with our regression formula on a
  # subset of data that only includes our response variable and possible covariates
  regression_model <- glm(regression_formula, data = data, family = regression_type)

  # If the objective/fitness function is AIC
  if(method == "AIC"){

    # If another objective/fitness function is specified, output warning
    if(!is.null(other.method)) {
      warning("Default objective function is AIC. Set `method` to 'other' to use other objective function.")
    }

    # Return the AIC of the regression model
    return(AIC(regression_model))
  }

  # If the objective/fitness function is not AIC
  if(method == "other"){

    # Make sure the other objective/fitness function is specified
    assert_that(!is.null(other.method),
                msg = "Must specify `other.method` if using other fitness function.")

    # Return the fitness value of the regression model
    return(other.method(regression_model, ...))
  }
}
