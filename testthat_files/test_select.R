## Testing the select function

library(MASS)
data("Boston")


test_that("select returns a data frame when inputting valid inputs", {
  expect_type(select(Boston, crim ~ zn + indus + chas + nox + rm + age +dis+rad+tax+
                       ptratio+black + lstat+medv, objective_function = "other",
                     other_objective = logLik, selection = "tournament",
                     cross_points = 3,
                     mutation_rate = 0.2, pop_scale = 2), "list")

  expect_length(select(Boston, crim ~ zn + indus + chas + nox + rm + age +dis+rad+tax+
                       ptratio+black + lstat+medv, objective_function = "other",
                     other_objective = logLik, selection = "tournament",
                     cross_points = 3,
                     mutation_rate = 0.2, pop_scale = 2), 5)
})




test_that("select returns an error when inputting a non-numeric value for
          pop_scale", {
            expect_error(select(Boston, crim~zn + indus + chas + nox + rm + age +
                                  dis+rad+tax+ptratio+black + lstat+medv,
                                pop_scale = "3"))

          })


test_that("select returns an error when inputting a string that is not either
          'AIC' or 'other' for objective_function", {
            expect_error(select(Boston, crim~zn + indus + chas + nox + rm + age +
                                  dis+rad+tax+ptratio+black + lstat+medv,
                                objective_function = "blahblah"))
          })



test_that("select returns an error when user inputs other for
          objective_function without specifying an input for other_objective ",
          {
            expect_error(select(Boston, crim~zn + indus + chas + nox + rm + age +
                                  dis+rad+tax+ptratio+black + lstat+medv,
                                objective_function = "other"))
          })



test_that("select returns an error when inputting a non-numeric value for
          cross_points", {
            expect_error(select(Boston, crim~zn + indus + chas + nox + rm + age +
                                  dis+rad+tax+ptratio+black + lstat+medv,
                                cross_points = "five"))

          })



test_that("select returns an error when inputting an invalid input for
          mutation_rate", {
            expect_error(select(Boston, crim ~ zn + indus + chas + nox + rm + age +
                                  dis+rad+tax+ptratio+black + lstat+medv,
                                selection = "tournament", mutation_rate = 1.2))
          })



test_that("select returns an error when inputting a non-numeric value for
          pop_scale", {
            expect_error(select(Boston, crim~zn + indus + chas + nox + rm + age +
                                  dis+rad+tax+ptratio+black + lstat+medv,
                                pop_scale = "3"))

          })



test_that("select returns an error when inputting a non-numeric value for
          diveristy_threshold", {
            expect_error(select(Boston, crim~zn + indus + chas + nox + rm + age +
                                  dis+rad+tax+ptratio+black + lstat+medv,
                                diversity_threshold = "0.10"))

          })


test_that("select returns an error when inputting a non-numeric value for
          max_itr", {
            expect_error(select(Boston, crim~zn + indus + chas + nox + rm + age +
                                  dis+rad+tax+ptratio+black + lstat+medv,
                                max_itr= "100"))

          })
