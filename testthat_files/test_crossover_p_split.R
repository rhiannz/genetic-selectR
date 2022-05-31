## Tests for crossover

parents<-list()
parents[[1]]<-rbind(rbinom(n = 9, size = 1, prob = 0.5),
                    rbinom(n = 9, size = 1, prob = 0.5))
parents[[2]]<-rbind(rbinom(n = 9, size = 1, prob = 0.5),
                    rbinom(n = 9, size = 1, prob = 0.5))

test_that("crossover give the correct length", {
  expect_length(crossover_p_split(parents, p=2), length(parents))
})

test_that("crossover give the correct type", {
  expect_type(crossover_p_split(parents, p=2), "character")
})

test_that("crossover outputs error message for non-numeric integer input", {
  expect_error(crossover_p_split(parents, "two"),
               "the number of split points must be a numeric value", fixed = TRUE)
})
