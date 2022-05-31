## Tests for mutate

gene <- as.character(10000)
mutation_rate <- 0.20

test_that("mutate give the correct length", {
  expect_length(mutate_genes(gene, mutation_rate, length(gene)), length(gene))
})

test_that("mutate give the correct type", {
  expect_type(mutate_genes(gene, mutation_rate, length(gene)), "character")
})

test_that("mutate outputs error message for non-numeric integer input", {
  expect_error(mutate_genes(gene, mutation_rate, "five"),
               "C must be a numeric integer", fixed = TRUE)
})
