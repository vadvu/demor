test_that("ages function handles first interval correctly", {
  age <- 0:100
  groups <- seq(10, 90, 10)
  res <- ages(x = age, groups = groups, char = FALSE, below_min_val = NA)
  expect_equal(sum(is.na(res)), sum(age < groups[1]))
})
