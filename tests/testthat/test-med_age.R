test_that("med.age errors on length mismatch", {
  expect_error(med.age(1:3, c(0, 5)), "lengths of age groups \\(age\\) and Population counts \\(N\\) are not the same")
})

test_that("med.age works", {
  age <- 0:2
  N <- c(10, 10, 10)
  expect_equal(med.age(N, age), 1.5)
})
