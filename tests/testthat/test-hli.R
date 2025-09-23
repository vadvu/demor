test_that("hli works on abridged and full life tables", {
  age_full <- 0:100
  age_abr  <- c(0:1, seq(5, 85, 5))
  mx_full  <- rep(0.01, length(age_full))
  mx_abr   <- rep(0.01, length(age_abr))

  h_full <- hli(age_full, mx_full)
  h_abr  <- hli(age_abr,  mx_abr)

  expect_true(is.finite(h_full) && h_full > 0)
  expect_true(is.finite(h_abr)  && h_abr  > 0)

  # They won't be identical, but should be in the same ballpark.
  expect_lt(abs(log(h_full / h_abr)), 0.1)  # ~10% tolerance; adjust if needed
})
