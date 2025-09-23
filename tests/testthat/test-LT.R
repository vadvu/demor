test_that("l0 scales lx/dx/Lx/Tx but leaves qx and ex invariant", {
  age <- 0:5; mx <- rep(0.01, 6)
  lt1 <- LT(age = age, mx = mx, l0 = 1)
  ltB <- LT(age = age, mx = mx, l0 = 1e5)

  expect_equal(lt1[, "qx"], ltB[, "qx"])
  expect_equal(lt1[, "ex"], ltB[, "ex"], tolerance = 1e-2)
  expect_false(all(abs(ltB[, "lx"] / lt1[, "lx"]-1) < 1e-3))
  expect_false(all(abs(ltB[, "dx"] / lt1[, "dx"]-1) < 1e-3))
  expect_false(all(abs(ltB[, "Lx"] / lt1[, "Lx"]-1) < 1e-3))
  expect_false(all(abs(ltB[, "Tx"] / lt1[, "Tx"]-1) < 1e-3))
})

test_that("weights arguement adds columns and computes wex as defined", {
  age <- 0:5; mx <- rep(0.01, 6)
  w   <- c(0.1, 0.2, 0.2, 0.2, 0.2, 0.1)
  lt  <- LT(age, mx = mx, w = w)
  expect_true(all(c("w","wLx","wex") %in% colnames(lt)))

  # Recompute wex using returned (rounded) lx and wLx; allow 1e-2 tolerance because wex is rounded to 2 d.p.
  lx   <- as.numeric(lt[, "lx"])
  wLx  <- as.numeric(lt[, "wLx"])
  wex2 <- round((1 / lx) * rev(cumsum(wLx)), 2)
  expect_equal(as.numeric(lt[, "wex"]), wex2, tolerance = 1e-2)
})

test_that("LT input validation and mx<0 handling work", {
  expect_error(LT(age = 0:5, mx = 0:3), regexp = "length")
  age <- 0:5; mx <- c(-0.01, rep(0.01, 5))
  expect_warning(lt <- LT(age = age, mx = mx), regexp = "replace")
  expect_equal(as.numeric(lt[1, "mx"]), 0)  # negatives replaced with 0
})
