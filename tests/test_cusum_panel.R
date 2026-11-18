source("R/cusum_panel.R")

expect_error <- function(expr) {
  caught <- tryCatch({ force(expr); FALSE }, error = function(e) TRUE)
  stopifnot(caught)
}

# Hand-calculated scores, sign convention and constant series.
panel <- rbind(changing = c(0, 0, 4, 4), constant = c(3, 3, 3, 3))
actual <- cusum_panel(panel)
expected <- rbind(c(-4 / sqrt(3), -4, -4 / sqrt(3)), c(0, 0, 0))
stopifnot(isTRUE(all.equal(unname(actual), expected)),
          identical(rownames(actual), rownames(panel)),
          identical(colnames(actual), c("1", "2", "3")))

# Compare cumulative sums with a separate direct-means calculation at every split.
uneven <- rbind(c(2, 8, -1, 4, 10), c(7, 3, 5, 2, -6))
reference <- matrix(0, nrow = 2, ncol = 4)
for (j in 1:2) {
  for (k in 1:4) {
    reference[j, k] <- sqrt(k * (5 - k) / 5) *
      (mean(uneven[j, 1:k]) - mean(uneven[j, (k + 1):5]))
  }
}
stopifnot(isTRUE(all.equal(unname(cusum_panel(uneven)), reference)),
          isTRUE(all.equal(cusum_panel(uneven + 1e12), cusum_panel(uneven))),
          isTRUE(all.equal(cusum_panel(-uneven), -cusum_panel(uneven))))

# A single series / split still returns a matrix; more series than time points work.
stopifnot(identical(dim(cusum_panel(matrix(c(1, 3), nrow = 1))), c(1L, 1L)),
          isTRUE(all.equal(as.numeric(cusum_panel(matrix(c(1, 3), 1))), -sqrt(2))))
wide_panel <- panel[rep(1:2, 6), , drop = FALSE]
stopifnot(identical(dim(cusum_panel(wide_panel)), c(12L, 3L)),
          isTRUE(all.equal(unname(cusum_panel(wide_panel)),
                           unname(actual[rep(1:2, 6), , drop = FALSE]))))

for (bad in list(1:4, matrix(numeric(0), 0, 4), matrix(1:3, 3, 1),
                 matrix(c(1, NA), 1), matrix(c(1, Inf), 1),
                 matrix(c("1", "2"), 1), matrix(c(TRUE, FALSE), 1))) {
  expect_error(cusum_panel(bad))
}
# Supplied row scales divide the scores; no recycling across series is allowed.
scaled_panel <- rbind(c(0, 0, 4, 4), c(0, 0, 8, 8))
scaled_expected <- rbind(c(-2 / sqrt(3), -2, -2 / sqrt(3)),
                         c(-2 / sqrt(3), -2, -2 / sqrt(3)))
stopifnot(isTRUE(all.equal(unname(cusum_panel(scaled_panel, c(2, 4))),
                          scaled_expected)),
          identical(cusum_panel(panel), cusum_panel(panel, c(1, 1))),
          isTRUE(all.equal(as.numeric(cusum_panel(matrix(c(1, 3), 1), 2)),
                           -sqrt(2) / 2)))
for (bad_scale in list(1, c(1, 2, 3), numeric(0), c(0, 1), c(-1, 1),
                       c(NA, 1), c(Inf, 1), c(1+1i, 1), c(TRUE, FALSE),
                       c("1", "2"), matrix(c(1, 2), 2))) {
  expect_error(cusum_panel(scaled_panel, bad_scale))
}
expect_error(cusum_panel(matrix(c(1+1i, 3), 1)))
expect_error(cusum_panel(matrix(c(-1e308, 1e308), 1)))
cat("All panel CUSUM tests passed.\n")
