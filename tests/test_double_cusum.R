source("R/cusum_panel.R")
source("R/double_cusum.R")

# Hand calculations cover one large score, equal magnitudes, and m = n.
scores <- cbind(one = c(-4, 0), both = c(4, -4))
actual <- double_cusum(scores)
expected <- cbind(c(2 * sqrt(3), 2), c(4 / sqrt(3), 4))
stopifnot(isTRUE(all.equal(unname(actual), expected)),
          identical(rownames(actual), c("1", "2")),
          identical(colnames(actual), colnames(scores)))
three <- matrix(c(-4, 2, -1), ncol = 1)
stopifnot(isTRUE(all.equal(as.numeric(double_cusum(three)),
                           c(17 / sqrt(30), 11 / (2 * sqrt(3)), 7 / sqrt(6)))))

# Independent reference: append n zeros, then compare means of two groups.
# The second group has 2*n-m entries, including those zeros.
fixture <- cbind(c(-3, 1, 5), c(2, -2, 0), c(0, 0, 0))
reference <- matrix(0, 3, 3)
for (k in 1:3) {
  padded <- c(sort(abs(fixture[, k]), decreasing = TRUE), rep(0, 3))
  for (m in 1:3) {
    reference[m, k] <- sqrt(m * (6 - m) / 6) *
      (mean(padded[seq_len(m)]) - mean(padded[(m + 1):6]))
  }
}
stopifnot(isTRUE(all.equal(unname(double_cusum(fixture)), reference)),
          isTRUE(all.equal(double_cusum(fixture[c(3, 1, 2), ]), double_cusum(fixture))),
          isTRUE(all.equal(double_cusum(-fixture), double_cusum(fixture))),
          isTRUE(all.equal(double_cusum(3 * fixture), 3 * double_cusum(fixture))))

# Matrix shape survives one series / one split; all-zero scores stay zero.
stopifnot(identical(dim(double_cusum(matrix(-4, 1, 1))), c(1L, 1L)),
          isTRUE(all.equal(as.numeric(double_cusum(matrix(-4, 1, 1))), 2 * sqrt(2))),
          all(double_cusum(matrix(0, 2, 3)) == 0),
          double_cusum(matrix(1500000000L, 2, 1))[2, 1] == 1500000000)

# End-to-end check with the saved two-series example.
panel <- rbind(c(0, 0, 4, 4), c(3, 3, 3, 3))
combined <- double_cusum(cusum_panel(panel))
stopifnot(isTRUE(all.equal(unname(combined),
                           rbind(c(2, 2 * sqrt(3), 2),
                                 c(2 / sqrt(3), 2, 2 / sqrt(3))))))

for (bad in list(1:4, matrix(numeric(0), 0, 2), matrix(numeric(0), 2, 0),
                 matrix(NA_real_, 1), matrix(Inf, 1), matrix("4", 1),
                 matrix(TRUE, 1), matrix(1 + 1i, 1), matrix(1e308, 4, 1))) {
  caught <- tryCatch({ double_cusum(bad); FALSE }, error = function(e) TRUE)
  stopifnot(caught)
}
cat("All Double CUSUM tests passed.\n")
