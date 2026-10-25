source("R/cusum_panel.R")
source("R/double_cusum.R")
source("R/double_cusum_scan.R")

# Known panel: the largest entry is at split 2, m = 1.
panel <- rbind(c(0, 0, 4, 4), c(3, 3, 3, 3))
result <- double_cusum_scan(panel)
stopifnot(isTRUE(all.equal(result$statistic, 2 * sqrt(3))),
          result$split == 2L, result$m == 1L)

# When both rows change equally, grouping both gives the larger score.
both <- rbind(c(0, 0, 4, 4), c(0, 0, 4, 4))
result <- double_cusum_scan(both)
stopifnot(isTRUE(all.equal(result$statistic, 4)),
          result$split == 2L, result$m == 2L)

# A zero score still has a deterministic candidate, not a change decision.
flat <- double_cusum_scan(matrix(3, 2, 4))
stopifnot(flat$statistic == 0, flat$split == 1L, flat$m == 1L,
          identical(names(flat), c("statistic", "split", "m")))

# Independent reference: direct means (no cumulative sums or production calls).
# Appending n zeros makes the second group size 2*n-m, as in the DC formula.
reference_candidates <- function(x) {
  n <- nrow(x)
  nt <- ncol(x)
  candidates <- expand.grid(m = seq_len(n), split = seq_len(nt - 1L))
  candidates$statistic <- NA_real_
  for (k in seq_len(nt - 1L)) {
    scores <- numeric(n)
    for (j in seq_len(n)) {
      scores[j] <- sqrt(k * (nt - k) / nt) *
        (mean(x[j, seq_len(k)]) - mean(x[j, (k + 1L):nt]))
    }
    padded <- c(sort(abs(scores), decreasing = TRUE), rep(0, n))
    for (m in seq_len(n)) {
      value <- sqrt(m * (2 * n - m) / (2 * n)) *
        (mean(padded[seq_len(m)]) - mean(padded[(m + 1L):(2 * n)]))
      candidates$statistic[candidates$split == k & candidates$m == m] <- value
    }
  }
  candidates
}

mixed <- rbind(c(1, -2, 0, 5, 4, 7), c(3, 1, -1, 0, 2, 1),
               c(-4, -3, -5, -1, 1, 2))
fixtures <- list(panel, both, mixed, mixed[, 3:6, drop = FALSE],
                 matrix(c(0, 2, 0), 1), matrix(c(0L, 4L), 1), matrix(3, 2, 4))
for (x in fixtures) {
  reference <- reference_candidates(x)
  # Check every m/split score, not only the winning entry.
  actual_scores <- as.numeric(double_cusum(cusum_panel(x)))
  stopifnot(isTRUE(all.equal(actual_scores, reference$statistic)))
  winner <- order(-reference$statistic, reference$split, reference$m)[1L]
  actual <- double_cusum_scan(x)
  stopifnot(isTRUE(all.equal(actual$statistic, reference$statistic[winner])),
            actual$split == reference$split[winner], actual$m == reference$m[winner])
}

# Nonzero tied positions must select the earlier split; one split stays valid.
tied <- double_cusum_scan(matrix(c(0, 2, 0), 1))
shortest <- double_cusum_scan(matrix(c(0L, 4L), 1))
stopifnot(isTRUE(all.equal(tied$statistic, 1 / sqrt(3))), tied$split == 1L,
          tied$m == 1L, isTRUE(all.equal(shortest$statistic, 2)),
          shortest$split == 1L, shortest$m == 1L)

for (bad in list(1:4, matrix(numeric(0), 0, 3), matrix(1, 2, 1),
                 matrix(NA_real_, 2, 3), matrix(Inf, 2, 3),
                 matrix(TRUE, 2, 3), matrix("1", 2, 3), matrix(1 + 1i, 2, 3))) {
  caught <- tryCatch({ double_cusum_scan(bad); FALSE }, error = function(e) TRUE)
  stopifnot(caught)
}
cat("All Double CUSUM scan tests passed.\n")
