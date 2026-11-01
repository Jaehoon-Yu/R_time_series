source("R/cusum_panel.R")
source("R/double_cusum.R")
source("R/double_cusum_scan.R")
source("tests/helpers/reference_candidates.R")

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

# Ignore the surrounding values and map back to input columns.
extended <- cbind(c(100, -100), panel, c(-100, 100))
interval <- double_cusum_scan(extended, start = 2L, end = 5L)
stopifnot(isTRUE(all.equal(interval$statistic, 2 * sqrt(3))),
          interval$split == 3L, interval$m == 1L,
          identical(double_cusum_scan(panel),
                    double_cusum_scan(panel, start = 1L, end = ncol(panel))))
one_row <- double_cusum_scan(matrix(c(100L, 0L, 4L, 100L), 1), 2L, 3L)
stopifnot(isTRUE(all.equal(one_row$statistic, 2)),
          one_row$split == 2L, one_row$m == 1L)

# Every valid interval of this panel gets its own direct-mean reference.
# The reference has local positions; scan must return original input positions.
for (start in seq_len(ncol(mixed) - 1L)) {
  for (end in seq.int(start + 1L, ncol(mixed))) {
    reference <- reference_candidates(mixed[, start:end, drop = FALSE])
    winner <- order(-reference$statistic, reference$split, reference$m)[1L]
    actual <- double_cusum_scan(mixed, start, end)
    stopifnot(isTRUE(all.equal(actual$statistic, reference$statistic[winner])),
              actual$split == start + reference$split[winner] - 1L,
              actual$m == reference$m[winner])
  }
}

# Exact ties and omitted bounds must also respect the original coordinates.
interval_tie <- double_cusum_scan(matrix(c(100, 0, 2, 0, 100), 1), 2, 4)
interval_flat <- double_cusum_scan(matrix(3, 2, 6), 3, 5)
stopifnot(isTRUE(all.equal(interval_tie$statistic, 1 / sqrt(3))),
          interval_tie$split == 2L, interval_tie$m == 1L,
          interval_flat$statistic == 0, interval_flat$split == 3L,
          interval_flat$m == 1L,
          identical(double_cusum_scan(mixed, start = 3L),
                    double_cusum_scan(mixed, 3L, ncol(mixed))),
          identical(double_cusum_scan(mixed, end = 4L),
                    double_cusum_scan(mixed, 1L, 4L)),
          identical(double_cusum_scan(mixed, 2, 5),
                    double_cusum_scan(mixed, 2L, 5L)))

for (bounds in list(c(0, 4), c(2, 2), c(4, 2), c(1, 5), c(1.5, 4))) {
  caught <- tryCatch({ double_cusum_scan(panel, bounds[1], bounds[2]); FALSE },
                     error = function(e) TRUE)
  stopifnot(caught)
}
for (bad_bound in list(NULL, numeric(0), c(1, 2), NA_real_, NaN,
                       Inf, -Inf, TRUE, "2", 2 + 0i)) {
  bad_start <- tryCatch({ double_cusum_scan(panel, start = bad_bound); FALSE },
                        error = function(e) TRUE)
  bad_end <- tryCatch({ double_cusum_scan(panel, end = bad_bound); FALSE },
                      error = function(e) TRUE)
  stopifnot(bad_start, bad_end)
}
cat("All Double CUSUM scan tests passed.\n")
