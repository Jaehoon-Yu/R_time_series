source("R/cusum_panel.R")
source("R/double_cusum.R")
source("R/double_cusum_scan.R")
source("R/double_cusum_decision.R")
source("tests/helpers/reference_candidates.R")

# The two rows have middle CUSUM magnitudes 4 and 4.
# Combining both gives sqrt(2 * 2 / 4) * (4 - 0) = 4.
panel <- rbind(c(0, 0, 4, 4), c(0, 0, 4, 4))
accepted <- double_cusum_decision(panel, threshold = 3)
equal <- double_cusum_decision(panel, threshold = 4)
below <- double_cusum_decision(panel, threshold = 5)
stopifnot(accepted$statistic == 4, accepted$threshold == 3,
          isTRUE(accepted$exceeds_threshold), accepted$split == 2L,
          accepted$m == 2L,
          identical(equal$exceeds_threshold, FALSE), is.na(equal$split),
          is.na(equal$m), identical(below$exceeds_threshold, FALSE),
          is.na(below$split), is.na(below$m))
stopifnot(identical(equal$split, NA_integer_), identical(equal$m, NA_integer_),
          identical(below$split, NA_integer_), identical(below$m, NA_integer_),
          equal$statistic == 4, equal$threshold == 4,
          below$statistic == 4, below$threshold == 5)

# A flat panel must not turn its arbitrary scan candidate into a change.
flat <- double_cusum_decision(matrix(3, 2, 4), threshold = 0)
stopifnot(flat$statistic == 0, identical(flat$exceeds_threshold, FALSE),
          is.na(flat$split), is.na(flat$m))

# Selected positions still refer to the original input columns.
extended <- cbind(c(100, -100), panel, c(-100, 100))
interval <- double_cusum_decision(extended, threshold = 3, start = 2, end = 5)
stopifnot(interval$statistic == 4, isTRUE(interval$exceeds_threshold),
          interval$split == 3L, interval$m == 2L)

# Compare the whole decision with direct means on every interval of one panel.
# Keep thresholds well away from rounding differences between the two methods.
# Exact equality is checked above with the hand-calculated score 4.
mixed <- rbind(c(1, -2, 0, 5, 4, 7), c(3, 1, -1, 0, 2, 1),
               c(-4, -3, -5, -1, 1, 2))
for (start in seq_len(ncol(mixed) - 1L)) {
  for (end in seq.int(start + 1L, ncol(mixed))) {
    reference <- reference_candidates(mixed[, start:end, drop = FALSE])
    winner <- order(-reference$statistic, reference$split, reference$m)[1L]
    score <- reference$statistic[winner]
    stopifnot(score > 0)
    for (accept in c(TRUE, FALSE)) {
      cutoff <- if (accept) score / 2 else 2 * score
      actual <- double_cusum_decision(mixed, cutoff, start, end)
      stopifnot(isTRUE(all.equal(actual$statistic, score)),
                identical(actual$threshold, cutoff),
                identical(actual$exceeds_threshold, accept))
      if (accept) {
        stopifnot(actual$split == start + reference$split[winner] - 1L,
                  actual$m == reference$m[winner])
      } else {
        stopifnot(identical(actual$split, NA_integer_),
                  identical(actual$m, NA_integer_))
      }
    }
  }
}

# One row and two times must keep their matrix shape; integer cutoffs work too.
shortest <- double_cusum_decision(matrix(c(0L, 4L), 1), threshold = 1L)
stopifnot(isTRUE(all.equal(shortest$statistic, 2)), shortest$threshold == 1,
          isTRUE(shortest$exceeds_threshold), shortest$split == 1L,
          shortest$m == 1L)

for (bad in list(-1, NA_real_, Inf, c(1, 2), "3", TRUE, 3 + 0i, matrix(3),
                 NULL, numeric(0), NaN, -Inf)) {
  caught <- tryCatch({ double_cusum_decision(panel, bad); FALSE },
                     error = function(e) TRUE)
  stopifnot(caught)
}
# Invalid data and intervals must still be rejected through the decision helper.
bad_calls <- list(list(x = 1:4), list(x = matrix(NA_real_, 2, 4)),
                  list(x = panel, start = 0), list(x = panel, end = 5),
                  list(x = panel, start = 2, end = 2))
for (args in bad_calls) {
  caught <- tryCatch({
    do.call(double_cusum_decision, c(args, list(threshold = 3)))
    FALSE
  }, error = function(e) TRUE)
  stopifnot(caught)
}
cat("All Double CUSUM decision tests passed (30 independent interval decisions).\n")
