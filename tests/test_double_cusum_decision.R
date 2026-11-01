source("R/cusum_panel.R")
source("R/double_cusum.R")
source("R/double_cusum_scan.R")
source("R/double_cusum_decision.R")

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

# A flat panel must not turn its arbitrary scan candidate into a change.
flat <- double_cusum_decision(matrix(3, 2, 4), threshold = 0)
stopifnot(flat$statistic == 0, identical(flat$exceeds_threshold, FALSE),
          is.na(flat$split), is.na(flat$m))

# Selected positions still refer to the original input columns.
extended <- cbind(c(100, -100), panel, c(-100, 100))
interval <- double_cusum_decision(extended, threshold = 3, start = 2, end = 5)
stopifnot(interval$statistic == 4, isTRUE(interval$exceeds_threshold),
          interval$split == 3L, interval$m == 2L)

for (bad in list(-1, NA_real_, Inf, c(1, 2), "3", TRUE, 3 + 0i, matrix(3))) {
  caught <- tryCatch({ double_cusum_decision(panel, bad); FALSE },
                     error = function(e) TRUE)
  stopifnot(caught)
}
cat("All Double CUSUM decision draft tests passed.\n")
