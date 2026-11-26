source("R/cusum_panel.R")
source("R/double_cusum.R")
source("R/double_cusum_scan.R")
source("R/double_cusum_decision.R")
source("R/double_cusum_segment.R")

# Removing candidate 1 must keep observation 1 in the CUSUM calculation.
edge <- rbind(c(10, 0, 0, 0, 0, 0), c(10, 0, 0, 0, 0, 0))
trimmed <- double_cusum_scan(edge, trim = 1)
stopifnot(double_cusum_scan(edge)$split == 1L,
          identical(double_cusum_scan(edge), double_cusum_scan(edge, trim = 0L)),
          trimmed$split == 2L, trimmed$m == 2L,
          isTRUE(all.equal(trimmed$statistic, 10 / sqrt(3))),
          !double_cusum_decision(edge, threshold = 6, trim = 1)$exceeds_threshold,
          double_cusum_decision(edge, threshold = 6)$exceeds_threshold)

# Ties must not select excluded positions; a single candidate keeps matrix shape.
flat <- double_cusum_scan(matrix(3, 2, 6), trim = 1)
only <- double_cusum_scan(matrix(c(0, 0, 4, 4), 1), trim = 1)
stopifnot(flat$statistic == 0, flat$split == 2L, flat$m == 1L,
          only$split == 2L, only$m == 1L,
          isTRUE(all.equal(only$statistic, 2 * sqrt(2))))
extended <- cbind(c(100, -100), edge, c(-100, 100))
shifted <- double_cusum_scan(extended, start = 2, end = 7, trim = 1)
stopifnot(shifted$split == 3L, shifted$m == trimmed$m,
          identical(shifted$statistic, trimmed$statistic))

# Both children have boundary peaks. Each child must also apply trim and scale.
branch <- c(8, 0, 0, 0, 0, 20, 20, 20, 20, 28)
branches <- rbind(branch, branch)
stopifnot(identical(double_cusum_segment(branches, 3, trim = 1), c(2L, 5L, 8L)),
          identical(double_cusum_segment(branches, 1.5, scale = c(2, 2), trim = 1),
                    c(2L, 5L, 8L)))

expect_error <- function(expr) {
  caught <- tryCatch({ force(expr); FALSE }, error = function(e) TRUE)
  stopifnot(caught)
}
for (bad in list(-1, 0.5, NA_real_, Inf, numeric(0), c(1, 2),
                 TRUE, "1", 1+0i, matrix(1))) {
  expect_error(double_cusum_scan(edge, trim = bad))
}
expect_error(double_cusum_scan(matrix(0, 2, 3), trim = 1))
expect_error(double_cusum_decision(edge, 0, trim = 3))
expect_error(double_cusum_segment(edge, 0, trim = 3))
cat("All Double CUSUM trim draft tests passed.\n")
