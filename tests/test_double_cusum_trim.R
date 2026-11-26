source("R/cusum_panel.R")
source("R/double_cusum.R")
source("R/double_cusum_scan.R")
source("R/double_cusum_decision.R")
source("R/double_cusum_segment.R")
source("tests/helpers/reference_candidates.R")
source("tests/helpers/reference_segments.R")

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

# Independent direct-mean scores and a queue rather than recursive calls.
allowed_candidates <- function(x, start, end, scale, trim) {
  tab <- reference_candidates(x[, seq.int(start, end), drop = FALSE], scale)
  tab <- tab[tab$split > trim & tab$split < end - start + 1L - trim, ]
  tab$split <- tab$split + start - 1L
  tab
}
mixed <- rbind(c(1,-2,0,5,4,7,2,-6,9,3),
               c(3,1,-1,0,2,1,8,4,-3,6),
               c(-4,-3,-5,-1,1,2,7,-2,0,5))
n_scores <- n_scans <- n_decisions <- n_trees <- 0L
max_difference <- 0
for (scale in list(rep(1, 3), c(2, 5, 0.5))) {
  for (trim in c(1L, 2L)) {
    for (start in seq_len(ncol(mixed) - 1L)) {
      for (end in seq.int(start + 1L, ncol(mixed))) {
        if (end - start + 1L < 2L * trim + 2L) next
        ref <- allowed_candidates(mixed, start, end, scale, trim)
        scores <- double_cusum(cusum_panel(mixed[, start:end, drop = FALSE], scale))
        selected <- scores[cbind(ref$m, ref$split - start + 1L)]
        difference <- max(abs(selected - ref$statistic))
        stopifnot(difference < 1e-12)
        max_difference <- max(max_difference, difference)
        n_scores <- n_scores + nrow(ref)
        winner <- ref[order(-ref$statistic, ref$split, ref$m)[1L], ]
        actual <- double_cusum_scan(mixed, start, end, scale, trim)
        stopifnot(abs(actual$statistic - winner$statistic) < 1e-12,
                  actual$split == winner$split, actual$m == winner$m)
        n_scans <- n_scans + 1L
        for (cutoff in c(0.75, 2.5, 20)) {
          decision <- double_cusum_decision(mixed, cutoff, start, end, scale, trim)
          passes <- winner$statistic > cutoff
          stopifnot(identical(decision$exceeds_threshold, passes),
                    abs(decision$statistic - winner$statistic) < 1e-12,
                    identical(decision$split, if (passes) as.integer(winner$split) else NA_integer_),
                    identical(decision$m, if (passes) as.integer(winner$m) else NA_integer_))
          n_decisions <- n_decisions + 1L
          expected <- reference_segments(mixed, cutoff, start, end, scale, trim)
          result <- double_cusum_segment(mixed, cutoff, start, end, scale, trim)
          stopifnot(identical(result, expected))
          n_trees <- n_trees + 1L
        }
      }
    }
  }
}
cat(sprintf("PASS: %d scores, %d scans, %d decisions, %d trees; max difference %.17g\n",
            n_scores, n_scans, n_decisions, n_trees, max_difference))
