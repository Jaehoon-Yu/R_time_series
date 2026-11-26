source("R/cusum_panel.R")
source("R/double_cusum.R")
source("R/double_cusum_scan.R")
source("R/double_cusum_decision.R")
source("R/double_cusum_segment.R")
source("tests/helpers/reference_candidates.R")

source("tests/helpers/reference_segments.R")

# The whole interval chooses 2; rescanning its right child chooses original 4.
# Both rows agree, so the combined score equals one row's CUSUM magnitude.
# Root score is 4/sqrt(3), then the right child score is 4.
panel <- rbind(c(0, 0, 4, 4, 0, 0), c(0, 0, 4, 4, 0, 0))
stopifnot(identical(double_cusum_segment(panel, 2), c(2L, 4L)))
# Stopping at the root must stop the entire search, even if a child could pass.
stopifnot(identical(double_cusum_segment(panel, 3), integer(0)))

# Root chooses 4 (score 16/sqrt(3)); the left child then chooses 2 (score 4).
left_panel <- rbind(c(0, 0, 4, 4, 10, 10), c(0, 0, 4, 4, 10, 10))
stopifnot(identical(double_cusum_segment(left_panel, 3), c(2L, 4L)),
          identical(reference_segments(left_panel, 3), c(2L, 4L)),
          identical(reference_segments(panel, 2), c(2L, 4L)),
          identical(reference_segments(panel, 3), integer(0)))

# Scaling must reach the root and BOTH recursive branches unchanged.
# Root score is 52/15; the smaller change's child score is 13*sqrt(3)/15 < 3.
# Omitting scale in either child would restore score 4 and add a false split.
row_scale <- c(2, 5)
scaled_scan <- double_cusum_scan(left_panel, scale = row_scale)
scaled_decision <- double_cusum_decision(left_panel, 3, scale = row_scale)
stopifnot(isTRUE(all.equal(scaled_scan$statistic, 52 / 15)),
          scaled_scan$split == 4L, scaled_scan$m == 1L,
          isTRUE(scaled_decision$exceeds_threshold), scaled_decision$split == 4L,
          !double_cusum_decision(left_panel, 4, scale = row_scale)$exceeds_threshold,
          identical(double_cusum_segment(left_panel, 3, scale = row_scale), 4L),
          identical(double_cusum_segment(left_panel[, 6:1], 3,
                                          scale = row_scale), 2L))
scaled_extended <- cbind(c(100, -100), left_panel, c(-100, 100))
stopifnot(identical(double_cusum_segment(scaled_extended, 3, 2, 7,
                                        scale = row_scale), 5L))

single <- rbind(c(0, 0, 4, 4), c(0, 0, 4, 4))
stopifnot(identical(double_cusum_segment(single, 3), 2L),
          identical(double_cusum_segment(single, 4), integer(0)),
          identical(double_cusum_segment(matrix(3, 2, 6), 0), integer(0)))

# A two-time input creates one-time children, which must not be scanned.
stopifnot(identical(double_cusum_segment(matrix(c(0, 4), 1), 1), 1L))
# Both returned changes must retain the coordinates of the original input.
extended <- cbind(c(100, -100), panel, c(-100, 100))
stopifnot(identical(double_cusum_segment(extended, 2, 2, 7), c(3L, 5L)))

# Compare complete splitting results on all 15 intervals at three cutoffs.
# Reuse the irregular panel from the decision checks, now checking the tree.
mixed <- rbind(c(1, -2, 0, 5, 4, 7), c(3, 1, -1, 0, 2, 1),
               c(-4, -3, -5, -1, 1, 2))
for (row_scale in list(rep(1, nrow(mixed)), c(2, 5, 0.5))) {
  for (start in seq_len(ncol(mixed) - 1L)) {
    for (end in seq.int(start + 1L, ncol(mixed))) {
      for (cutoff in c(0.75, 2.5, 20)) {
        expected <- reference_segments(mixed, cutoff, start, end, scale = row_scale)
        actual <- double_cusum_segment(mixed, cutoff, start, end, scale = row_scale)
        stopifnot(identical(actual, expected))
      }
    }
  }
}

bad_calls <- list(list(x = panel, threshold = -1),
                  list(x = panel, threshold = NA_real_),
                  list(x = panel, threshold = TRUE),
                  list(x = matrix(1, 2, 1), threshold = 0),
                  list(x = panel, threshold = 2, start = 2, end = 2),
                  list(x = panel, threshold = 2, end = 7),
                  list(x = panel, threshold = 2, scale = c(0, 1)),
                  list(x = panel, threshold = 2, scale = 1))
for (args in bad_calls) {
  caught <- tryCatch({ do.call(double_cusum_segment, args); FALSE },
                    error = function(e) TRUE)
  stopifnot(caught)
}
cat("double_cusum_segment checks passed (90 independent interval comparisons)\n")
