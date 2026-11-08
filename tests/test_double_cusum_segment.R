source("R/cusum_panel.R")
source("R/double_cusum.R")
source("R/double_cusum_scan.R")
source("R/double_cusum_decision.R")
source("R/double_cusum_segment.R")

# The whole interval chooses 2; rescanning its right child chooses original 4.
# Both rows agree, so the combined score equals one row's CUSUM magnitude.
# Root score is 4/sqrt(3), then the right child score is 4.
panel <- rbind(c(0, 0, 4, 4, 0, 0), c(0, 0, 4, 4, 0, 0))
stopifnot(identical(double_cusum_segment(panel, 2), c(2L, 4L)))
# Stopping at the root must stop the entire search, even if a child could pass.
stopifnot(identical(double_cusum_segment(panel, 3), integer(0)))

single <- rbind(c(0, 0, 4, 4), c(0, 0, 4, 4))
stopifnot(identical(double_cusum_segment(single, 3), 2L),
          identical(double_cusum_segment(single, 4), integer(0)),
          identical(double_cusum_segment(matrix(3, 2, 6), 0), integer(0)))

# A two-time input creates one-time children, which must not be scanned.
stopifnot(identical(double_cusum_segment(matrix(c(0, 4), 1), 1), 1L))
# Both returned changes must retain the coordinates of the original input.
extended <- cbind(c(100, -100), panel, c(-100, 100))
stopifnot(identical(double_cusum_segment(extended, 2, 2, 7), c(3L, 5L)))

bad_calls <- list(list(x = panel, threshold = -1),
                  list(x = panel, threshold = NA_real_),
                  list(x = panel, threshold = TRUE),
                  list(x = matrix(1, 2, 1), threshold = 0),
                  list(x = panel, threshold = 2, start = 2, end = 2),
                  list(x = panel, threshold = 2, end = 7))
for (args in bad_calls) {
  caught <- tryCatch({ do.call(double_cusum_segment, args); FALSE },
                    error = function(e) TRUE)
  stopifnot(caught)
}
cat("double_cusum_segment checks passed\n")
