source("R/center_by_segments.R")
source("R/estimate_residual_scale.R")

# Segment means are (2, 10) in row 1 and (5, 22) in row 2.
panel <- rbind(first = c(1, 3, 8, 12), second = c(3, 7, 18, 26))
colnames(panel) <- paste0("t", 1:4)
expected <- rbind(c(-1, 1, -2, 2), c(-2, 2, -4, 4))
residuals <- center_by_segments(panel, change_points = 2)
stopifnot(isTRUE(all.equal(unname(residuals), expected)),
          identical(dimnames(residuals), dimnames(panel)),
          isTRUE(all.equal(estimate_residual_scale(residuals),
                           c(first = 2.2239, second = 4.4478))))

# An empty split vector keeps one segment; edge splits allow singleton segments.
stopifnot(isTRUE(all.equal(unname(center_by_segments(panel[1, , drop = FALSE])),
                          matrix(c(-5, -3, 2, 6), 1))),
          isTRUE(all.equal(unname(center_by_segments(panel[1, , drop = FALSE],
                                                    c(1, 3))),
                          matrix(c(0, -2.5, 2.5, 0), 1))),
          identical(center_by_segments(matrix(7, 1, 1)), matrix(0, 1, 1)))

expect_error <- function(expr) {
  caught <- tryCatch({ force(expr); FALSE }, error = function(e) TRUE)
  stopifnot(caught)
}
for (bad in list(0, 4, 1.5, NA_real_, Inf, c(2, 2), c(3, 1),
                 TRUE, "2", 2+1i, matrix(2, 1, 1))) {
  expect_error(center_by_segments(panel, bad))
}
for (bad in list(1:4, matrix(numeric(0), 1, 0), matrix(c(1, NA), 1),
                 matrix(TRUE, 1, 2), matrix(1+1i, 1, 2))) {
  expect_error(center_by_segments(bad))
}
cat("All segment centering draft tests passed.\n")
