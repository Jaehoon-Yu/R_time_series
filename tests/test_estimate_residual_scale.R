source("R/estimate_residual_scale.R")
source("R/cusum_panel.R")

# Hand values exercise each of the three possible maxima.
residuals <- rbind(mad_larger = c(-1, -1, 1, 1),
                   sd_larger = c(-1, -1, -1, 3),
                   flat = c(0, 0, 0, 0))
scale <- estimate_residual_scale(residuals)
stopifnot(isTRUE(all.equal(unname(scale), c(1.4826, 2, 0.01))),
          identical(names(scale), rownames(residuals)),
          identical(estimate_residual_scale(matrix(c(0, 0), 1)), 0.01))

# Separate formulas do not call sd() or mad().
uneven <- rbind(c(-3, -2, -1, 0, 1, 2, 3), c(-3, -3, -3, 1, 1, 1, 6))
reference <- vapply(seq_len(nrow(uneven)), function(j) {
  z <- uneven[j, ]
  sample_sd <- sqrt(sum((z - mean(z))^2) / (length(z) - 1))
  median_deviation <- 1.4826 * median(abs(z - median(z)))
  max(sample_sd, median_deviation, 0.01)
}, numeric(1))
stopifnot(isTRUE(all.equal(estimate_residual_scale(uneven), reference)),
          identical(estimate_residual_scale(matrix(c(0, 0), 1), min_scale = 0.5),
                    0.5))

# Known means are removed before estimation; the raw mean shift inflates scale.
known_means <- rbind(c(0, 0, 4, 4), c(0, 0, 8, 8))
panel <- known_means + rbind(c(-1, 1, -1, 1), c(-2, 2, -2, 2))
estimated <- estimate_residual_scale(panel - known_means)
scores <- cusum_panel(panel, scale = estimated)
expected <- c(-2 * sqrt(3), -4, -2 * sqrt(3)) / 1.4826
stopifnot(isTRUE(all.equal(unname(scores[1, ]), expected)),
          isTRUE(all.equal(scores[1, ], scores[2, ])),
          all(estimate_residual_scale(panel) > estimated))

expect_error <- function(expr) {
  caught <- tryCatch({ force(expr); FALSE }, error = function(e) TRUE)
  stopifnot(caught)
}
for (bad in list(1:4, matrix(numeric(0), 0, 2), matrix(1, 1, 1),
                 matrix(c(1, NA), 1), matrix(c(1, Inf), 1),
                 matrix(c(TRUE, FALSE), 1), matrix(c("1", "2"), 1),
                 matrix(c(1+1i, 2), 1))) {
  expect_error(estimate_residual_scale(bad))
}
for (bad in list(0, -1, NA_real_, Inf, TRUE, "1", numeric(0), c(1, 2),
                 matrix(1, 1, 1))) {
  expect_error(estimate_residual_scale(residuals, min_scale = bad))
}
expect_error(estimate_residual_scale(matrix(c(-1e308, 1e308), 1)))
cat("All residual scale tests passed.\n")
