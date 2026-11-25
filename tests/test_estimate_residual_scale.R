source("R/estimate_residual_scale.R")

# Hand values exercise each of the three possible maxima.
residuals <- rbind(mad_larger = c(-1, -1, 1, 1),
                   sd_larger = c(-1, -1, -1, 3),
                   flat = c(0, 0, 0, 0))
scale <- estimate_residual_scale(residuals)
stopifnot(isTRUE(all.equal(unname(scale), c(1.4826, 2, 0.01))),
          identical(names(scale), rownames(residuals)),
          identical(estimate_residual_scale(matrix(c(0, 0), 1)), 0.01))
cat("Residual scale draft checks passed.\n")
