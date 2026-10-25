source("R/cusum_panel.R")
source("R/double_cusum.R")
source("R/double_cusum_scan.R")

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
cat("Double CUSUM scan draft checks passed.\n")
