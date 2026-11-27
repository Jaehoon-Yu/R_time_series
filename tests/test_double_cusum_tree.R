source("R/cusum_panel.R")
source("R/double_cusum.R")
source("R/double_cusum_scan.R")
source("R/double_cusum_tree.R")
source("R/center_by_segments.R")

# Root chooses 4 with score 16/sqrt(3); its left child chooses 2 with score 4.
panel <- rbind(c(0, 0, 4, 4, 10, 10), c(0, 0, 4, 4, 10, 10))
root <- double_cusum_tree(panel, height = 1, trim = 1)
tree <- double_cusum_tree(panel, height = 2, trim = 1)
stopifnot(identical(root$change_points, 4L),
          identical(tree$change_points, c(2L, 4L)),
          identical(tree$nodes$level, c(1L, 2L)),
          identical(tree$nodes$start, c(1L, 1L)),
          identical(tree$nodes$end, c(6L, 4L)),
          identical(tree$nodes$split, c(4L, 2L)),
          isTRUE(all.equal(tree$nodes$statistic, c(16/sqrt(3), 4))),
          identical(center_by_segments(panel, tree$change_points), matrix(0, 2, 6)))

# Both children are included at depth 2; their short children must stop.
branch <- c(8, 0, 0, 0, 0, 20, 20, 20, 20, 28)
both <- double_cusum_tree(rbind(branch, branch), height = 2, trim = 1)
stopifnot(identical(both$change_points, c(2L, 5L, 8L)),
          identical(both$nodes$level, c(1L, 2L, 2L)),
          identical(both$nodes$start, c(1L, 1L, 6L)),
          identical(both$nodes$end, c(10L, 5L, 10L)),
          identical(double_cusum_tree(rbind(branch, branch), 5, trim = 1), both))

# Even zero scores produce preparation candidates. These are not detections.
flat <- double_cusum_tree(matrix(3, 2, 8), height = 2, trim = 1)
stopifnot(identical(flat$change_points, c(2L, 4L)),
          all(flat$nodes$statistic == 0), all(flat$nodes$m == 1L))
single <- double_cusum_tree(matrix(c(0, 4), 1), height = 3)
stopifnot(identical(single$change_points, 1L), nrow(single$nodes) == 1L,
          identical(single$nodes$level, 1L))

expect_error <- function(expr) {
  caught <- tryCatch({ force(expr); FALSE }, error = function(e) TRUE)
  stopifnot(caught)
}
for (bad in list(0, -1, 1.5, NA_real_, Inf, numeric(0), c(1, 2),
                 TRUE, "2", 2+0i, matrix(2), 2^31)) {
  expect_error(double_cusum_tree(panel, bad, trim = 1))
}
expect_error(double_cusum_tree(matrix(0, 2, 3), 1, trim = 1))
expect_error(double_cusum_tree(panel, 1, trim = -1))
expect_error(double_cusum_tree(matrix(c(0, NA), 1), 1))
cat("All initial Double CUSUM tree draft tests passed.\n")
