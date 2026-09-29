# No extra packages required; run from the repository root.
source("R/find_split.R")
expect_error <- function(expr) {
  caught <- tryCatch({ force(expr); FALSE }, error = function(e) TRUE)
  stopifnot(caught)
}

# Known boundaries, sign, constant input and minimum length.
stopifnot(
  identical(find_split(c(25, 25, 25, 25, 20, 20, 20, 20)), 4L),
  identical(find_split(c(20, 20, 25, 25, 25, 25)), 2L),
  identical(find_split(rep(20, 8)), NA_integer_),
  identical(find_split(c(1, 3)), 1L),
  identical(split_gaps(c(1, 3))$gap, 2),
  identical(find_split(c(0, 1, 0)), 1L),
  identical(find_split(c(0, 0, 10, 10), min_segment = 2), 2L),
  identical(as.integer(split_gaps(1:6, 2)$k), 2:4)
)

# Translation and sign changes preserve the winning boundary.
x <- c(2, 3, 1, 9, 8, 10)
stopifnot(find_split(x) == 3L,
          find_split(x + 100) == find_split(x),
          find_split(-x) == find_split(x))

for (bad in list(numeric(0), 1, c(1, NA), c(1, NaN), c(1, Inf),
                 c("1", "2"), matrix(1:4, 2), list(1, 2))) {
  expect_error(find_split(bad))
}
for (bad in list(0, -1, 1.5, NA_real_, Inf, c(1, 2), "2", 4)) {
  expect_error(find_split(1:6, bad))
}
expect_error(find_split(c(-1e308, 1e308)))
cat("All split tests passed.\n")
