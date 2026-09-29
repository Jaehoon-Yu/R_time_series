# Exploratory split: maximize abs(mean(right) - mean(left)).
# k is the boundary AFTER observation k, not a significance test.
split_gaps <- function(x, min_segment = 1L) {
  if (!is.numeric(x) || !is.null(dim(x)) || length(x) < 2L ||
      any(!is.finite(x))) {
    stop("x must be a numeric vector with at least two finite values.")
  }
  if (!is.numeric(min_segment) || length(min_segment) != 1L ||
      !is.finite(min_segment) || min_segment < 1 ||
      min_segment != floor(min_segment) || min_segment > length(x) / 2) {
    stop("min_segment must be a positive integer allowing two segments.")
  }
  n <- length(x)
  k <- seq.int(min_segment, n - min_segment)
  left_mean <- vapply(k, function(i) mean(x[seq_len(i)]), numeric(1))
  right_mean <- vapply(k, function(i) mean(x[seq.int(i + 1L, n)]), numeric(1))
  gap <- right_mean - left_mean
  if (any(!is.finite(gap))) {
    stop("Mean differences overflowed; rescale x before analysis.")
  }
  data.frame(k = k, left_mean = left_mean, right_mean = right_mean, gap = gap)
}

find_split <- function(x, min_segment = 1L) {
  scores <- split_gaps(x, min_segment)
  # Do not invent a boundary when every candidate has exactly zero gap.
  if (all(scores$gap == 0)) return(NA_integer_)
  # For tied maxima, choose the earliest candidate.
  as.integer(scores$k[which.max(abs(scores$gap))])
}
