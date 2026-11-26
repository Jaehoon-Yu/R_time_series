# Independent control flow: a queue of intervals, with direct means for scores.
# No production functions are called by this reference.
reference_segments <- function(x, threshold, start = 1L, end = ncol(x),
                               scale = rep(1, nrow(x)), trim = 0L) {
  pending <- list(c(start, end))
  next_interval <- 1L
  changes <- integer(0)
  while (next_interval <= length(pending)) {
    bounds <- pending[[next_interval]]
    next_interval <- next_interval + 1L
    if (bounds[2L] - bounds[1L] + 1L < 2L * trim + 2L) next
    candidates <- reference_candidates(x[, bounds[1L]:bounds[2L], drop = FALSE],
                                       scale = scale)
    candidates <- candidates[candidates$split > trim &
      candidates$split < bounds[2L] - bounds[1L] + 1L - trim, ]
    winner <- order(-candidates$statistic, candidates$split, candidates$m)[1L]
    if (candidates$statistic[winner] > threshold) {
      split <- bounds[1L] + candidates$split[winner] - 1L
      changes <- c(changes, split)
      pending <- c(pending, list(c(bounds[1L], split),
                                c(split + 1L, bounds[2L])))
    }
  }
  sort(as.integer(changes))
}
