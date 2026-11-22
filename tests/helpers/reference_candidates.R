# Independent reference: direct means (no cumulative sums or production calls).
# Appending n zeros makes the second group size 2*n-m, as in the DC formula.
reference_candidates <- function(x, scale = rep(1, nrow(x))) {
  n <- nrow(x)
  nt <- ncol(x)
  candidates <- expand.grid(m = seq_len(n), split = seq_len(nt - 1L))
  candidates$statistic <- NA_real_
  for (k in seq_len(nt - 1L)) {
    scores <- numeric(n)
    for (j in seq_len(n)) {
      scores[j] <- sqrt(k * (nt - k) / nt) *
        (mean(x[j, seq_len(k)]) - mean(x[j, (k + 1L):nt])) / scale[j]
    }
    padded <- c(sort(abs(scores), decreasing = TRUE), rep(0, n))
    for (m in seq_len(n)) {
      value <- sqrt(m * (2 * n - m) / (2 * n)) *
        (mean(padded[seq_len(m)]) - mean(padded[(m + 1L):(2 * n)]))
      candidates$statistic[candidates$split == k & candidates$m == m] <- value
    }
  }
  candidates
}
