# Open R_time_series.Rproj first, or run from the repository root.
source("R/find_split.R")

examples <- list(
  downward = c(25, 25, 25, 25, 20, 20, 20, 20),
  upward = c(20, 20, 25, 25, 25, 25),
  constant = rep(20, 8),
  fluctuating = c(19, 21, 20, 19, 21, 20)
)
results <- data.frame(
  example = names(examples),
  split_after = vapply(examples, find_split, integer(1))
)
print(results, row.names = FALSE)

# A nonzero gap in fluctuating data does NOT prove a structural change.
print(split_gaps(examples$fluctuating), row.names = FALSE)
