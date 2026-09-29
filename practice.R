# Run from the project root. Keep this explicit loop for learning.
source("R/find_split.R")

before <- c(19, 20, 21, 20)
after <- c(24, 25, 26, 25)

difference <- mean(after) - mean(before)

print(difference)

before1 <- c(19, 20, 21, 20)
after1 <- c(24+2, 25+2, 26+2, 25+2)

difference1 <- mean(after1) - mean(before1)

print(difference1)

# Combine the observations
x <- c(before, after)
print(x)

n <- length(x)
gaps <- numeric(n - 1L)

for (k in seq_len(n - 1L)) {
  # Commands to repeat
  left <- x[1:k]
  right <- x[(k + 1):length(x)]
  
  print(left)
  print(right)
  
  # Compare the two means
  gap <- mean(right) - mean(left)
  gaps[k] <- gap
  print(c(k = k,gap = gap))
}

print(gaps)

best_k <- find_split(x)
print(best_k)

# The reusable function must agree with the step-by-step calculation.
stopifnot(isTRUE(all.equal(gaps, split_gaps(x)$gap)))
