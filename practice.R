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

gaps <- numeric(7)

for (k in 1:7) {
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

best_k <- which.max(abs(gaps))
print(best_k)