# Input data
x <- c(25, 25, 25, 25, 20, 20, 20, 20)
# x <- c(20, 20, 25, 25, 25, 25, 25, 25)
# x <- c(20, 20, 20, 20, 20, 20, 20, 20)

# Case 1: downward change after position 4
data1 <- c(25, 25, 25, 25, 20, 20, 20, 20)

# Case 2: upward change after position 2
data2 <- c(20, 20, 25, 25, 25, 25)

# Case 3: no change
data3 <- c(20, 20, 20, 20, 20, 20)

#평균 유지, 평균균 차이
data4 <- c(19, 21, 20, 19, 21, 20)

result4 <- find_split(data4)
print(result4)

# Calculate and store the gaps
find_split <- function(x) {
n <- length(x)
gaps <- numeric(n - 1)

for (k in 1:(n-1)) {
  left <- x[1:k]
  right <- x[(k + 1):length(x)]
  
  gap <- mean(right) - mean(left)
  gaps[k] <- gap
}


best_k <- which.max(abs(gaps))
return(best_k)
}

result <- find_split(x)
result1 <- find_split(data1)
result2 <- find_split(data2)
result3 <- find_split(data3)
result4 <- find_split(data4)

print(c(result,result1, result2, result3, result4))


