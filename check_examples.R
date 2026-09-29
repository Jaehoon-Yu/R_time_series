# R_time_series.Rproj를 열고 실행함.
source("R/find_split.R")

examples <- list(
  downward = c(25, 25, 25, 25, 20, 20, 20, 20),
  upward = c(20, 20, 25, 25, 25, 25),
  constant = rep(20, 8),
  fluctuating = c(19, 21, 20, 19, 21, 20)
)
# 예제를 하나씩 계산함. 결과 4는 네 번째 숫자 다음에서 나눈다는 뜻임.
positions <- integer(length(examples))
for (i in seq_along(examples)) {
  positions[i] <- find_split(examples[[i]])
}
results <- data.frame(example = names(examples), split_after = positions)
print(results, row.names = FALSE)

# 첫 예제를 4번째 숫자 다음에서 직접 나눠 확인함.
left <- examples$downward[1:4]
right <- examples$downward[5:8]
print(c(left_mean = mean(left), right_mean = mean(right),
        gap = mean(right) - mean(left)))
# 왼쪽 평균 25, 오른쪽 평균 20이므로 차이는 -5임.

# 숫자가 흔들리기만 해도 차이가 큰 위치는 나올 수 있음.
print(split_gaps(examples$fluctuating), row.names = FALSE)
