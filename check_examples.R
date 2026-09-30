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

# 같은 예제에서 네 번째 숫자만 25에서 20으로 바꿔봄.
changed <- examples$downward
changed[4] <- 20
print(changed)
print(c(original = find_split(examples$downward),
        changed = find_split(changed)))
# 처음에는 4, 바꾼 뒤에는 3이 나옴. 낮아지는 시작점이 한 칸 앞당겨졌음.

# 바꾼 예제를 3번째, 4번째 숫자 뒤에서 나눠 직접 비교함.
gap3 <- mean(changed[4:8]) - mean(changed[1:3])
gap4 <- mean(changed[5:8]) - mean(changed[1:4])
print(c(after3 = gap3, after4 = gap4))
# 차이는 각각 -5, -3.75임. 부호를 빼고 크기를 비교하면 5가 더 큼.
print(c(after3 = abs(gap3), after4 = abs(gap4)))
