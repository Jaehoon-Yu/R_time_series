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

# 같은 숫자들을 나눌 수 있는 모든 위치에서 비교함.
comparison <- split_gaps(changed)
# abs_gap은 평균 차이에서 부호를 뺀 크기임.
comparison$abs_gap <- abs(comparison$gap)
print(comparison, row.names = FALSE)

# k가 3, 4인 줄에서 차이를 꺼내 직접 계산한 값과 나란히 놓음.
gap_check <- data.frame(
  k = c(3, 4),
  direct = c(gap3, gap4),
  from_table = c(comparison$gap[comparison$k == 3],
                 comparison$gap[comparison$k == 4])
)
# direct는 직접 계산한 값, from_table은 표에서 찾은 값임.
print(gap_check, row.names = FALSE)

# 이 예제의 표에서 차이의 크기가 가장 큰 줄을 고름.
# which.max는 가장 큰 값이 있는 줄 번호를 알려줌.
best_row <- which.max(comparison$abs_gap)
best_split <- comparison[best_row, ]
print(best_split, row.names = FALSE)
# k가 3이므로 세 번째 숫자 다음에서 나눈다는 뜻임.

# 표에서 고른 위치와 기존 함수가 찾은 위치를 나란히 비교함.
split_check <- c(from_table = best_split$k,
                 from_function = find_split(changed))
print(split_check)
# 이 예제에서는 두 값 모두 3이 나옴.

# 마지막 숫자 하나만 커지면 평균과 고르는 위치도 달라질지 비교함.
# 평균은 올라갈 것으로 예상함. 고르는 위치는 계산해서 확인함.
# 26보다 1 큰 27에서도 같은 위치를 고르는지 살펴봄.
last_values <- c(20, 25, 26, 27, 30, 60)
large_comparison <- data.frame(
  last_value = last_values,
  mean = numeric(length(last_values)),
  right_mean4 = numeric(length(last_values)),
  abs_gap4 = numeric(length(last_values)),
  abs_gap7 = numeric(length(last_values)),
  split_after = integer(length(last_values))
)
# 매번 원래 예제를 복사하고 마지막 숫자만 바꿔 표의 한 줄을 채움.
for (i in seq_along(last_values)) {
  numbers <- examples$downward
  numbers[8] <- last_values[i]
  large_comparison$mean[i] <- mean(numbers)
  large_comparison$right_mean4[i] <- mean(numbers[5:8])
  large_comparison$abs_gap4[i] <- abs(large_comparison$right_mean4[i] - mean(numbers[1:4]))
  large_comparison$abs_gap7[i] <- abs(mean(numbers[8]) - mean(numbers[1:7]))
  large_comparison$split_after[i] <- find_split(numbers)
}
print(large_comparison, row.names = FALSE)
# last_value는 마지막 숫자임. 20인 줄이 원래 예제임.
# mean은 전체 평균, split_after는 몇 번째 숫자 다음에서 나누는지 뜻함.
# right_mean4는 4번째 뒤에서 나눴을 때 오른쪽 네 숫자의 평균임.
# 26을 27로 바꾸면 합이 1 늘고 4로 나누므로, 이 평균은 0.25 올라감.
# abs_gap4와 abs_gap7은 4번째, 7번째 뒤에서 나눴을 때 평균 차이의 크기임.
# 25와 26에서는 위치가 4임. 27, 30, 60에서는 위치가 7로 바뀜.
# 26에서는 4 쪽 차이가 더 크고, 27에서는 7 쪽 차이가 더 큼.
# 숫자 하나로 위치가 바뀔 수 있으므로, 전체 흐름이 바뀌었다고 단정하지 않음.
