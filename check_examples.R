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
# 7번째 뒤에서는 오른쪽 숫자가 하나뿐이라 last_value가 그쪽 평균임.
# 같은 26에서 27로의 변화가 이 평균에는 그대로 1 반영됨.
# abs_gap4와 abs_gap7은 4번째, 7번째 뒤에서 나눴을 때 평균 차이의 크기임.
# 이 두 예제에서는 왼쪽 평균이 그대로라 4 쪽 차이는 0.25 줄고, 7 쪽은 1 늘어남.
# 25와 26에서는 위치가 4임. 27, 30, 60에서는 위치가 7로 바뀜.
# 26에서는 4 쪽 차이가 더 크고, 27에서는 7 쪽 차이가 더 큼.
# 숫자 하나로 위치가 바뀔 수 있으므로, 전체 흐름이 바뀌었다고 단정하지 않음.

# 마지막 숫자가 40이면 4번째 뒤에서 나눈 양쪽 평균이 같을 것으로 예상함.
# 40에서 39나 41로 바꾸면 오른쪽 평균만 0.25 내려가거나 올라갈 것으로 예상함.
# 마지막 숫자만 바꾸고, 기존 min_segment 설정을 비교함.
# 같은 숫자라도 4번째 대신 5번째 뒤에서 나누면 평균 차이가 달라질 것으로 예상함.
min_size_values <- c(27, 39, 40, 41, 60)
min_size_comparison <- data.frame(
  last_value = min_size_values,
  min_one = integer(length(min_size_values)),
  min_two = integer(length(min_size_values)),
  min_three = integer(length(min_size_values)),
  min_four = integer(length(min_size_values)),
  gap_four = numeric(length(min_size_values)),
  gap_five = numeric(length(min_size_values))
)
for (i in seq_along(min_size_values)) {
  numbers <- examples$downward
  numbers[8] <- min_size_values[i]
  min_size_comparison$min_one[i] <- find_split(numbers, min_segment = 1)
  min_size_comparison$min_two[i] <- find_split(numbers, min_segment = 2)
  min_size_comparison$min_three[i] <- find_split(numbers, min_segment = 3)
  min_size_comparison$min_four[i] <- find_split(numbers, min_segment = 4)
  min_size_comparison$gap_four[i] <- mean(numbers[5:8]) - mean(numbers[1:4])
  min_size_comparison$gap_five[i] <- mean(numbers[6:8]) - mean(numbers[1:5])
}
print(min_size_comparison, row.names = FALSE)
# min_one은 양쪽에 최소 한 개, min_two는 최소 두 개씩 남기는 경우임.
# min_three는 최소 세 개씩 남기는 경우임. 3, 4, 5번째 뒤에서만 나눌 수 있음.
# min_four는 최소 네 개씩 남기는 경우임. 4번째 뒤의 평균만 비교함.
# 같은 위치를 골라도 큰 숫자의 영향이 없어졌다는 뜻은 아님.
# gap_four는 4번째 뒤에서 나눠 오른쪽 평균에서 왼쪽 평균을 뺀 값임.
# gap_five는 같은 숫자를 5번째 뒤에서 나눠 구한 차이임.
# 39인 줄은 gap_four가 -0.25, gap_five가 약 2.33으로 부호도 달라짐.
# 41인 줄은 0.25와 3으로 둘 다 양수임. 위치를 옮겨도 부호가 꼭 바뀌는 건 아님.
# 4번째 뒤에서 나눌 때 오른쪽 평균은 (60 + 마지막 숫자) / 4임.
# 왼쪽 평균 25를 빼면 (마지막 숫자 - 40) / 4라서 40일 때 차이가 0임.
# 27일 때는 -3.25, 60일 때는 5임. 음수면 오른쪽 평균이 더 작고 양수면 더 큼.
# 40일 때는 양쪽 평균이 25라 차이가 0이고, min_four는 NA가 나옴.
# 여기서 NA는 고른 위치가 없다는 뜻임. 숫자가 모두 같거나 변화가 없다는 뜻은 아님.
# 39와 41일 때 오른쪽 평균은 24.75, 25.25라 차이는 -0.25, 0.25임.
# 두 경우 모두 min_four는 4를 고름. 위치만으로 차이의 방향이나 크기를 알 수는 없음.
# 같은 40 예제를 5번째 뒤에서 나누면 왼쪽 평균은 120/5 = 24임.
# 오른쪽 평균은 80/3으로 약 26.67이라 차이는 약 2.67임.
# 최소 세 개 설정은 3, 4번째 뒤의 차이 0보다 이 차이가 커서 5를 고름.

# 같은 39 예제에서 끝의 두 숫자 자리만 바꿔봄.
# 전체 평균은 같을 것으로 예상함. 고르는 위치도 같은지 비교함.
before_swap <- examples$downward
before_swap[8] <- 39
after_swap <- before_swap
after_swap[7] <- before_swap[8]
after_swap[8] <- before_swap[7]
print(c(before = mean(before_swap), after = mean(after_swap)))
print(c(before = find_split(before_swap), after = find_split(after_swap)))
# 기본 설정에서는 양쪽에 최소 한 개씩 남김.
# 전체 평균은 둘 다 24.875지만 고른 위치는 7에서 6으로 바뀜.
print(c(before_min_two = find_split(before_swap, min_segment = 2),
        after_min_two = find_split(after_swap, min_segment = 2)))
# 최소 두 개씩 남기면 7번째 뒤는 제외되고, 두 경우 모두 위치 6을 고름.
# 이때 바꾼 두 숫자가 같은 오른쪽 구간에 남아 그 구간 평균은 같음.

# 이번에는 6번째와 7번째의 20, 39를 바꾸고 6번째 뒤에서만 나눠봄.
# 39가 왼쪽으로 옮겨가면 왼쪽 평균은 커지고 오른쪽 평균은 작아질 것으로 예상함.
across_swap <- after_swap
across_swap[6] <- after_swap[7]
across_swap[7] <- after_swap[6]
print(c(before_gap6 = mean(after_swap[7:8]) - mean(after_swap[1:6]),
        after_gap6 = mean(across_swap[7:8]) - mean(across_swap[1:6])))
# 오른쪽 평균에서 왼쪽 평균을 뺀 차이가 약 6.17에서 -6.5로 바뀜.
# 최소 세 개씩 남기면 위치 6이 빠져 고르는 위치도 달라질 것으로 예상함.
for (minimum in c(2, 3)) {
  print(c(minimum = minimum,
          before = find_split(after_swap, min_segment = minimum),
          after = find_split(across_swap, min_segment = minimum)))
}
# 차이의 방향은 바뀌었지만, 최소 두 개씩 남기면 둘 다 위치 6을 고름.
# 최소 세 개씩 남기면 3, 4, 5번째 뒤만 비교하고 둘 다 위치 5를 고름.
# 5번째 뒤에서는 바꾼 20과 39가 둘 다 오른쪽 구간에 남음.
# 두 경우 모두 왼쪽 평균은 24, 오른쪽은 79/3(약 26.33)이라 차이는 약 2.33임.

# 5번째 뒤에서 나누고, 이번에는 그 앞뒤의 20과 39를 바꿔봄.
# 39가 왼쪽으로 옮겨가면 왼쪽 평균이 더 커질 것으로 예상함.
near_swap <- across_swap
near_swap[5] <- across_swap[6]
near_swap[6] <- across_swap[5]
print(c(before_gap5 = mean(across_swap[6:8]) - mean(across_swap[1:5]),
        after_gap5 = mean(near_swap[6:8]) - mean(near_swap[1:5])))
# 같은 위치에서 비교한 차이가 약 2.33에서 -7.8로 바뀜.
# 최소 네 개씩 남기면 4번째 뒤만 비교할 수 있을 것으로 예상함.
for (minimum in c(3, 4)) {
  print(c(minimum = minimum,
          before = find_split(across_swap, min_segment = minimum),
          after = find_split(near_swap, min_segment = minimum)))
}
# 최소 세 개씩 남기면 3, 4, 5번째 뒤를 비교하고, 두 경우 모두 위치 5를 고름.
# 이때 고른 위치는 같지만 평균 차이의 방향과 크기는 달랐음.
# 최소 네 개씩 남기면 위치 5는 빠지고, 두 경우 모두 위치 4를 고름.
# 4번째 뒤에서는 바꾼 20과 39가 모두 오른쪽 구간에 남음.
# 두 경우 모두 왼쪽 평균은 100/4 = 25, 오른쪽은 99/4 = 24.75라 차이는 -0.25임.
# 최소 개수만 세 개에서 네 개로 바꿔도, 같은 4번째 뒤의 평균은 그대로임.

# 최소 네 개 조건은 그대로 두고, 4번째의 25와 5번째 숫자의 자리만 바꿔봄.
# 39 대신 29를 쓰면 자리 교환에 따른 평균 차이의 변화폭도 작을 것으로 예상함.
for (large_value in c(39, 29, 30)) {
  swap_before <- near_swap
  swap_before[5] <- large_value
  swap_after <- swap_before
  swap_after[4] <- swap_before[5]
  swap_after[5] <- swap_before[4]
  before_four <- split_gaps(swap_before, min_segment = 4)
  after_four <- split_gaps(swap_after, min_segment = 4)
  print(c(value = large_value, before_gap = before_four$gap, after_gap = after_four$gap))
}
# 39일 때 평균 차이는 -0.25에서 -7.25로, 29일 때는 -2.75에서 -4.75로 바뀜.
# 이 두 예제에서 자리 교환에 따른 평균 차이의 변화폭은 7과 2였음.
# 29를 30으로 바꾸면 평균 차이는 -2.5에서 -5로 바뀌어 변화폭은 2.5임.
# 30과 25의 차이 5를 네 개씩 나누므로 양쪽 평균은 각각 5/4 = 1.25씩 움직임.

# 마지막 반복의 30 예제에서 끝의 20 하나만 빼고 4번째 뒤에서 나눠봄.
# 오른쪽 숫자가 세 개로 줄면 같은 자리 교환의 영향이 더 클 것으로 예상함.
print(c(before_three = mean(swap_before[5:7]) - mean(swap_before[1:4]),
        after_three = mean(swap_after[5:7]) - mean(swap_after[1:4])))
# 평균 차이는 약 -1.67에서 -4.58로 바뀌어, 변화폭은 약 2.92였음.
# 왼쪽 합은 100에서 105가 되어 평균이 5/4 = 1.25 커짐.
# 오른쪽 합은 70에서 65가 되어 평균이 5/3만큼 작아짐. 차이의 변화폭은 5/4 + 5/3임.

# 같은 일곱 개에서 양쪽에 최소 세 개씩 남기면 3, 4번째 뒤를 비교함.
# 3번째 뒤를 함께 비교해도, 두 경우 모두 4번째 뒤의 차이 크기가 더 클 것으로 예상함.
# 숫자는 그대로 두고, 최소 개수만 세 개와 두 개로 바꿔 비교함.
for (minimum in c(3, 2)) {
  print(c(minimum = minimum,
          before = find_split(swap_before[1:7], min_segment = minimum),
          after = find_split(swap_after[1:7], min_segment = minimum)))
}
# 최소 세 개일 때는 자리 교환 전후 모두 4를, 두 개일 때는 모두 5를 골랐음.
# 두 개일 때 새로 비교하는 5번째 뒤에서는 25와 30이 모두 왼쪽에 남음.
# 두 경우 모두 왼쪽 평균 130/5 = 26, 오른쪽 평균 40/2 = 20이라 차이는 -6임.

# 같은 일곱 개와 최소 두 개 조건에서 마지막 값에 더하는 양만 바꿔봄.
# 1 대신 10을 더하면 평균 차이뿐 아니라 고른 위치도 달라질 수 있을 것으로 예상함.
for (increase in c(0, 1, 10)) {
  small_change <- swap_before[1:7]
  small_change[7] <- small_change[7] + increase
  print(c(increase = increase,
          gap4 = mean(small_change[5:7]) - mean(small_change[1:4]),
          gap5 = mean(small_change[6:7]) - mean(small_change[1:5]),
          split_after = find_split(small_change, min_segment = 2)))
}
# 마지막 값이 20, 21, 30일 때 5번째 뒤의 차이는 -6, -5.5, -1임.
# 30일 때 4번째 뒤의 차이는 약 1.67이라, 차이의 크기가 1인 5번째 뒤보다 큼.
# 모든 후보를 비교해 고른 위치는 5, 5, 4였음. 1을 더했을 때의 결과가 계속 유지되지는 않았음.
