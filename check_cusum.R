source("R/cusum_panel.R")
source("R/double_cusum.R")

# 두 시계열을 한 번에 넣음. 첫 행만 중간에 평균이 올라감.
panel <- rbind(changing = c(0, 0, 4, 4), constant = c(3, 3, 3, 3))
scores <- cusum_panel(panel)
print(scores)
# 첫 행의 두 번째 위치는 sqrt(2 * 2 / 4) * (0 - 4) = -4임.
# 둘째 행은 어느 위치에서 나눠도 평균 차이가 없어 모두 0임.

combined <- double_cusum(scores)
print(combined)
# 각 열에서 점수 크기를 정렬하고, 큰 점수 1개 또는 2개를 묶어 계산함.
# 가운데 열의 점수 크기는 4와 0임. 1개를 묶으면 약 3.464, 2개면 2임.
# 출력의 행 1, 2는 묶은 점수 개수임. 원래 시계열의 행 번호가 아님.
# 아직 기준값과 비교하지 않았으므로 변화가 있다고 판정한 것은 아님.
