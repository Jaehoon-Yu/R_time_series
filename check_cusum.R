source("R/cusum_panel.R")

# 두 시계열을 한 번에 넣음. 첫 행만 중간에 평균이 올라감.
panel <- rbind(changing = c(0, 0, 4, 4), constant = c(3, 3, 3, 3))
scores <- cusum_panel(panel)
print(scores)
# 첫 행의 두 번째 위치는 sqrt(2 * 2 / 4) * (0 - 4) = -4임.
# 둘째 행은 어느 위치에서 나눠도 평균 차이가 없어 모두 0임.
# 이 계산만으로 변화가 있는지 판정한 것은 아님.
