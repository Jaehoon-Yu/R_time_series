source("R/cusum_panel.R")
source("R/estimate_residual_scale.R")
source("R/center_by_segments.R")
source("R/double_cusum.R")
source("R/double_cusum_scan.R")
source("R/double_cusum_decision.R")
source("R/double_cusum_segment.R")

# 두 시계열을 한 번에 넣음. 첫 행만 중간에 평균이 올라감.
panel <- rbind(changing = c(0, 0, 4, 4), constant = c(3, 3, 3, 3))
scores <- cusum_panel(panel)
print(scores)
# 첫 행의 두 번째 위치는 sqrt(2 * 2 / 4) * (0 - 4) = -4임.
# 둘째 행은 어느 위치에서 나눠도 평균 차이가 없어 모두 0임.

# 변화 위치 2를 직접 넣고 각 구간의 평균을 빼서 남은 흔들림을 구함.
known_means <- rbind(c(0, 0, 4, 4), c(0, 0, 8, 8))
scale_panel <- known_means + rbind(c(-1, 1, -1, 1), c(-2, 2, -2, 2))
residuals <- center_by_segments(scale_panel, change_points = 2)
row_scale <- estimate_residual_scale(residuals)
print(row_scale)
print(cusum_panel(scale_panel, scale = row_scale))
# 나누는 값은 1.4826과 2.9652가 나옴. 크기를 맞춘 두 행의 점수는 같음.
# 평균은 넣은 구간마다 계산함. 실제 자료의 변화 위치를 자동으로 고르는 과정은 아직 연결하지 않았음.

combined <- double_cusum(scores)
print(combined)
# 각 열에서 점수 크기를 정렬하고, 큰 점수 1개 또는 2개를 묶어 계산함.
# 가운데 열의 점수 크기는 4와 0임. 1개를 묶으면 약 3.464, 2개면 2임.
# 출력의 행 1, 2는 묶은 점수 개수임. 원래 시계열의 행 번호가 아님.
# 아직 기준값과 비교하지 않았으므로 변화가 있다고 판정한 것은 아님.

candidate <- double_cusum_scan(panel)
print(candidate)
# 예상한 가운데 위치 2와 묶은 개수 1이 나옴. 가장 큰 점수는 약 3.464임.
# split은 나누는 후보이며, 변화가 확정됐다는 뜻은 아님.

# 구간을 지정하면 앞뒤의 다른 값은 계산에서 제외함.
extended_panel <- cbind(c(100, -100), panel, c(-100, 100))
print(double_cusum_scan(extended_panel, start = 2, end = 5))
# 점수는 그대로이고, 후보는 원래 자료의 세 번째 값 뒤인 3으로 나옴.

# 기준값 3은 이 예제의 비교 규칙을 확인하려고 직접 정한 값임.
print(double_cusum_decision(panel, threshold = 3))
# 약 3.464가 3보다 크므로 TRUE와 위치 2를 돌려줌.
print(double_cusum_decision(matrix(3, 2, 4), threshold = 0))
# 값이 일정하면 점수가 0이라 FALSE가 나옴. NA는 선택한 위치가 없다는 뜻임.
# 실제 자료에 맞는 기준값을 자동으로 구하는 기능은 아직 없음.

# 행별 점수를 나누는 값을 모든 구간에 똑같이 전달함.
two_changes <- rbind(c(0, 0, 4, 4, 10, 10), c(0, 0, 4, 4, 10, 10))
print(double_cusum_segment(two_changes, threshold = 3, scale = c(2, 5)))
# 위치 4만 나옴. 왼쪽 구간도 같은 값으로 나누면 점수가 3보다 작아 더 나누지 않음.
# 기준값과 scale은 직접 정한 예시임. 이 값들을 자동으로 구하는 부분은 아직 없음.

# trim = 1은 구간 양끝에서 후보를 하나씩 제외함. 자료 자체는 그대로 씀.
edge_panel <- rbind(c(10, 0, 0, 0, 0, 0), c(10, 0, 0, 0, 0, 0))
print(double_cusum_scan(edge_panel, trim = 1))
# 첫 번째 후보를 제외하여 위치 2가 나옴. 첫 값 10은 평균 계산에 계속 들어감.
