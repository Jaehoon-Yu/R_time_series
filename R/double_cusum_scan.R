# 한 구간의 시계열 행렬을 넣어 가장 큰 Double CUSUM 점수와 후보를 구함.
# Cho (2016), Section 2: https://arxiv.org/abs/1611.08631
# split은 입력의 몇 번째 열 뒤에서 나누는지, m은 묶은 점수 개수임.
# m은 실제로 변화한 시계열 개수를 확정한 값이 아님.
# 점수가 정확히 같으면 앞쪽 split, 그 안에서는 작은 m을 고름.
# 점수가 모두 0이어도 후보는 나옴. 실제 변화 여부를 판단하는 함수는 아님.
double_cusum_scan <- function(x) {
  if (!(is.double(x) || is.integer(x))) {
    stop("x must contain real numeric values.")
  }
  combined <- double_cusum(cusum_panel(x))
  # R은 행렬을 첫 열부터 읽으므로 같은 최고 점수에서 위 순서가 적용됨.
  best <- which.max(combined)
  position <- arrayInd(best, dim(combined))
  list(statistic = unname(combined[best]),
       split = position[1L, 2L],
       m = position[1L, 1L])
}
