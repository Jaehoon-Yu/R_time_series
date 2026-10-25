# 시계열 행렬의 start부터 end까지에서 가장 큰 Double CUSUM 점수와 후보를 구함.
# Cho (2016), Section 2: https://arxiv.org/abs/1611.08631
# start와 end는 모두 포함하며, 생략하면 입력 전체를 계산함.
# split은 원래 입력 x의 몇 번째 열 뒤에서 나누는지, m은 묶은 점수 개수임.
# m은 실제로 변화한 시계열 개수를 확정한 값이 아님.
# 점수가 정확히 같으면 앞쪽 split, 그 안에서는 작은 m을 고름.
# 점수가 모두 0이어도 후보는 나옴. 실제 변화 여부를 판단하는 함수는 아님.
double_cusum_scan <- function(x, start = 1L, end = ncol(x)) {
  if (!is.matrix(x) || !(is.double(x) || is.integer(x)) ||
      nrow(x) < 1L || ncol(x) < 2L || any(!is.finite(x))) {
    stop("x must be a finite real numeric matrix: rows are series, columns are times.")
  }
  valid_bound <- function(value) {
    (is.double(value) || is.integer(value)) && length(value) == 1L &&
      is.finite(value) && value == floor(value)
  }
  if (!valid_bound(start) || !valid_bound(end) ||
      start < 1L || start >= end || end > ncol(x)) {
    stop("start and end must be whole numbers with 1 <= start < end <= ncol(x).")
  }
  start <- as.integer(start)
  end <- as.integer(end)
  # 구간 안에서 평균과 점수를 다시 계산함. 한 행이어도 행렬 모양을 유지함.
  segment <- x[, seq.int(start, end), drop = FALSE]
  combined <- double_cusum(cusum_panel(segment))
  # R은 행렬을 첫 열부터 읽으므로 같은 최고 점수에서 위 순서가 적용됨.
  best <- which.max(combined)
  position <- arrayInd(best, dim(combined))
  list(statistic = unname(combined[best]),
       split = start + position[1L, 2L] - 1L,
       m = position[1L, 1L])
}
