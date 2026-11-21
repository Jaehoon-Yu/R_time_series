# 기존 scan의 가장 큰 점수를 직접 넣은 기준값과 비교함.
# Cho (2016), Section 3.2, DCBS Step 1.3: https://arxiv.org/abs/1611.08631
# 기준값과 같거나 작으면 split과 m은 NA로 두어 위치를 선택하지 않음.
# 기준값은 scale로 나눈 뒤의 점수와 같은 단위로 넣음. 자동 선택은 없음.
# m은 묶은 점수 개수이며, 실제로 변화한 시계열 개수를 뜻하지 않음.
# 이 함수는 한 구간만 판단함. 반복해서 나누는 부분은 double_cusum_segment에 있음.
double_cusum_decision <- function(x, threshold, start = 1L, end = ncol(x),
                                  scale = rep(1, nrow(x))) {
  if (!is.numeric(threshold) || is.complex(threshold) ||
      length(threshold) != 1L || !is.null(dim(threshold)) ||
      !is.finite(threshold) || threshold < 0) {
    stop("threshold must be one finite non-negative real number.")
  }
  threshold <- as.double(threshold)
  candidate <- double_cusum_scan(x, start = start, end = end, scale = scale)
  exceeds_threshold <- candidate$statistic > threshold
  list(statistic = candidate$statistic,
       threshold = threshold,
       exceeds_threshold = exceeds_threshold,
       split = if (exceeds_threshold) candidate$split else NA_integer_,
       m = if (exceeds_threshold) candidate$m else NA_integer_)
}
