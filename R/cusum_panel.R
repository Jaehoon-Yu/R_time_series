# 한 행은 시계열 하나이고, 열은 시간 순서임.
# 각 위치에서 왼쪽 평균 - 오른쪽 평균에 양쪽 길이를 반영한 값을 곱함.
# Cho and Korkas (2021), Section 3.2, equation (7).
# https://doi.org/10.1016/j.ecosta.2021.07.009
# 이 함수는 첫 CUSUM 계산만 함. 여러 행의 결합이나 변화 여부 판정은 하지 않음.
# scale은 행 순서대로 넣는 양수임. 각 행의 점수를 이 값으로 나눔.
# Cho (2016), equation (1): https://arxiv.org/abs/1611.08631
# 생략하면 전부 1을 써서 기존 계산과 같음. scale을 자동으로 구하지는 않음.
cusum_panel <- function(x, scale = rep(1, nrow(x))) {
  if (!is.matrix(x) || !(is.double(x) || is.integer(x)) || nrow(x) < 1L ||
      ncol(x) < 2L || any(!is.finite(x))) {
    stop("x must be a finite numeric matrix: rows are series, columns are times.")
  }
  if (!(is.double(scale) || is.integer(scale)) || !is.null(dim(scale)) ||
      length(scale) != nrow(x) || any(!is.finite(scale)) || any(scale <= 0)) {
    stop("scale must contain one finite positive real number per row of x.")
  }

  n_time <- ncol(x)
  left_count <- as.double(seq_len(n_time - 1L))
  right_count <- n_time - left_count
  weight <- sqrt((left_count / n_time) * right_count)
  scores <- matrix(0, nrow = nrow(x), ncol = n_time - 1L,
                   dimnames = list(rownames(x), as.character(left_count)))

  for (j in seq_len(nrow(x))) {
    # 같은 값을 빼도 평균 차이는 같음. 큰 공통 값 때문에 생기는 계산 손실을 줄임.
    values <- as.double(x[j, ]) - x[j, 1L]
    partial_sum <- cumsum(values)
    left_mean <- partial_sum[-n_time] / left_count
    right_mean <- (partial_sum[n_time] - partial_sum[-n_time]) / right_count
    scores[j, ] <- (weight * (left_mean - right_mean)) / scale[j]
  }
  if (any(!is.finite(scores))) {
    stop("CUSUM calculation overflowed; rescale the input values.")
  }
  scores
}
