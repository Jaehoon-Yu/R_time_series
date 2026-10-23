# 한 행은 시계열 하나이고, 열은 시간 순서임.
# 각 위치에서 왼쪽 평균 - 오른쪽 평균에 양쪽 길이를 반영한 값을 곱함.
# Cho and Korkas (2021), Section 3.2, equation (7).
# https://doi.org/10.1016/j.ecosta.2021.07.009
# 이 함수는 첫 CUSUM 계산만 함. 여러 행의 결합이나 변화 여부 판정은 하지 않음.
cusum_panel <- function(x) {
  if (!is.matrix(x) || !is.numeric(x) || nrow(x) < 1L ||
      ncol(x) < 2L || any(!is.finite(x))) {
    stop("x must be a finite numeric matrix: rows are series, columns are times.")
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
    scores[j, ] <- weight * (left_mean - right_mean)
  }
  if (any(!is.finite(scores))) {
    stop("CUSUM calculation overflowed; rescale the input values.")
  }
  scores
}
