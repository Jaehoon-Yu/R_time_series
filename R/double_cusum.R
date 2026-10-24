# 입력의 행은 시계열이고, 열은 나누는 위치임. cusum_panel의 결과를 넣음.
# 결과의 m번째 행은 크기가 큰 m개 점수를 묶었을 때의 Double CUSUM 값임.
# Cho and Korkas (2021), Section 3.2, equation (8):
# https://doi.org/10.1016/j.ecosta.2021.07.009
# 남은 합은 m+1번째부터 끝까지임. Cho (2016), equation (4), phi = 1/2:
# https://arxiv.org/abs/1611.08631
# 이 함수는 점수만 계산함. 변화 여부를 판단하는 기준값은 아직 적용하지 않음.
double_cusum <- function(scores) {
  if (!is.matrix(scores) || !(is.double(scores) || is.integer(scores)) ||
      nrow(scores) < 1L || ncol(scores) < 1L || any(!is.finite(scores))) {
    stop("scores must be a finite real numeric matrix: rows are series, columns are splits.")
  }

  n_series <- as.double(nrow(scores))
  m <- seq_len(nrow(scores))
  remaining_count <- 2 * n_series - m
  weight <- sqrt((m / (2 * n_series)) * remaining_count)
  combined <- matrix(0, nrow = nrow(scores), ncol = ncol(scores),
                     dimnames = list(as.character(m), colnames(scores)))

  for (k in seq_len(ncol(scores))) {
    ordered <- sort(abs(as.double(scores[, k])), decreasing = TRUE)
    top_sum <- cumsum(ordered)
    # 뒤에서 더해 남은 합을 구함. 모든 점수를 묶으면 남은 합은 0임.
    remaining_sum <- c(rev(cumsum(rev(ordered)))[-1L], 0)
    combined[, k] <- weight * (top_sum / m - remaining_sum / remaining_count)
  }
  if (any(!is.finite(combined))) {
    stop("Double CUSUM calculation overflowed; rescale the input scores.")
  }
  combined
}
