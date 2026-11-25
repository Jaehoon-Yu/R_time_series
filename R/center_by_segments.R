# 주어진 변화 위치를 기준으로 각 행의 구간 평균을 빼고 남은 값을 돌려줌.
# 위치 2를 넣으면 1~2열과 3열부터 끝까지를 나눔. 위치를 직접 찾지는 않음.
# hdbinseg 1.0.3 clean.cp의 마지막 평균 빼기 부분을 따름.
# https://github.com/cran/hdbinseg/blob/1.0.3/R/code.R#L51-L55
# 위치는 작은 순서로 중복 없이 넣음. 생략하면 전체를 한 구간으로 봄.
center_by_segments <- function(x, change_points = integer(0)) {
  if (!is.matrix(x) || !(is.double(x) || is.integer(x)) ||
      nrow(x) < 1L || ncol(x) < 1L || any(!is.finite(x))) {
    stop("x must be a finite real numeric matrix with at least one row and column.")
  }
  if (!(is.double(change_points) || is.integer(change_points)) ||
      !is.null(dim(change_points)) || any(!is.finite(change_points)) ||
      any(change_points != floor(change_points)) ||
      any(change_points < 1 | change_points >= ncol(x)) ||
      any(diff(change_points) <= 0)) {
    stop("change_points must be strictly increasing integer positions from 1 to ncol(x) - 1.")
  }

  boundaries <- c(0, change_points, ncol(x))
  residuals <- x
  for (k in seq_len(length(boundaries) - 1L)) {
    columns <- seq.int(boundaries[k] + 1, boundaries[k + 1L])
    block <- x[, columns, drop = FALSE]
    residuals[, columns] <- sweep(block, 1L, rowMeans(block), "-")
  }
  if (any(!is.finite(residuals))) {
    stop("Centering overflowed; rescale the input values.")
  }
  residuals
}
