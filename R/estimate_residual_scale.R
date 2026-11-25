# 구간별 평균을 뺀 뒤 남은 값(잔차)을 행마다 넣음.
# hdbinseg 1.0.3 dcbs.alg의 temporal = FALSE 크기 계산을 따름.
# https://github.com/cran/hdbinseg/blob/1.0.3/R/dcbs.R#L62-L68
# 표준편차, 가운데 값에서 떨어진 정도(MAD), 최소값 중 큰 값을 씀.
# MAD는 R 기본값인 1.4826을 곱함. 출력 순서는 입력 행 순서와 같음.
# 평균을 빼거나 변화 위치를 찾는 작업은 이 함수가 하지 않음.
# 시간상 이웃한 값들의 관계를 보정하는 장기분산 추정은 포함하지 않음.
estimate_residual_scale <- function(residuals, min_scale = 0.01) {
  if (!is.matrix(residuals) ||
      !(is.double(residuals) || is.integer(residuals)) ||
      nrow(residuals) < 1L || ncol(residuals) < 2L ||
      any(!is.finite(residuals))) {
    stop("residuals must be a finite real numeric matrix with at least two columns.")
  }
  if (!(is.double(min_scale) || is.integer(min_scale)) ||
      !is.null(dim(min_scale)) || length(min_scale) != 1L ||
      !is.finite(min_scale) || min_scale <= 0) {
    stop("min_scale must be one finite positive real number.")
  }

  scale <- vapply(seq_len(nrow(residuals)), function(j) {
    values <- residuals[j, ]
    max(stats::sd(values), stats::mad(values), min_scale)
  }, numeric(1))
  if (any(!is.finite(scale))) {
    stop("Scale calculation overflowed; rescale the residual values.")
  }
  names(scale) <- rownames(residuals)
  scale
}
