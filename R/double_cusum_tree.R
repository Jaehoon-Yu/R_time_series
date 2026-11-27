# 잔차를 만들기 전에 쓸 임시 분할 후보를 정해진 깊이까지 찾음.
# hdbinseg 1.0.3 dcbs.make.tree의 기준값 없이 나누는 규칙을 따름.
# https://github.com/cran/hdbinseg/blob/1.0.3/R/dcbs.R#L212-L252
# 첫 구간의 깊이는 1임. height는 직접 넣는 양의 정수임.
# 원자료를 scale = 1로 계산하며, 점수가 0이어도 후보를 저장함.
# nodes의 한 행은 살펴본 구간 하나임. change_points는 정렬된 후보 위치임.
# 후보는 실제 변화 확정이 아님. 높이 자동 선택이나 기준값 비교는 하지 않음.
# trim은 기존 scan처럼 허용 후보 안에서 동점을 처리하며 기본값은 0임.
double_cusum_tree <- function(x, height, trim = 0L) {
  if (!(is.double(height) || is.integer(height)) || length(height) != 1L ||
      !is.null(dim(height)) || !is.finite(height) || height < 1 ||
      height != floor(height) || height > .Machine$integer.max) {
    stop("height must be one positive integer no larger than .Machine$integer.max.")
  }
  height <- as.integer(height)
  # 첫 구간에서 자료와 trim도 검사함. 후보가 없는 입력 구간은 오류로 알림.
  first <- double_cusum_scan(x, trim = trim)
  pending <- list(c(start = 1L, end = ncol(x), level = 1L))
  nodes <- list()
  i <- 1L
  while (i <= length(pending)) {
    bounds <- pending[[i]]
    start <- unname(bounds["start"])
    end <- unname(bounds["end"])
    level <- unname(bounds["level"])
    candidate <- if (i == 1L) first else double_cusum_scan(x, start, end, trim = trim)
    split <- as.integer(candidate$split)
    nodes[[i]] <- data.frame(level = level, start = start, split = split, end = end,
                             statistic = candidate$statistic, m = candidate$m)
    # 길이가 충분한 양쪽 구간만 다음 깊이에서 확인함.
    if (level < height) {
      if (split - start + 1L >= 2 * trim + 2) {
        pending[[length(pending) + 1L]] <- c(start = start, end = split, level = level + 1L)
      }
      if (end - split >= 2 * trim + 2) {
        pending[[length(pending) + 1L]] <- c(start = split + 1L, end = end, level = level + 1L)
      }
    }
    i <- i + 1L
  }
  nodes <- do.call(rbind, nodes)
  list(nodes = nodes, change_points = sort(as.integer(nodes$split)))
}
