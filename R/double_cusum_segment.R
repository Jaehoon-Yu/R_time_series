# 기준값을 넘은 위치에서 양쪽 구간을 나누고 같은 계산을 반복함.
# Cho (2016), Section 3.2, Steps 1.3-1.4: https://arxiv.org/abs/1611.08631
# 결과는 원래 자료에서 나누는 위치를 작은 순서로 모은 정수 벡터임.
# 선택한 위치가 없으면 integer(0)을 돌려줌.
# 모든 구간에 같은 기준값을 직접 넣는 기본 분할임.
# scale은 행 순서대로 직접 넣는 양수이며, 나눈 모든 구간에 같은 값을 씀.
# 생략하면 전부 1임. scale 추정, 경계 부근 제외, 기준값 자동 선택, 마지막 재검사는 없음.
double_cusum_segment <- function(x, threshold, start = 1L, end = ncol(x),
                                 scale = rep(1, nrow(x))) {
  # 첫 구간의 입력도 기존 함수로 검사함.
  first <- double_cusum_decision(x, threshold, start, end, scale = scale)

  split_interval <- function(start, end, decision) {
    if (!decision$exceeds_threshold) return(integer(0))
    split <- as.integer(decision$split)
    left <- right <- integer(0)
    # 값이 하나뿐인 구간은 더 나눌 수 없으므로 계산하지 않음.
    if (start < split) {
      left <- split_interval(start, split,
        double_cusum_decision(x, threshold, start, split, scale = scale))
    }
    if (split + 1L < end) {
      right <- split_interval(split + 1L, end,
        double_cusum_decision(x, threshold, split + 1L, end, scale = scale))
    }
    c(left, split, right)
  }

  split_interval(as.integer(start), as.integer(end), first)
}
