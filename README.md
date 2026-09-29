# R_time_series

R로 숫자를 앞뒤 두 부분으로 나누고 평균을 비교하는 연습.
지금은 평균 차이가 가장 큰 위치를 찾는 코드를 조금씩 수정하고 있음.

R_time_series.Rproj를 RStudio에서 열고 아래 코드를 실행하면 됨.

```r
source("check_examples.R")
source("practice.R")
```

check_examples.R에는 몇 가지 예제가 있고, practice.R에서는 계산 과정을 볼 수 있음.
R/find_split.R에는 계산에 쓰는 함수를 모아둠.
결과가 4면 네 번째 숫자 다음에서 나눴다는 뜻임.
숫자가 모두 같으면 NA가 나옴.
차이가 큰 위치를 찾았다고 실제로 변화가 생겼다고 단정할 수는 없음.

확인용 예제는 아래처럼 실행함.

```r
source("tests/test_find_split.R")
```
