# 
# kimhokyeong_191218.R 
# 김호경 2019.12.18 / 2019.12.18
#
# 문1)
# R에서 제공하는 state.x77 데이터셋에 대해 k-평균 군집화를 실시하고 결과를 그래프로 출력하시오.
# 
# • 군집의 수는 5로 한다.
# • state.x77은 각 변수(열)의 값들의 단위의 차이가 많이 나기 때문에 0~1 표준화를 실시한 후 군집화를 실행한다.
library( cluster )

st <- data.frame( state.x77 )
str( st )

# 표준화 수행 함수
std <- function( x ) {
  return ( ( x - min ( x ) ) / ( max( x ) - min( x ) ) )
}

mydata <- apply( st, 2, std )
fit <- kmeans( x = mydata, center = 5 )
fit

clusplot( mydata, fit$cluster, color = TRUE, shade = TRUE, labels = 2, lines = 1 )

# 
# 문2)
# mlbench 패키지에서 제공하는 Sonar 데이터셋에 대해 k-평균 군집화를 실시하고 결과를 그래프로 출력하시오.
# 
# • 군집의 수는 2로 한다.
# • Sonar 데이터셋에서 마지막에 있는 Class 열은 제외하고 군집화를 실행한다.
library( mlbench )
data( "Sonar" )
str( Sonar )

colNm <- ncol( Sonar )

mydata <- Sonar[ , -colNm ]
fit <- kmeans( x = mydata, center = 2 )
fit

clusplot( mydata, fit$cluster, color = TRUE, shade = TRUE, labels = 2, lines = 1 )

# 
# 문3) 
# mlbench 패키지에서 제공하는 Sonar 데이터셋에 대해 k-최근접 이웃 알고리즘을 이용하여 모델을 만들고 예측 정확도를 측정하시오.
# 
# . Sonar 데이터셋에서 마지막에 있는 Class 열이 그룹 정보이다.
# . Sonar 데이터셋에서 홀수 번째 데이터(관측값)를 훈련용 데이터로 하고, 짝수번째 데이터(관측값)를 테스트용 데이터로 한다.
# . k-최근접 이웃에서 k를 3, 5, 7로 다르게 하여 예측 정확도를 비교한다.
library( mlbench )
library( class )
data( "Sonar" )
str( Sonar )

tr.idx_odd <- as.integer( row.names( Sonar ) ) %% 2 != 0
tr.idx_even <- as.integer( row.names( Sonar ) ) %% 2 == 0
colNm <- ncol( Sonar )

ds.tr <- Sonar[ tr.idx_odd, -colNm ] # 훈련용
ds.ts <- Sonar[ tr.idx_even, -colNm ] # 테스트용
cl.tr <- factor( Sonar[ tr.idx_odd, colNm ] )  # 훈련용 그룹정보
cl.ts <- factor( Sonar[ tr.idx_even, colNm ] ) # 테스트 그룹정보

pred.k3 <- knn( ds.tr, ds.ts, cl.tr, k = 3, prob = TRUE )
pred.k3
pred.k5 <- knn( ds.tr, ds.ts, cl.tr, k = 5, prob = TRUE )
pred.k5
pred.k7 <- knn( ds.tr, ds.ts, cl.tr, k = 7, prob = TRUE )
pred.k7

acc.k3 <- mean( pred.k3 == cl.ts )
acc.k3
acc.k5 <- mean( pred.k5 == cl.ts )
acc.k5
acc.k7 <- mean( pred.k7 == cl.ts )
acc.k7

# k=3 : 0.8269231     k=5 : 0.75      k=7 : 0.7115385

# 문4) 
# mlbench 패키지에서 제공하는 Sonar 데이터셋에 대해 k-최근접 이웃 알고리즘을 이용하여 모델을 만들고 예측 정확도를 측정하시오.
# 
# . Sonar 데이터셋에서 마지막에 있는 Class 열이 그룹 정보이다.
# . k-최근접 이웃에서 k는 3으로 한다.
# . 5-fold 교차 검증 방법으로 예측 정확도를 측정한다.
library( mlbench )
library( class )
library( cvTools )
data( "Sonar" )
str( Sonar )

colNm <- ncol( Sonar )

k = 5
folds <- cvFolds( nrow( Sonar ), K = k )
acc <- c()

for( i in 1:k ) {
  ts.idx <- folds$which == i
  ds.tr <- Sonar[ -ts.idx, -colNm ]
  ds.ts <- Sonar[ ts.idx, -colNm ]
  cl.tr <- factor( Sonar[ -ts.idx, colNm ] )
  cl.ts <- factor( Sonar[ ts.idx, colNm ] )
  pred <- knn( ds.tr, ds.ts, cl.tr, k = 3 )
  acc[ i ] <- mean( pred == cl.ts )
}
acc
mean( acc )
