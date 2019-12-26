#
# day16_answer.R
#
# 군집화 / 분류 실습
#
#문1)
#R에서 제공하는 state.x77 데이터셋에 대해 k-평균 군집화를 실시하고 결과를 그래프로 출력하시오.
#
#• 군집의 수는 5로 한다.
#• state.x77은 각 변수(열)의 값들의 단위의 차이가 많이 나기 때문에 0~1 표준화를 실시한 후 군집화를 실행한다.

# 0~1 표준화 함수
std <- function( x ) {
  result <- ( x - min( x ) ) / ( max( x ) - min( x ) )
  return( result )
}

# 데이터 표준화
ds.new <- std( state.x77 )
head( ds.new )

# 군집화
fit <- kmeans( x = ds.new, centers = 5 )
fit

# 차원 축소 후 군집 시각화
library( cluster )

clusplot( ds.new, fit$cluster, color = TRUE, shade = TRUE, labels = 2, lines = 0 )

#문2)
#mlbench 패키지에서 제공하는 Sonar 데이터셋에 대해 k-평균 군집화를 실시하고 결과를 그래프로 출력하시오.
#
# • 군집의 수는 2로 한다.
# • Sonar 데이터셋에서 마지막에 있는 Class 열은 제외하고 군집화를 실행한다.

library( mlbench )
data( "Sonar" ) 			# 데이터셋 불러오기

dim( Sonar )
str( Sonar )
head( Sonar )
View( Sonar )

ds.new <- Sonar[ ,-61 ]   # Class 열 제거
head( ds.new )

# 군집화
fit <- kmeans( x = ds.new, centers = 2 )
fit

# 차원 축소 후 군집 시각화
clusplot( ds.new, fit$cluster, color = TRUE, shade = TRUE, labels = 2, lines = 0 )

#문3) 
#mlbench 패키지에서 제공하는 Sonar 데이터셋에 대해 k-최근접 이웃 알고리즘을 이용하여 모델을 만들고 예측 정확도를 측정하시오.
#
# . Sonar 데이터셋에서 마지막에 있는 Class 열이 그룹 정보이다.
# . Sonar 데이터셋에서 홀수 번째 데이터(관측값)를 훈련용 데이터로 하고, 짝수번째 데이터(관측값)를 테스트용 데이터로 한다.
# . k-최근접 이웃에서 k를 3, 5, 7로 다르게 하여 예측 정확도를 비교한다.

# 결측값 제거
ds.new <- Sonar[ complete.cases( Sonar ), ]

# 훈련, 학습 데이터 생성
idx <- seq( 1, nrow( ds.new ), 2 )
x.train <- ds.new[ idx, -61 ]
y.train <- ds.new$Class[ idx ]
x.test <- ds.new[ -idx, -61 ]
y.test <- ds.new$Class[ -idx ]

# 분류 모델
library( class )

pred3 <- knn( x.train, x.test, y.train, k = 3 )
acc3 <- mean( pred3 == y.test )
acc3

pred5 <- knn( x.train, x.test, y.train, k = 5 )
acc5 <- mean( pred5 == y.test )
acc5

pred7 <- knn( x.train, x.test, y.train, k = 7 )
acc7 <- mean( pred7 == y.test )
acc7

# 예측 정확도
#k=3 일 때 : 0.8269231
#k=5 일 때 : 0.75
#k=7 일 때 : 0.7115385

#문4) 
#mlbench 패키지에서 제공하는 Sonar 데이터셋에 대해 k-최근접 이웃 알고리즘을 이용하여 모델을 만들고 예측 정확도를 측정하시오.
#
# . Sonar 데이터셋에서 마지막에 있는 Class 열이 그룹 정보이다.
# . k-최근접 이웃에서 k는 3으로 한다.
# . 5-fold 교차 검증 방법으로 예측 정확도를 측정한다.

data( "Sonar" )                             # 데이터셋 불러오기

# 결측값 제거
ds.new <- Sonar[ complete.cases( Sonar ), ]

library( class )
library( cvTools )        

folds <- cvFolds( nrow( ds.new ), K = 5)    # 5 폴드 생성
acc <- c()                                  # 폴드별 예측정확도 저장용 벡터

for ( i in 1:5 ) {
  # 훈련, 학습 데이터 생성
  idx <- folds$which == i
  x.train <- ds.new[ -idx, -61 ]
  y.train <- ds.new$Class[ -idx ]
  x.test <- ds.new[ idx, -61 ]
  y.test <- ds.new$Class[ idx ]
  
  # 분류 모델
  pred <- knn( x.train, x.test, y.train, k = 3 )
  acc[i] <- mean( pred == y.test )
}

acc
mean( acc )
