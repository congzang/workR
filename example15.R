#
# 16일차
#
# 군집화( clustering ) / 분류( classification )
#
# 군집화( clustering ) : 주어진 대상 데이터들을 유사성이 높은 것끼리 묶어주는 기술( 군집, 범주, 그룹 )
#
# k-means(평균) 군집화 알고리즘
mydata <- iris[ , 1:4 ]
fit <- kmeans( x = mydata, center = 3 )
fit
# K-means clustering with 3 clusters of sizes 21, 33, 96      >> 3개의 군집에 속한 데이터 개수
# 
# Cluster means:                                              >>  3개 군집의 중심점 좌표
#   Sepal.Length Sepal.Width Petal.Length Petal.Width
# 1     4.738095    2.904762     1.790476   0.3523810
# 2     5.175758    3.624242     1.472727   0.2727273
# 3     6.314583    2.895833     4.973958   1.7031250
#(↑군집번호)
#
# Clustering vector:                                          >> 각 데이터에 대한 군집번호
#  [1] 2 1 1 1 2 2 2 2 1 1 2 2 1 1 2 2 2 2 2 2 2 2 2 2 1 1 2 2 2 1 1 2 2 2 1 2 2 2 1 2 2 1 1 2 2 1 2 1 2 2 3 3 3 3 3 3 3 1 3 3 1 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3
# [94] 1 3 3 3 3 1 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3
# 
# Within cluster sum of squares by cluster:
#   [1]  17.669524   6.432121 118.651875
#    (between_SS / total_SS =  79.0 %)
# 
# Available components:
#   [1] "cluster"      "centers"      "totss"        "withinss"     "tot.withinss" "betweenss"    "size"         "iter"         "ifault"      
fit$cluster
fit$centers

library( cluster ) # 차원축소 후 군집 시각화 패키지
clusplot( mydata,        # 군집할 대상
          fit$cluster,   # 생성된 군집의 번호
          color = TRUE,  # 그룹별 원의 색
          shade = TRUE,  # 그룹별 원의 빗금표시유무
          labels = 2,    # 관측값 출력 형태(데이터 번호유무 등)
          lines = 1 )    # 각 그룹의 중심선 연결 표시

subset( mydata, fit$cluster == 2 )

#
# 대상 데이터 표준화 후 군집화
# 
#   데이터와 데이터의 거리를 계산할 때 발생하는 문제
#   모든 변수가 거리 계산에 동등한 영향을 갖도록 하기 위해서 모든 변수의 자료번위를 0~1사이로 표준화한 후 거리 계산을 한다.
# 
#   ( x - min( A ) ) / ( max( A ) - min( A ) )  (x : 변수 A의 임의의 관측값)
#   max( A ), min( A )는 변수 A 관측값 중 최대/최소값

# 표준화 수행 함수
std <- function( x ) {
  return ( ( x - min ( x ) ) / ( max( x ) - min( x ) ) )
}

mydata <- apply( iris[ , 1:4 ], 2, std ) # 표준화 된 데이터 사용
fit <- kmeans( x = mydata, center = 3 )
fit

#
# KNN( K-Nearest Neighbor, K-최근접 이웃) 분류 알고리즘
#
library( class )

# 훈련용/테스트용 데이터 준비
tr.idx <- c( 1:25, 51:75, 101:125 )
ds.tr <- iris[ tr.idx, 1:4 ]  # 훈련용
ds.ts <- iris[ -tr.idx, 1:4 ] # 테스트용
cl.tr <- factor( iris[ tr.idx, 5 ] )  # 훈련용 그룹정보
cl.ts <- factor( iris[ -tr.idx, 5 ] ) # 테스트 그룹정보
pred <- knn( ds.tr, ds.ts, cl.tr, k = 3, prob = TRUE ) # k값은 정해진 것이 없고 바꿔가면서 정확도 높은 값을 찾는다.(1~7)
pred
acc <- mean( pred == cl.ts )
acc
table( pred, cl.ts )

#
# 교차 검증 방법( K-fold cross validation )
#
install.packages( "cvTools" )
library( cvTools )

k = 10
folds <- cvFolds( nrow( iris ), K = k ) # 폴드 생성(K: 훈련횟수)
acc <- c() # 폴드별 예측 정확도 저장용 벡터

for( i in 1:k ) {
  ts.idx <- folds$which == i
  ds.tr <- iris[ -ts.idx, 1:4 ]
  ds.ts <- iris[ ts.idx, 1:4 ]
  cl.tr <- factor( iris[ -ts.idx, 5 ] )
  cl.ts <- factor( iris[ ts.idx, 5 ] )
  pred <- knn( ds.tr, ds.ts, cl.tr, k = 5 )
  acc[ i ] <- mean( pred == cl.ts ) # 예측 정확도
}
acc # 폴드별 예측 정확도
mean( acc ) # 폴드 평균 예측 정확도
