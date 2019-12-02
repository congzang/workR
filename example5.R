#
# 5일차
#
setwd( "D:/WorkR" )
df <- read.table( file  = "airquality.txt", header = T )
df
class( df )
dim( df )
str( df )
head( df, 3 )
tail( df, 3 )

# 패키지 설치
#install.packages( "xlsx" )
#install.packages( "rJava" )

# 패키지 사용 선언(사용 설정) - library 호출순서 중요
library( rJava )
library( xlsx )

df.xlsx <- read.xlsx( "airquality.xlsx", sheetIndex = 1, encoding = "UTF-8" ) # sheetIndex 옵션 필수

df.xlsx
class( df.xlsx )
str( df.xlsx ) # read.table했을 경우에는Factor로 data type이 잡히지않았던 것이 Factor로 됨
head( df.xlsx, 3 )
tail( df.xlsx, 3 )

score <- c( 76, 84, 69, 50, 95, 6, 82, 71, 88, 84 )
which( score == 69 )
which( score >= 85 )
max( score )
which.max( score )
min( score )
which.min( score )

idx <- which( score >= 60, arr.ind = TRUE )
idx
score[ idx ] <- 61
score

idx <- which( iris[ , 1:4 ] > 5.0, arr.ind = TRUE )
idx

#
# 단일변수(일변량) 범주형 자료 탐색
#
favorite <- c( "WINTER", "SUMMER", "SPRING", "SUMMER", "SUMMER", "FALL", "FALL", "SUMMER", "SPRING", "SPRING" )
favorite
class( favorite )
table( favorite )
table( favorite ) / length( favorite )
ds <- table( favorite ) # table: 범주형에 대한 도수분포표 생성
ds
barplot( ds, main = 'favorite season' )

ds.new <- ds[ c( 2, 3, 1, 4 ) ] # 범례(x축) 순서 바꿈
ds.new
barplot( ds.new, main = 'favorite season' )

pie( ds, main = 'favorite season' )
pie( ds.new, main = 'favorite season', clockwise = TRUE )

favorite.color <- c( 2, 3, 2, 1, 1, 2, 2, 
                     1, 3, 2, 1, 3, 2, 1, 2 )
ds <- table( favorite.color ); ds
barplot( ds, main = "favorite color" )
colors <- c( "green", "red", "blue" )
names( ds ) <- colors; ds
barplot( ds, main  = "favorite color", col = colors ) # col: 색상부여
pie( ds, main = "favorite color", col = colors )

#
#  단일변수(일변량) 연속형 자료 탐색
#
weight <- c( 60, 62, 64, 65, 68, 69 ); weight
weight.heavy <- c( weight, 120 ); weight.heavy

# 평균
mean( weight ); mean( weight.heavy )
# 중앙값
median( weight ); median( weight.heavy )
# 절사평균
mean( weight, trim = 0.2 )
mean( weight.heavy, trim = 0.2 )
# 사분위수
quantile( weight.heavy )
quantile( weight.heavy, ( 0:10 ) / 10 )
summary( weight.heavy )

# 산포(distribution) - 값이 퍼져있는 정도 파악
# 분산
var( weight )
# 표준편차
sd( weight )
# 값의 범위(최소값과 최대값)
range( weight )
# 최대값과 최소값의 차이
diff( range( weight ))


# histogram : 연속형 자료의 분포를 시각화
# 연속형 자료에서는 구간을 나누고 구간에 속한 값들의 개수를 세는 방법으로 사용
str( cars )
cars
dist <- cars[ , 2 ]
hist( dist, main = "Histogram for 제동거리", xlab = '제동거리',  ylab = '빈도수', 
            border = 'blue', col = 'green', las = 2, breaks = 5 ) # breaks opt는 사분위수 50% 분포확인 후 막대개수 결정

# 상자그림(boxplot, 상자수염그림)
# 사분위수를 시각화하여 그래프 형태로 표시
# 상자그림은 하나의 그래프로 데이터의 분포형태를 포함한 다양한 정보를 전달
# 자료의 전반적인 분포를 이해하는데 도움
# 구체적인 최소/최대/중앙값을 알기는 어렵다
boxplot( dist, main = "자동차 제동거리" )
boxplot.stats( dist )
boxplot.stats( dist )$stats # 정상범위 사분위수
boxplot.stats( dist )$n     # 관측치 개수
boxplot.stats( dist )$conf  # 중앙값 신뢰구간
boxplot.stats( dist )$out   # 이상치(특이값) 목록

# 일변량중 그룹으로 구성된 자료의 상자그림
boxplot( Petal.Length~Species, data = iris, main = "품종별 꽃잎의 길이1" ) # [원하는 data]~[group 변수]
boxplot( iris$Petal.Length~iris$Species, main = "품종별 꽃잎의 길이2" )

# 한 화면에 여러 그래프 작성(par() 사용)
par( mfrow = c( 1, 3 ) ) # 1 X 3 가상화면 분할
barplot( table( mtcars$carb ), main = "C", xlab = "carburetors", ylab = "freq", col = "blue" )
barplot( table( mtcars$cyl ), main = "Cyl", xlab = "cyl", ylab = "freq", col = "red" )
barplot( table( mtcars$carb ), main = "G", xlab = "gear", ylab = "freq", col = "green" )
par( mfrow = c( 1, 1 ) ) # 가상화면 분할 해제
