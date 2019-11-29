#
# break/next
#

#### break ####
sum <- 0

for( i in 1:10 ) {
  sum <- sum + 1
  
  if( i >= 5 ) {
    break
  }
}
sum

#### next ####
sum <- 0

for( i in 1:10 ) {
  if( i %% 2 == 0 ) {
    next
  }
  
  sum <- sum + i
}
sum


#
#### 산술 내장 함수 ####
#
log( 10 ) + 5       # 로그함수
log( 10, base = 2 )
sqrt( 25 )          # 제곱근
max( 5, 3, 2 )      # 최대값
min( 3, 9, 5 )      # 최소값
abs( -10 )          # 절대값
factorial( 5 )      # 팩토리얼
sin( pi / 2 )       # 삼각함수

#
#### 사용자 정의 함수 ####
#
mymax <- function( x, y ) {   # 함수 정의
  num.max <- x
  
  if( y > num.max ) {
    num.max <- y
  }
  
  return ( num.max )
}

mymax( 10, 15 )               # 함수 호출

a <- 10
b <- 5
c <- 8
max <- mymax( a, b )
max <- mymax( max, c )
max


#
#### 사용자 정의 함수 매개변수 초기값 설정 ####
#
mydiv <- function ( x, y = 2 ) {
  result <- x / y

  return ( result )
}

mydiv( x = 10, y = 3 )
mydiv( 10, 3 )
mydiv( 10 )

#### 외부 파일에 있는 함수 호출
setwd( "D:/workR" )               # 경로 지정
source( "mylib.R" )

my_max( 10, 5 )
my_div( 10, 2 )


#
#### Vector 도입 ####
#

# 1. Scalar만 사용
a <- 10
b <- 5
c <- 8
d <- 25
e <- 13
f <- 71
g <- 8
h <- 89
i <- 7
j <- 10
max <- a

if( b > max ) { max <- b }
if( c > max ) { max <- c }
if( d > max ) { max <- d }
if( e > max ) { max <- e }
if( f > max ) { max <- f }
if( g > max ) { max <- g }
if( h > max ) { max <- h }
if( i > max ) { max <- i }
if( j > max ) { max <- j }

max

# 2. Vector 사용
v <- c( 10, 5, 8, 25, 13, 71, 8, 89, 7, 10 )
max <- v[ 1 ]

for( i in 2:length( v ) ) {
  if( v[ i ] > max ) {
    max <- v[ i ]
  }
}

max


#### vector 생성 ####
x <- c( 1, 2, 3 )
y <- c( "a", "b", "c" )
z <- c( TRUE, TRUE, FALSE, TRUE )
x; y; z

class( x ); class( y ); class( z )

w <- c( 1, 2, 3, "a", "b", "c")   # 강제 타입변환(숫자->문자)
w
class(w)

v1 <- 50:90; v1
v2 <- c( 1, 2, 3, 50:90 ); v2
class( v1 ); class( v2 )

class(v1[1])


v3 <- seq( 1, 101, 3 ); v3
v4 <- seq( 0.1, 1.0, 0.1 ); v4

v5 <- rep( 1, times = 5 ); v5
v6 <- rep( 1:5, times = 3 ); v6
v7 <- rep( c( 1, 5, 9 ), times = 3 ); v7


#### 벡터 원소값에 이름 지정 ####
score <- c( 90, 85, 70 ); score

names( score )
names( score ) <- c( "Hong", "Kim", "Lee" )
names( score )

score

#### 벡터 원소 접근 ####
score[ 1 ]
score[ 2 ]
score[ 3 ]
score[ "Hong" ]
score[ "Lee" ]
score[ "Kim" ]

d <- c( 1, 4, 3, 7, 8 )
d[ 1 ]; d[ 2 ]; d[ 3 ]; d[ 4 ]; d[ 5 ]; d[ 6 ]

for( i in 1:length( score ) ) {
  print( score[ i ] )
}

score_names <- c( "Kim", "Hong", "Lee" )

for( i in 1:length( score ) ) {
  print( score[ score_names[ i ] ] )
}



#### 벡터에서 여러개의 값을 한번에 추출 ####
d <- c( 1, 4, 5, 7, 8 )
d[ c( 1, 3, 5) ]
d[ 1:3 ]
d[ seq( 1, 5, 2 ) ]
d[ -2 ]
d[ -c( 2:4 ) ]  # index 2~4 제외
d[ -(2:4) ]     # index 2~4 제외


GNP <- c( 2090, 2450, 960 ); GNP
names( GNP ) <- c( "Korea", "Japan", "Nepal" ); GNP
GNP[ 1 ]
GNP[ "Korea" ]
GNP[ c( "Korea", "Nepal" ) ]

#### 벡터 요소값 변경 ####
v1 <- c( 1, 5, 7, 8, 9 ); v1
v1[ 2 ] <- 3; v1
v1[ c( 1, 5 ) ] <- c ( 10, 20 ); v1

#### 벡터 간의 연산 ####
x <- c( 1, 2, 3 )
y <- c( 4, 5, 6 )
x + y
x - y
x * y
x / y
z <- x + y
z

#### 벡터에 적용가능한 함수 ####
d <- c ( 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 )
sum( d )                                # 합계
sum( 2 * d )
length( d )                             # 벡터의 요소 개수(길이)
mean( d[ 1:5 ] )                        # 평균
mean( d )
median( d[ 1:5 ] )                      # 중앙값
median( d )
max( d )                                # 최대값
min( d )                                # 최소값
sort( d )                               # 정렬
sort( d, decreasing = FALSE )
sort( d, decreasing = TRUE )
range( d )                              # 값의 범위(최대, 최소)
var( d )                                # 분산
sd( d )                                 # 표준편차

v <- sum( d ) / length( d ); v

#### 벡터에 논리연산 적용 ####
d >= 5            # 모든 요소에서 조건에 만족하는지 true/false
d[ d > 5 ]        # 조건에 만족하는 vector의 집합
sum( d > 5 )      # 조건에 만족하는 요소개수 count
sum( d[ d > 5 ] ) # 조건에 만족하는 요소값의 합계
d == 5            # 모든 요소에서 조건에 만족하는지 true/false

cond <- d > 5 & d < 8; cond
d[ cond ]

all( d > 5 )  # 모든 요소가 조건에 만족하는가?
any( d > 5 )  # 요소 중에 조건에 만족하는 값이 있는가?

head( d )     # default: 처음부터 6개
tail( d )     # default: 마지막부터 6개
head( d, 3 )  # 개수 지정
tail( d, 3 )

x <- c( 1, 2, 3, 4 ); x
y <- c( 4, 5, 6 ); y
z <- c( 3, 1, 2 ); z

w <- c( x, y ); w   # 두 vector 합침(중복되어도)
union( x, y )       # 합집합
intersect( x, y )   # 교집합
setdiff( x, y )     # 차집합
setequal( x, y )    # x, y에 모든 요소가 동일한가
setequal( x, z )    # x, z의 모든 요소가 동일한가



#### List ####
ds <- c( 90, 85, 70, 84 )
my.info <- list( name = "Hong", age = 30, status = TRUE, score = ds )

my.info
my.info[ 1:2 ]
my.info[[ 1 ]]
my.info$name
my.info[[ 4 ]]
my.info[[ 4 ]][1]

#### Factor 형(범주형) ####
bt <- c( 'A', 'B', 'B', 'O', 'AB', 'A' )  # 일반 vector
bt.new <- factor( bt )                    # factor형 vector
bt  
bt.new
bt[ 5 ]
bt.new[ 5 ]
levels( bt.new )
as.integer( bt.new )
bt.new[ 7 ] <- 'B'
bt.new[ 8 ] <- 'C'
bt.new


#### 함수 반환값(return값)이 여러 개일 때의 처리 ####
myfunc <- function( x, y ) {
  val.sum <- x + y
  val.mul <- x * y
  
  return ( list( sum = val.sum, mul = val.mul ) )
}

result <- myfunc( 5, 8 )
s <- result$sum
m <- result$mul
cat( '5 + 8 = ', s, '\n' )
cat( '5 * 8 = ', m, '\n' )

#### Matrix 생성 ####
z <- matrix( 1:20, nrow = 4 )
z
z <- matrix( 1:20, ncol = 4 )
z
z <- matrix( 1:20, nrow = 4, ncol = 5 )
z
z <- matrix( 1:20, nrow = 4, ncol = 5, byrow = T )
z

x <- 1:4
x
y <- 5:8
y
z <- matrix( 1:20, nrow = 5, ncol = 5 )
z

m1 <- cbind( x, y )
m1
m2 <- rbind( x, y )
m2
m3 <- rbind( m2, x )
m3
m4 <- cbind( z, x )
m4


#### Matrix에서 cell의 값 추출 ####
z
z[ 2, 3 ]
z[ 1, 4 ]
z[ 2, ]
z[ ,4 ]

z[ 2, 1:3 ]
z[ 1, c( 1, 2, 4 ) ]
z[ 1:2, ]
z[ , c( 1, 4 ) ]

#### Matrix에서 행,열에 이름 지정
score <- matrix( c( 90, 85, 69, 78, 
                    85, 96, 49, 95, 
                    90, 80, 70 ,70), 
                 nrow = 4, ncol = 3 )
score
rownames( score ) <- c( "Hong", "Kim", "Lee", "Yoo" )
colnames( score ) <- c( "English", "Math", "Science" )
score

score[ "Hong", "Math" ]
score[ "Kim", c( "Math", "Science" ) ]
score[ "Lee", ]
score[ , "English" ]
rownames( score )
colnames( score )
colnames( score )[ 2 ]

#### Data Frame 생성 ####
city <- c( "Seoul", "Tokyo", "Washington" )
rank <- c( 1, 2, 3 )
city.info <- data.frame( city, rank )
city.info

name <- c( "Hong", "Kim", "Lee" )
age <- c( 22, 20, 25 )
gender = factor( c( "M", "F", "M" ) )
blood.type = factor( c( "A", "O", "B" ) )
person.info <- data.frame( name, age, gender, blood.type )
person.info

person2.info <- data.frame( name = c( "Hong", "Kim", "Lee" ),
                            age = age <- c( 22, 20, 25 ),
                            gender = factor( c( "M", "F", "M" ) ),
                            blood.type = factor( c( "A", "O", "B" ) ) )
person2.info


city.info[ 1, 1 ]
city.info[ 1, ]
city.info[ , 1 ]
city.info[ city.info$city, ]
city.info[ , "rank"]

person.info$name
person.info[ person.info$name == "Hong", ]
person.info[ person.info$name == "Hong", c( "name", "age" ) ]

#data()

iris
iris[ , c( 1:2 ) ]
iris[ , c( 1, 3, 5 ) ]
iris[ , c( "Sepal.Length", "Species" ) ]
iris[ 1:5, ]; iris[ 1:5, c( 1, 3 ) ]



#### Matrix와 Data Frame에서 사용하는 함수
person.info
m3
dim( person.info )            # 행(관측치)의 개수, 열(변수)의 개수
nrow( person.info )
nrow( m3 )
ncol( person.info )
ncol( m3 )
head( iris )                  # (default:6)
tail( iris )                  #
str( iris )                   #
str( city.info )
str( person.info )
iris[, 5 ]
unique( iris[, 5 ] )
table( iris[, "Species" ] )   #
table( person.info[, "blood.type" ] )
table( person.info[, "gender" ] )
table( person.info[, "age" ] )


#### Matrix/Data Frame 사용 함수 ####
# 행별/열별 합계와 평균 계산
colSums( ( iris[ , -5 ] ) );        apply( iris[ , 1:4 ], 2, sum )
colMeans( ( iris[ , -5 ] ) );       apply( iris[ , 1:4 ], 2, mean )
rowSums( ( iris[ , -5 ] ) );        apply( iris[ , -5 ], 1, sum )
rowMeans( ( iris[ , -5 ] ) );       apply( iris[ , -5 ], 1, mean )
apply( iris[ , -5 ], 2, median )

# 행/열 방향 전환
z <- matrix( 1:20 , nrow = 4, ncol = 5 ); z
t( z )

# 조건에 맞는 행과 열의 값 추출(Data Frame만 가능)
IR.1 <- subset( iris, Species == "setosa" );                       IR.1
IR.2 <- subset( iris, Sepal.Length > 5.0 & Sepal.Width > 4.0 );    IR.2
IR.2[ , c( 2, 4 ) ]


# Matrix/Data Frame 산술연산
a <- matrix( 1:20, 4, 5 );  a
b <- matrix( 21:40, 4, 5 ); b

2 * a
b - 5
2 * a + 3 * b

a + b
b - a
b / a
a + b

# Matrix/Data Frame 자료구조 확인/변환 ####
class( iris );        str( iris )
class( state.x77 );   str( state.x77 )
head( state.x77 )
is.matrix( iris )
is.data.frame( iris )
is.matrix( state.x77 )
is.data.frame( state.x77 )

st <- data.frame( state.x77 ) # matrix를 data frame으로 변환
str( st )
head( st )
tail( st )
class( st )
dim( st )

iris.m <- as.matrix( iris[ , 1:4 ] )
head( iris.m )
class( iris.m )
dim( iris.m )

head( st )
Population
attach( st )
Population
detach( st )
Population


#### csv file 내용 읽기 ####
setwd( "D:/workR" ) # 현재 폴더이면 안해도 됨
air <- read.csv( "person.csv", header = T )

class( air )
dim( air )
str( air )
head( air )
tail( air )


