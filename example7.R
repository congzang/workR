#
# 7일차
#
# 결측치 처리
#
# vector의 결측치 처리
z <- c( 1, 2, 3, NA, 5, NA, 8 )
sum( z )
is.na( z )
sum( is.na( z ) )
sum( z, na.rm = TRUE )

# 결측치 대체 및 제거
z1 <- z
z2 <- c( 5, 8, 1, NA, 3, NA, 7 )
z1[ is.na( z1 ) ] <- 0            # 단순대입법
z1
z3 <- as.vector( na.omit( z2 ) )  # Listwise Deletion (na.omit() : 결측치가 포함된 데이터 삭제)
z3

# Matrix / Data Frame 결측치 처리
x <- iris
head (x)
x[ 1, 2 ] <- NA
x[ 1, 3 ] <- NA
x[ 2, 3 ] <- NA
x[ 3, 4 ] <- NA
x[ 4, 2 ] <- NA
x[ 5, 2 ] <- NA
head( x )

# Matrix / Data Frame 열별 결측치 확인
# 1) for문 이용
for( i in 1:ncol( x ) ) {
  this.na <- is.na( x[ , i ] )
  cat( colnames( x )[ i ], "\t", sum( this.na ), "\n" )
}

# 2) apply() 이용
col_na <- function( y ) {
  return ( sum( is.na( y ) ) )
}
na_count <- apply( x, 2, col_na )
na_count

na_count <- apply( x, 2, function ( y ) sum( is.na( y ) ) )
na_count

barplot( na_count[ na_count > 0 ] )

install.packages( "VIM" )
require( VIM ) # (= library( VIM ))

# 결측치 자료 조합 확인용 시각화 도구
aggr( x, prop = FALSE, numbers = TRUE )

# 두 개의 변수 간의 결측치 관계 확인 시각화 도구
head( x )
head( x[ c( "Sepal.Width", "Petal.Length" ) ] )
marginplot( x[ c( "Sepal.Width", "Petal.Length" ) ], pch = 20, col = c( "darkgray", "red", "blue" ) )


# Matrix / Data Frame의 행(data)별 결측치 확인
rowSums( is.na( x ) )             # 각 행별 결측치 개수

sum( rowSums( is.na( x ) ) > 0 )  # 결측치가 포함된 행(데이터)의 개수
sum( is.na( x ) )                 # 결측치 개수

# 결측치를 제외한 새로운 데이터셋 생성
head( x )
x[ !complete.cases( x ), ] # NA가 포함된 행 출력
y <- x[ complete.cases( x ), ]
head( y )

# 특이값, 이상치(outlier)
st <- data.frame( state.x77 )
summary( st$Income )
boxplot( st$Income )
boxplot.stats( st$Income )$out

# 특이값 처리 : NA로 변환 후 결측치 처리방법 이용
out.val <- boxplot.stats( st$Income )$out
st$Income[ st$Income %in% out.val ] <- NA # A %in% B : B가 A에 포함되어 있다면
head( st )
newdata <- st[ complete.cases( st ), ]
head( newdata )

#
# 데이터 가공
#
# 데이터 정렬
# vector 정렬
vc <- c( 8, 7, 6, 5, 4 ); order( vc ) # 정렬 후 index
v1 <- c( 1, 7, 6, 8, 4, 2, 3 )
order( v1 )
v1 <- sort( v1 )
v1
v2 <- sort( v1, decreasing = T )
v2

# Matrix / Data Frame 정렬
head( iris )
order( iris$Sepal.Length )
iris[ order( iris$Sepal.Length ), ] # Ascending
iris[ order( iris$Sepal.Length, decreasing = T ), ] # Decreasing
iris.new <- iris[ order( iris$Sepal.Length ), ]
head( iris.new )
iris[ order( iris$Species, decreasing = T, iris$Sepal.Length ), ] # 정렬기준을 2개 설정 order( 변수1, 정렬기준1, 변수2, 정렬기준2, ... )

# 데이터 분리
sp <- split( iris, iris$Species ) # species별로 list 생성
sp
sp$setosa # list 중 하나 선택
summary( sp ) # list별 요약정보
summary( sp$setosa )

# 데이터 선택
subset( iris, Species == "setosa" )
subset( iris, Sepal.Length > 7.5 )
subset( iris, Sepal.Length > 5.1 & Sepal.Width > 3.9 )
subset( iris, Sepal.Length > 7.6, select = c( Petal.Length, Petal.Width ) )

# 데이터 Sampling
# 숫자를 임의로 추출
x <- 1:3
y <- sample( x, size = 3, replace = TRUE ) # replace : 중복시킬지 여부(FALSE: 중복안함(비복원추출), TRUE: 중복(복원추출))
y

# 행을 임의로 추출
idx <- sample( 1:nrow( iris ), size = 50, replace = FALSE )
idx
iris.50 <- iris[ idx, ]
dim( iris.50 )
head( iris.50 )

sample( 1:20, size = 5 )
sample( 1:20, size = 5 )
sample( 1:20, size = 5 )

set.seed( 100 )
sample( 1:20, size = 5 )
set.seed( 100 )
sample( 1:20, size = 5 )
set.seed( 200 )
sample( 1:20, size = 5 )

# 데이터 조합
combn( 1:5, 3 )

x <- c( "red", "green", "blue", "black", "white" )
com <- combn( x, 2 )
com

for( i in 1:ncol( com ) ) {
  cat( com[ , i ], "\n" )
}

# 데이터 집계 - 내부적으로 그룹핑 필요
agg <- aggregate( iris[ , -5 ], by = list( iris$Species ), FUN = mean )
agg

agg <- aggregate( iris[ , -5 ], by = list( iris$Species ), FUN = sd )
agg

head( mtcars )
agg <- aggregate( mtcars, by = list( cyl = mtcars$cyl, vs = mtcars$vs ), FUN = max )
agg

# 데이터 병합
x <- data.frame( name = c( "a", "b", "c" ), mat = c( 90, 80, 40 ) ); x
y <- data.frame( name = c( "a", "b", "d" ), korean = c( 75, 60, 90 ) ); y
z <- merge( x, y, by = c( "name" ) )
z

merge( x, y, all.x = T )
merge( x, y, all.y = T )
merge( x, y, all = T )


x <- data.frame( name = c( "a", "b", "c" ), mat = c( 90, 80, 40 ) ); x
y <- data.frame( sname = c( "a", "b", "d" ), korean = c( 75, 60, 90 ) ); y

merge( x, y, by.x = c( "name" ), by.y = c( "sname" ) )

#
# dplyr package : 데이터 가공하는 입장에서 많이 사용
#
install.packages( "dplyr" )
library( dplyr )

df <- data.frame( var1 = c( 1, 2, 1 ), var2 = c( 2, 3, 2) )
df

# rename() : 변수명 변경
df <- rename( df, v1 = var1, v2 = var2 )
df  

# 파생변수 추가
df$sum <- df$v1 + df$v2 # $로 추가(pipe 연결 불가능)
df
df[ 2, 1 ] <- 5
df

df <- data.frame( id = c( 1, 2, 3, 4, 5, 6 ),
                  class = c( 1, 1, 1, 1, 2, 2 ),
                  math = c( 50, 60, 45, 30, 25, 50 ),
                  english = c( 98, 97, 86, 98, 80, 89 ),
                  science = c( 50, 60, 78, 58, 65, 98 ) )
df

# 행 추출
df %>% filter( class == 1 )
df %>% filter( class == 2 )
df %>% filter( class != 1 )
df %>% filter( class != 2 )

df %>% filter( science > 70 )
df %>% filter( math < 50 )

df %>% filter( class == 1 & math >= 50 )
df %>% filter( math >= 50 | english >= 90 )
df %>% filter( class %in% c( 1, 3, 5 ) ) %>% filter( math >= 50 )

class1 <- df %>% filter( class == 1 )
class2 <- df %>% filter( class == 2 )
class1
class2

# select() : 변수 추출
df %>% select( math )
df %>% select( science )

df %>% select( class, math, science )

df %>% select( -math )

# dplyr 함수 조합
df %>% 
  filter( class == 1 ) %>% 
  select( science )

df %>% 
  select( id, class, science ) %>% 
  head

df %>% 
  select( id, class, science ) %>% 
  sum

df %>% 
  select( id, class, science ) %>% 
  max

# arrange() : 정렬
df %>% arrange( science )
df %>% arrange( desc( science ) )

# mutate() : 파생변수 추가
df %>% 
  mutate( total = math + english + science, average = (math + english + science)/3 ) %>% 
  head

df %>% mutate( grade = ifelse( science >= 60, "pass", "fail" ) ) %>% 
  head

df %>% 
  mutate( total = math + english + science, average = (math + english + science)/3 ) %>%
  mutate( grade = ifelse( average >= 70, "pass", ifelse( average < 60, "fail", "normal" ) ) ) %>% 
  head

df %>% 
  mutate( total = math + english + science, average = (math + english + science)/3 ) %>%
  arrange( desc( average ) ) %>% 
  head

df.sort <- df %>% 
  mutate( total = math + english + science, average = (math + english + science)/3 ) %>%
  arrange( desc( average ) ) %>% 
  head

df
df.sort

# summarise() : 집단별 요약
# group_by() : 집단별 나누기
df %>% summarise( mean_math = mean( math ) )
df %>% group_by( class ) %>% 
  summarise( mean_math = mean( math ),
             mean_english = mean( english ),
             mean_science = mean( science ), 
             n = n() ) # n() :  빈도수

install.packages( "ggplot2" )

str( ggplot2::mpg )
mpg <- data.frame( ggplot2::mpg )
dim( mpg )
str( mpg )
head( mpg )
View( mpg )

mpg %>% 
  group_by( manufacturer, drv ) %>% 
  summarise( mean_cty = mean( cty ) ) %>% 
  head( 10 )

mpg %>% 
  group_by( manufacturer, drv ) %>% 
  filter( class == "suv" ) %>% 
  mutate( tot = ( cty + hwy ) / 2 ) %>% 
  summarise( mean_tot = mean( tot ) ) %>% 
  arrange( desc( mean_tot ) ) %>% 
  head( 5 )

# 데이터 합치기(병합)
# left_join() : 가로로 합치기 (변수 추가)
# inner_join() : 가로로 합치기 (변수 추가)
# full_join() : 가로로 합치기 (변수 추가)
# bind_rows() : 세로로 합치기 (data 추가)
df1 <- data.frame( id = c( 1, 2, 3, 4, 5 ),
                   midterm = c( 60, 80, 70, 90, 85 ) )

df2 <- data.frame( id = c( 1, 2, 3, 4, 5 ),
                   final = c( 60, 80, 70, 90, 85 ) )

total <- left_join( df1, df2, by = "id" ); total

df1 <- data.frame( id = c( 1, 2, 3 ),
                   address = c( "서울", "부산", "제주" ),
                   stringsFactors = F ) # stringsFactors=F: string을 factor로 만들어줄 가능성이 있으므로 설정함(Factor로 변환하지 않도록 함)
df2 <- data.frame( id = c( 1, 2, 4 ),
                   gender = c( "남", "여", "남" ) )

df_left <- left_join( df1, df2, by = "id" )
df_left
df_inner <- inner_join( df1, df2, by = "id" )
df_inner
df_full <- full_join( df1, df2, by = "id" )
df_full

df1 <- data.frame( id = c( 1, 2, 3, 4, 5 ),
                   test = c( 60, 80, 70, 90, 85 ) )

df2 <- data.frame( id = c( 1, 2, 3, 4, 5 ),
                   test = c( 60, 80, 70, 90, 85 ) )

df_all <- bind_rows( df1, df2 ) # 세로로 추가..(행 추가)
df_all

install.packages( "psych" )
library( psych )

summary( mtcars )
describe( mtcars ) # summary보다 더 많은 내용을 출력

install.packages( "descr" )
require( descr )

df <- data.frame( id = c( 1, 2, 4 ),
                   gender = c( "남", "여", "남" ) )

table( df$gender )

freq( df$gender )
freq( df$gender, plot = F )
