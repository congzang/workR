#
# kimhokyeong_191202.R
#
# 김호경 2019.11.29 / 2019.12.02
# 
# 문1) 
# >score
# m  f
# [1,] 10 21
# [2,] 40 60
# [3,] 60 70
# [4,] 20 30
# 
# 1. 위와 같은 내용의 matrix score를 생성
score <- matrix( c( 10, 40, 60, 20 ),
                 c( 21, 60, 70, 30 ),
                 nrow = 4, ncol = 2 )
colnames( score ) <- c( "m", "f" )

# 2. score의 열 이름을 각각 male, female로 바꾸시오
colnames( score ) <- c( "male", "female" )

# 3. 2행에 있는 모든 값을 출력
score[ 2, ]

# 4. female의 모든 값을 출력
score[ , "female" ]

# 5. 3행 2열의 값을 출력
score[ 3, 2 ]

# 
# 문2)
# R에서 제공하는 state.x77 데이터셋을 이용하여 작성
# 
# 1. state.x77을 변환하여 st에 data frame으로 작성
st <- data.frame( state.x77 )
st

# 2. st의 내용을 출력
st

# 3. st의 열 이름 출력
colnames( st )

# 4. st의 행 이름 출력
rownames( st )

# 5. st의 행의 개수와 열의 개수 출력
dim( st )

# 6. st의 요약 정보 출력
str( st )

# 7. st의 행별 합계와 평균 출력
apply( st, 1, sum )
apply( st, 1, mean )

# 8. st의 열별 합계와 평균 출력
apply( st, 2, sum )
apply( st, 2, mean )

# 9. Florida 주의 모든 정보 출력
st[ "Florida", ]

# 10. 50개 주의 수입(Income) 정보만 출력
st[ , "Income" ]

# 11. Texas 주의 면적(Area)을 출력
st[ "Texas", "Area" ]

# 12. Ohio 주의 인구(Population)와 수입(Income) 출력
st[ "Ohio", c( "Population", "Income" ) ]

# 13. 인구가 5,000 이상인 주의 데이터만 출력
subset( st, Population >= 5000 )

# 14. 수입이 4,500 이상인 주의 인구, 수입, 면적을 출력
tmp <- subset( st, Income >= 4500 )
tmp[ , c( "Population", "Income", "Area" ) ]

# 15. 수입이 4,500 이상인 주는 몇 개인지 출력
nrow( tmp )

# 16. 전체 면적(Area)이 100,000 이상이고, 결빙일수(Frost)가 120 이상인 주의 정보 출력
tmp <- subset( st, Area >= 100000 & Frost >= 120 )
tmp

# 17. 인구(Population)가 2,000 미만이고, 범죄율(Murder)이 12미만인 주의 정보 출력
tmp <- subset( st, Population < 2000 & Murder < 12.0 )
tmp

# 18. 문맹률(Illiteracy)이 2.0 이상인 주의 평균 수입은 얼마인지 출력
tmp <- subset( st, Illiteracy >= 2.0 )
avg_tmp <- apply( tmp, 2, mean )[ "Income" ]
avg_tmp

# 19. 문맹률(Illiteracy)이 2.0 미만인 주와 2.0 이상인 주의 평균 수입의 차이 출력
tmp2 <- subset( st, Illiteracy < 2.0 )
avg_tmp2 <- apply( tmp2, 2, mean )[ "Income" ]
abs( avg_tmp - avg_tmp2 )

# 20. 기대수명(Life Exp)이 가장 높은 주는 어디인지 출력
rownames( subset( st, Life.Exp == max( st[ , "Life.Exp" ] ) ) )

# 21 Pennsylvania 주보다 수입(Income)이 높은 주들 출력
penIncome <- st[ "Pennsylvania", "Income" ]
rownames( subset( st, Income > penIncome ) )

# 
# 문3)
# R에서 제공하는 mtcars 데이터셋은 자동차 모델에 대한 제원 정보를 담고 있다.
# 
# 1. 이 데이터셋의 자료구조 출력
str( mtcars )

# 2. 이 데이터셋의 행의 개수와 열의 개수 출력
dim( mtcars )

# 3. 이 데이터셋 열들의 자료형 출력
for( i in 1:ncol( mtcars ) ) {
  cat( colnames( mtcars )[i], ":", class( mtcars[ , i ] ), "\n" )
}

# 4. 연비(mpg)가 가장 좋은 자동차 모델 출력
subset( mtcars, mpg == max( mtcars[ , "mpg" ] ) )

# 5. gear가 4인 자동차 모델 중 연비가 가장 낮은 모델 출력
tmp <- subset( mtcars, gear == 4 )
tmp <- subset( tmp, mpg == min( tmp[ , "mpg" ] ) )
tmp

# 6. Honda Civic의 연비(mpg)와 gear 수 출력
mtcars[ "Honda Civic", c( "mpg", "gear" ) ]

# 7. Pontiac Firebird 보다 연비가 좋은 자동차 모델 출력
pf_mpg <- mtcars[ "Pontiac Firebird", "mpg" ]
subset( mtcars, mpg > pf_mpg )

# 8. 자동차 모델들의 평균 연비 출력
apply( mtcars, 2, mean)[ "mpg" ]

# 9. gear의 수 종류 출력
unique( mtcars[ , "gear" ] )

# 
# 문4)
# R에서 제공하는 airquality 데이터셋은 일별로 대기의 질을 측정한 정보를 담고 있다.
# 

# 1. 이 데이터셋의 자료구조 출력
class( airquality )

# 2. 이 데이터셋의 앞쪽 일부분 내용만 출력
head( airquality )

# 3. 기온(Temp)이 가장 높은 날은 언제인지 월(Month)과 일(Day) 출력
subset( airquality, Temp == max( airquality[ , "Temp" ] ) )[ , c( "Month", "Day" ) ]

# 4. 6월달에 발생한 가장 강한 바람(Wind)의 세기 출력
max( subset( airquality, Month == 6 )[ , "Wind" ] )

# 5. 7월 달의 평균 기온(Temp) 출력
mon7 <- subset( airquality, Month == 7 )
apply( mon7, 2, mean )[ "Temp" ]

# 6. 오존(Ozone) 농도가 100을 넘는 날은 며칠이나 되는지 출력
nrow( subset( airquality, Ozone > 100 ) )

# 
# 문5)
# 1. R에서 제공하는 state.x77 데이터셋에서 수입(Income)이 5,000 이상인 주의 데이터에서
# 수입(Income), 인구(Population), 면적(Area) 열의 값들만 추출하여 rich_state.csv에 저장
st <- data.frame( state.x77 )
st2 <- subset( st, Income >= 5000 )[ , c( "Income", "Population", "Area" )]
setwd( "D:/workR" )
write.csv( st2, "rich_state.csv", row.names = T, quote = F )

# 2. 1.에서 만든 rich_state.csv파일을 읽어서 ds 변수에 저장한 후 ds 내용 출력
ds <- read.csv( "rich_state.csv", header = T )
ds
