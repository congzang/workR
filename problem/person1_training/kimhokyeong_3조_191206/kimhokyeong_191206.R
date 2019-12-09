#
# kimhokyeong_191206.R
# 
# 김호경 2019.12.06 / 2019.12.06
#

# 패키지 설치
# install.packages( "xlsx" )
# install.packages( "rJava" )
# install.packages( "dplyr" )

library( rJava )
library( xlsx )
library( dplyr )

# data 파일 경로 설정
setwd( "D:/workR/problem/person1_training/data" )

############
# 사업체수 #
############

# 2015~2016년 데이터에서 2015년, 2016년 data frame 생성
df.compCnt.2015to2016_o <- read.xlsx( "기업규모별·지역별·산업중분류별_사업체수(2015~2016).xlsx", 
                                      sheetIndex = 1, encoding = "UTF-8", startRow = 2 )
df.compCnt.2015to2016 <- df.compCnt.2015to2016_o

# 사업체수 컬럼들만 결측치 NA로 변환
for( i in 3:ncol( df.compCnt.2015to2016 ) ) {
  col <- df.compCnt.2015to2016[ , i ]

  if( sum( col %in% c( "-", "X" ) ) > 0 ) {
    df.compCnt.2015to2016[ , i ][ col %in% c( "-", "X" ) ] <- NA
  }
  
  df.compCnt.2015to2016[ , i ] <- as.numeric( df.compCnt.2015to2016[ , i ] )  # Factor->숫자형 변환
}

df.compCnt.2015 <- df.compCnt.2015to2016[ , 1:8 ]             # 2015 분리
df.compCnt.2016 <- df.compCnt.2015to2016[ , c( 1:2, 9:14 ) ]  # 2016 분리

df.compCnt.2015 <- rename( df.compCnt.2015, 산업별.중분류 = "산업별.9차.중분류", 사업체수 = "전체..개.", 소상공인 = "소상공인..개.",
                           소기업 = "소기업..개.", 중기업 = "중기업..개.", 중소기업 = "중소기업..개.", 대기업 = "대기업..개." )
df.compCnt.2016 <- rename( df.compCnt.2016, 산업별.중분류 = "산업별.9차.중분류", 사업체수 = "전체..개..1", 소상공인 = "소상공인..개..1",
                           소기업 = "소기업..개..1", 중기업 = "중기업..개..1", 중소기업 = "중소기업..개..1", 대기업 = "대기업..개..1" )

df.compCnt.2015 <- df.compCnt.2015 %>% 
                    filter( df.compCnt.2015$"산업별.중분류" == "전산업" & df.compCnt.2015$"지역별" != "전국" ) %>% 
                    select( "지역별", "사업체수" ) %>% 
                    mutate( year = 2015 )
df.compCnt.2016 <- df.compCnt.2016 %>% 
                    filter( df.compCnt.2016$"산업별.중분류" == "전산업" & df.compCnt.2016$"지역별" != "전국" ) %>% 
                    select( "지역별", "사업체수" ) %>% 
                    mutate( year = 2016 )

# 2017년 데이터에서 2017년 data frame 생성
df.compCnt.2017_o <- read.xlsx( "기업규모별·지역별·산업중분류별_사업체수(2017).xlsx", 
                                sheetIndex = 1, encoding = "UTF-8", startRow = 2 )
df.compCnt.2017 <- df.compCnt.2017_o

# 사업체수 컬럼들만 결측치 NA로 변환
for( i in 3:ncol( df.compCnt.2017 ) ) {
  col <- df.compCnt.2017[ , i ]
  
  if( sum( col %in% c( "-", "X" ) ) > 0 ) {
    df.compCnt.2017[ , i ][ col %in% c( "-", "X" ) ] <- NA
  }
  
  df.compCnt.2017[ , i ] <- as.numeric( df.compCnt.2017[ , i ] )  # Factor->숫자형 변환
}

df.compCnt.2017 <- rename( df.compCnt.2017, 산업별.중분류 = "산업별.10차.중분류", 대기업 = "중소기업범위초과", 사업체수 = "전체" )
df.compCnt.2017 <- df.compCnt.2017 %>% 
                    filter( df.compCnt.2017$"산업별.중분류" == "전산업" & df.compCnt.2017$"지역별" != "전국" ) %>% 
                    select( "지역별", "사업체수" ) %>% 
                    mutate( year = 2017 )

df.compCnt <- bind_rows( df.compCnt.2015, df.compCnt.2016, df.compCnt.2017 ) # 2015~2017 전국 사업체수
# df.compCnt$year <- as.character( df.compCnt$year )
df.compCnt.city <- split( df.compCnt, df.compCnt$"지역별" ) # 2015~2017 지역별 사업체수
# str(df.compCnt.city)
# 사업체수 년도별 변화 그래프
city.ylim <- c( min( df.compCnt.city$"제주"$"사업체수" )-1000, max( df.compCnt.city$"제주"$"사업체수" )+1000 ) # y축 최대,최소값
plot( df.compCnt.city$"제주"$year, df.compCnt.city$"제주"$"사업체수", main = "제주", type = "b", xlab = "년도", ylab = "사업체수", ylim = city.ylim, col = "red" )
city.ylim <- c( min( df.compCnt.city$"서울"$"사업체수" )-1000, max( df.compCnt.city$"서울"$"사업체수" )+1000 ) # y축 최대,최소값
plot( df.compCnt.city$"서울"$year, df.compCnt.city$"서울"$"사업체수", main = "서울", type = "b", xlab = "년도", ylab = "사업체수", ylim = city.ylim )
city.ylim <- c( min( df.compCnt.city$"충북"$"사업체수" )-1000, max( df.compCnt.city$"충북"$"사업체수" )+1000 ) # y축 최대,최소값
plot( df.compCnt.city$"충북"$year, df.compCnt.city$"충북"$"사업체수", main = "충북", type = "b", xlab = "년도", ylab = "사업체수", ylim = city.ylim, col = "blue" )

##########
# 매출액 #
##########

# 2015~2016년 데이터에서 2015년, 2016년 data frame 생성
df.compSales.2015to2016_o <- read.xlsx( "기업규모별·지역별·산업중분류별_매출액(2015~2016).xlsx", sheetIndex = 1, encoding = "UTF-8", startRow = 2, stringsAsFactors = FALSE )
df.compSales.2015to2016 <- df.compSales.2015to2016_o

# 사업체수 컬럼들만 결측치 NA로 변환
for( i in 3:ncol( df.compSales.2015to2016 ) ) {
  col <- df.compSales.2015to2016[ , i ]
  
  if( sum( col %in% c( "-", "X" ) ) > 0 ) {
    df.compSales.2015to2016[ , i ][ col %in% c( "-", "X" ) ] <- NA
  }
}

df.compSales.2015 <- df.compSales.2015to2016[ , 1:3 ]          # 2015 분리
df.compSales.2016 <- df.compSales.2015to2016[ , c( 1:2, 9 ) ]  # 2016 분리

df.compSales.2015 <- rename( df.compSales.2015, 매출액 = "전체" )
df.compSales.2016 <- rename( df.compSales.2016, 매출액 = "전체.1" )

df.compSales.2015 <- df.compSales.2015 %>% 
                      filter( df.compSales.2015$"산업별.9차.중분류" == "전산업" & df.compSales.2015$"지역별" != "전국" ) %>% 
                      select( "지역별", "매출액" ) %>% 
                      mutate( year = 2015 )

df.compSales.2016 <- df.compSales.2016 %>% 
                      filter( df.compSales.2016$"산업별.9차.중분류" == "전산업" & df.compSales.2016$"지역별" != "전국" ) %>% 
                      select( "지역별", "매출액" ) %>% 
                      mutate( year = 2016 )


# 2017년 데이터에서 2017년 data frame 생성
df.compSales.2017_o <- read.xlsx( "기업규모별·지역별·산업중분류별_매출액(2017).xlsx", sheetIndex = 1, encoding = "UTF-8", startRow = 2, stringsAsFactors = FALSE )
df.compSales.2017 <- df.compSales.2017_o

# 사업체수 컬럼들만 결측치 NA로 변환
for( i in 3:ncol( df.compSales.2017 ) ) {
  col <- df.compSales.2017[ , i ]
  
  if( sum( col %in% c( "-", "X" ) ) > 0 ) {
    df.compSales.2017[ , i ][ col %in% c( "-", "X" ) ] <- NA
  }
}

df.compSales.2017 <- rename( df.compSales.2017, 지역별 = "지역별.1.", 매출액 = "전체" )
df.compSales.2017 <- df.compSales.2017 %>% 
                      filter( df.compSales.2017$"산업별.10차.중분류.1." == "전산업" & df.compSales.2017$"지역별" != "전국" ) %>% 
                      select( "지역별", "매출액" ) %>% 
                      mutate( year = 2017 )

df.compSales <- bind_rows( df.compSales.2015, df.compSales.2016, df.compSales.2017 ) # 2015~2017 전국 사업체수
df.compSales$매출액 <- as.integer( df.compSales$매출액 )
df.compSales$year <- as.character( df.compSales$year )
df.compSales.city <- split( df.compSales, df.compSales$"지역별" ) # 2015~2017 지역별 사업체수

city.ylim <- c( min( df.compSales.city$"제주"$"매출액" )-1000, max( df.compSales.city$"제주"$"매출액" )+1000 ) # y축 최대,최소값
plot( df.compSales.city$"제주"$year, df.compSales.city$"제주"$"매출액", main = "제주", type = "b", xlab = "년도", ylab = "매출액", ylim = city.ylim, col = "red" )
city.ylim <- c( min( df.compSales.city$"서울"$"매출액" )-1000, max( df.compSales.city$"서울"$"매출액" )+1000 ) # y축 최대,최소값
plot( df.compSales.city$"서울"$year, df.compSales.city$"서울"$"매출액", main = "서울", type = "b", xlab = "년도", ylab = "매출액", ylim = city.ylim )
city.ylim <- c( min( df.compSales.city$"충북"$"매출액" )-1000, max( df.compSales.city$"충북"$"매출액" )+1000 ) # y축 최대,최소값
plot( df.compSales.city$"충북"$year, df.compSales.city$"충북"$"매출액", main = "충북", type = "b", xlab = "년도", ylab = "매출액", ylim = city.ylim, col = "blue" )

##########
# 인구수 #
##########
df.population.jeju.2009to2018_o <- read.csv( "200912_201812_주민등록인구및세대현황_월간(제주).csv", header = TRUE, stringsAsFactors = FALSE )
df.population.seoul.2009to2018_o <- read.csv( "200912_201812_주민등록인구및세대현황_월간(서울).csv", header = TRUE, stringsAsFactors = FALSE )
df.population.chbk.2009to2018_o <- read.csv( "200912_201812_주민등록인구및세대현황_월간(충북).csv", header = TRUE, stringsAsFactors = FALSE )

df.population.jeju.2009to2018 <- df.population.jeju.2009to2018_o
df.population.seoul.2009to2018 <- df.population.seoul.2009to2018_o
df.population.chbk.2009to2018 <- df.population.chbk.2009to2018_o

# 인구수 결측치 처리(0으로 변환)
for( i in 2:ncol( df.population.jeju.2009to2018 ) ) {
  col <- df.population.jeju.2009to2018[ , i ]
  
  if( sum(  is.na( col ) ) > 0 ) {
    df.population.jeju.2009to2018[ , i ][ is.na( col ) ] <- 0
  }
}
for( i in 2:ncol( df.population.seoul.2009to2018 ) ) {
  col <- df.population.seoul.2009to2018[ , i ]
  
  if( sum(  is.na( col ) ) > 0 ) {
    df.population.seoul.2009to2018[ , i ][ is.na( col ) ] <- 0
  }
}
for( i in 2:ncol( df.population.chbk.2009to2018 ) ) {
  col <- df.population.chbk.2009to2018[ , i ]
  
  if( sum(  is.na( col ) ) > 0 ) {
    df.population.chbk.2009to2018[ , i ][ is.na( col ) ] <- 0
  }
}

df.population.jeju.2009to2018 <- rename( df.population.jeju.2009to2018,
                                      X2009년 = "X2009년12월_총인구수",
                                      X2010년 = "X2010년12월_총인구수",
                                      X2011년 = "X2011년12월_총인구수",
                                      X2012년 = "X2012년12월_총인구수",
                                      X2013년 = "X2013년12월_총인구수",
                                      X2014년 = "X2014년12월_총인구수",
                                      X2015년 = "X2015년12월_총인구수",
                                      X2016년 = "X2016년12월_총인구수",
                                      X2017년 = "X2017년12월_총인구수",
                                      X2018년 = "X2018년12월_총인구수" )

df.population.seoul.2009to2018 <- rename( df.population.seoul.2009to2018,
                                      X2009년 = "X2009년12월_총인구수",
                                      X2010년 = "X2010년12월_총인구수",
                                      X2011년 = "X2011년12월_총인구수",
                                      X2012년 = "X2012년12월_총인구수",
                                      X2013년 = "X2013년12월_총인구수",
                                      X2014년 = "X2014년12월_총인구수",
                                      X2015년 = "X2015년12월_총인구수",
                                      X2016년 = "X2016년12월_총인구수",
                                      X2017년 = "X2017년12월_총인구수",
                                      X2018년 = "X2018년12월_총인구수" )

df.population.chbk.2009to2018 <- rename( df.population.chbk.2009to2018,
                                          X2009년 = "X2009년12월_총인구수",
                                          X2010년 = "X2010년12월_총인구수",
                                          X2011년 = "X2011년12월_총인구수",
                                          X2012년 = "X2012년12월_총인구수",
                                          X2013년 = "X2013년12월_총인구수",
                                          X2014년 = "X2014년12월_총인구수",
                                          X2015년 = "X2015년12월_총인구수",
                                          X2016년 = "X2016년12월_총인구수",
                                          X2017년 = "X2017년12월_총인구수",
                                          X2018년 = "X2018년12월_총인구수" )

df.population.jeju <- df.population.jeju.2009to2018 %>% 
                        filter( 행정구역 == "제주특별자치도  (5000000000)" ) %>%
                        select( "X2009년", "X2010년", "X2011년", "X2012년", "X2013년", "X2014년", "X2015년",
                                "X2016년", "X2017년", "X2018년" )

df.population.seoul <- df.population.seoul.2009to2018 %>% 
                        filter( 행정구역 == "서울특별시  (1100000000)" ) %>%
                        select( "X2009년", "X2010년", "X2011년", "X2012년", "X2013년", "X2014년", "X2015년",
                                "X2016년", "X2017년", "X2018년" )

df.population.chbk <- df.population.chbk.2009to2018 %>% 
                        filter( 행정구역 == "충청북도  (4300000000)" ) %>%
                        select( "X2009년", "X2010년", "X2011년", "X2012년", "X2013년", "X2014년", "X2015년",
                                "X2016년", "X2017년", "X2018년" )

df.population.jeju <- df.population.jeju / 10000
df.population.seoul <- df.population.seoul / 10000
df.population.chbk <- df.population.chbk / 10000

year <- 2009:2018
city.ylim <- c( min( df.population.seoul )-10, max( df.population.seoul )+10 ) # y축 최대,최소값
plot( year, df.population.seoul, main = "서울", type = "b", xlab = "년도", ylab = "인구수", ylim = city.ylim )
city.ylim <- c( min( df.population.jeju )-10, max( df.population.jeju )+10 ) # y축 최대,최소값
plot( year, df.population.jeju, main = "제주", type = "b", xlab = "년도", ylab = "인구수", ylim = city.ylim, col = "red" )
city.ylim <- c( min( df.population.chbk )-10, max( df.population.chbk )+10 ) # y축 최대,최소값
plot( year, df.population.chbk, main = "충북", type = "b", xlab = "년도", ylab = "인구수", ylim = city.ylim, col = "blue" )
