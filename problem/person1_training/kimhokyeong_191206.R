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

df.compCnt.2015 <- rename( df.compCnt.2015, 산업별.중분류 = "산업별.9차.중분류", 전체 = "전체..개.", 소상공인 = "소상공인..개.",
                           소기업 = "소기업..개.", 중기업 = "중기업..개.", 중소기업 = "중소기업..개.", 대기업 = "대기업..개." )
df.compCnt.2016 <- rename( df.compCnt.2016, 산업별.중분류 = "산업별.9차.중분류", 전체 = "전체..개..1", 소상공인 = "소상공인..개..1",
                           소기업 = "소기업..개..1", 중기업 = "중기업..개..1", 중소기업 = "중소기업..개..1", 대기업 = "대기업..개..1" )

df.compCnt.2015 <- df.compCnt.2015 %>% filter( df.compCnt.2015$"산업별.중분류" == "전산업" & df.compCnt.2015$"지역별" != "전국" )
df.compCnt.2016 <- df.compCnt.2016 %>% filter( df.compCnt.2016$"산업별.중분류" == "전산업" & df.compCnt.2016$"지역별" != "전국" )

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

df.compCnt.2017 <- rename( df.compCnt.2017, 산업별.중분류 = "산업별.10차.중분류", 대기업 = "중소기업범위초과" )
df.compCnt.2017 <- df.compCnt.2017 %>% filter( df.compCnt.2017$"산업별.중분류" == "전산업" & df.compCnt.2017$"지역별" != "전국" )

# rownames( df.compCnt.2015 ) <- df.compCnt.2015$"지역별"
# rownames( df.compCnt.2016 ) <- df.compCnt.2016$"지역별"
# rownames( df.compCnt.2017 ) <- df.compCnt.2017$"지역별"
df.compCnt.2015
df.compCnt.2016
df.compCnt.2017

##########
# 매출액 #
##########

# 2015~2016년 데이터에서 2015년, 2016년 data frame 생성
df.compSales.2015to2016_o <- read.xlsx( "기업규모별·지역별·산업중분류별_매출액(2015~2016).xlsx", sheetIndex = 1, encoding = "UTF-8", startRow = 2 )
df.compSales.2015to2016 <- df.compSales.2015to2016_o

# 사업체수 컬럼들만 결측치 NA로 변환
for( i in 3:ncol( df.compSales.2015to2016 ) ) {
  col <- df.compSales.2015to2016[ , i ]
  
  if( sum( col %in% c( "-", "X" ) ) > 0 ) {
    df.compSales.2015to2016[ , i ][ col %in% c( "-", "X" ) ] <- NA
  }
  
  df.compSales.2015to2016[ , i ] <- as.numeric( df.compSales.2015to2016[ , i ] )  # Factor->숫자형 변환
}

df.compSales.2015 <- df.compSales.2015to2016[ , 1:8 ]             # 2015 분리
df.compSales.2016 <- df.compSales.2015to2016[ , c( 1:2, 9:14 ) ]  # 2016 분리

df.compSales.2015 <- rename( df.compSales.2015, 산업별.중분류 = "산업별.9차.중분류" )
df.compSales.2016 <- rename( df.compSales.2016, 산업별.중분류 = "산업별.9차.중분류", 전체 = "전체.1", 소상공인 = "소상공인.1",
                           소기업 = "소기업.1", 중기업 = "중기업.1", 중소기업 = "중소기업.1", 대기업 = "대기업.1" )

df.compSales.2015 <- df.compSales.2015 %>% filter( df.compSales.2015$"산업별.중분류" == "전산업" & df.compSales.2015$"지역별" != "전국" )
df.compSales.2016 <- df.compSales.2016 %>% filter( df.compSales.2016$"산업별.중분류" == "전산업" & df.compSales.2016$"지역별" != "전국" )

# 2017년 데이터에서 2017년 data frame 생성
df.compSales.2017_o <- read.xlsx( "기업규모별·지역별·산업중분류별_매출액(2017).xlsx", sheetIndex = 1, encoding = "UTF-8", startRow = 2 )
df.compSales.2017 <- df.compSales.2017_o

# 사업체수 컬럼들만 결측치 NA로 변환
for( i in 3:ncol( df.compSales.2017 ) ) {
  col <- df.compSales.2017[ , i ]
  
  if( sum( col %in% c( "-", "X" ) ) > 0 ) {
    df.compSales.2017[ , i ][ col %in% c( "-", "X" ) ] <- NA
  }
  
  df.compSales.2017[ , i ] <- as.numeric( df.compSales.2017[ , i ] )  # Factor->숫자형 변환
}

df.compSales.2017 <- rename( df.compSales.2017, 지역별 = "지역별.1.", 산업별.중분류 = "산업별.10차.중분류.1.", 대기업 = "중소기업범위초과" )
df.compSales.2017 <- df.compSales.2017 %>% filter( df.compSales.2017$"산업별.중분류" == "전산업" & df.compSales.2017$"지역별" != "전국" )

# rownames( df.compSales.2015 ) <- df.compSales.2015$"지역별"
# rownames( df.compSales.2016 ) <- df.compSales.2016$"지역별"
# rownames( df.compSales.2017 ) <- df.compSales.2017$"지역별"
df.compSales.2015
df.compSales.2016
df.compSales.2017

##########
# 인구수 #
##########
population.jeju.2009to2018_o <- read.csv( "200912_201812_주민등록인구및세대현황_월간(제주).csv", header = TRUE, stringsAsFactors = FALSE )
population.seoul.2009to2018_o <- read.csv( "200912_201812_주민등록인구및세대현황_월간(서울).csv", header = TRUE, stringsAsFactors = FALSE )
population.chbk.2009to2018_o <- read.csv( "200912_201812_주민등록인구및세대현황_월간(충북).csv", header = TRUE, stringsAsFactors = FALSE )

population.jeju.2009to2018 <- population.jeju.2009to2018_o
population.seoul.2009to2018 <- population.seoul.2009to2018_o
population.chbk.2009to2018 <- population.chbk.2009to2018_o

# 인구수 결측치 처리(0으로 변환)
for( i in 2:ncol( population.jeju.2009to2018 ) ) {
  col <- population.jeju.2009to2018[ , i ]
  
  if( sum(  is.na( col ) ) > 0 ) {
    population.jeju.2009to2018[ , i ][ is.na( col ) ] <- 0
  }
}
for( i in 2:ncol( population.seoul.2009to2018 ) ) {
  col <- population.seoul.2009to2018[ , i ]
  
  if( sum(  is.na( col ) ) > 0 ) {
    population.seoul.2009to2018[ , i ][ is.na( col ) ] <- 0
  }
}
for( i in 2:ncol( population.chbk.2009to2018 ) ) {
  col <- population.chbk.2009to2018[ , i ]
  
  if( sum(  is.na( col ) ) > 0 ) {
    population.chbk.2009to2018[ , i ][ is.na( col ) ] <- 0
  }
}


