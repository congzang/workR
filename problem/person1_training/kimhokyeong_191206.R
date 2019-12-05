#
# kimhokyeong_191206.R
# 
# 김호경 2019.12.06 / 2019.12.06
#
library( rJava )
library( xlsx )

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

df.compCnt.2015 <- df.compCnt.2015 %>% filter( df.compCnt.2015$"산업별.중분류" == "전산업" )
df.compCnt.2016 <- df.compCnt.2016 %>% filter( df.compCnt.2016$"산업별.중분류" == "전산업" )

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
df.compCnt.2017 <- df.compCnt.2017 %>% filter( df.compCnt.2017$"산업별.중분류" == "전산업" )

table( df.compCnt.2015[ , c( "지역별", "전체" ) ] )

barplot( df.compCnt.2015[ , c( "지역별", "전체" ) ] )

str(df.compCnt.2015)

##########
# 매출액 #
##########
df.compSales.2015to2016 <- read.xlsx( "기업규모별기업규모별·지역별·산업중분류별_매출액(2015~2016)_20191205144704.xlsx", sheetIndex = 1, encoding = "UTF-8", startRow = 2 )

