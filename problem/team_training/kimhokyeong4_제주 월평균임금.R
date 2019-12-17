library( rJava )
library( xlsx )
library( tidyverse )

getwd()
setwd( "D:/workR/problem/team_training/data" )

# excel_o <- read.xlsx( "지역별_4월기준_상용근로자.xlsx", sheetIndex = 1, encoding = "UTF-8", startRow = 2 )
excel_o <- read.xlsx( "월평균_임금_및_임금상승률_시도.xlsx", sheetIndex = 1, encoding = "UTF-8", startRow = 2 )
df_o <- excel_o

str(df_o)

# 필요 컬럼만 추출, 컬럼이름 변경
# df_o <- df_o %>% select( 지역별, 
#                  상용근로일수..일., 상용총근로시간..시간., 상용월급여액..원.,
#                  상용근로일수..일..1, 상용총근로시간..시간..1, 상용월급여액..원..1,
#                  상용근로일수..일..2, 상용총근로시간..시간..2, 상용월급여액..원..2,
#                  상용근로일수..일..3, 상용총근로시간..시간..3, 상용월급여액..원..3,
#                  상용근로일수..일..4, 상용총근로시간..시간..4, 상용월급여액..원..4,
#                  상용근로일수..일..5, 상용총근로시간..시간..5, 상용월급여액..원..5 )

colnames( df_o ) <- c( "지역",
                       "2014_월평균임금액", "2014_임금상승률",
                       "2015_월평균임금액", "2015_임금상승률",
                       "2016_월평균임금액", "2016_임금상승률",
                       "2017_월평균임금액", "2017_임금상승률",
                       "2018_월평균임금액", "2018_임금상승률",
                       "2019_월평균임금액", "2019_임금상승률" )

# 월급여액 컬럼만
df.sal <- df_o %>% 
  select( 지역, "2014_월평균임금액", "2015_월평균임금액", "2016_월평균임금액", "2017_월평균임금액", "2018_월평균임금액", "2019_월평균임금액" ) %>% 
  filter( 지역 == "제주특별자치도" )
df.sal <- df.sal[ , -1 ] / 10000

# 임금상승률 컬럼만
df.salRate <- df_o %>% 
  filter( 지역 == "제주특별자치도" ) %>% 
  select( "2014_임금상승률", "2015_임금상승률", "2016_임금상승률", "2017_임금상승률", "2018_임금상승률", "2019_임금상승률" )


# 데이터 선 그래프 그리기
year <- 2014:2019

df <- data.frame( 년도 = year, 월급여액 = as.vector( as.numeric( df.sal[ 1, ] ) ) )

rate <- as.vector( as.numeric( df.salRate[ 1, ] ) )
df2 <- data.frame( 년도 = year, 임금상승률 = rate )


# 그래프 변경 시 아래 값 수정
title <- "제주의 5개년(2014~2019) 월 평균 급여액"
xLabel <- "년도"
yLabel <- "월 평균 급여액(만원)"

ggplot() +
  geom_bar( data = df, stat = "identity", mapping = aes( x = 년도, y = 월급여액 ), fill = "#F14B69", width = 0.3 ) + 
  labs( x = xLabel, y = yLabel ) +
  ggtitle( title )
