library( rJava )
library( xlsx )
library( tidyverse )

getwd()
setwd( "D:/workR/problem/team_training/data" )

excel_o <- read.xlsx( "9.8.2_사업자_현황Ⅱ_지역_업태_2005_2019.xlsx", sheetIndex = 1, encoding = "UTF-8", startRow = 2 )
df_o <- excel_o

# 컬럼이름만 바꿈
colnames( df_o ) <- c( "지역", 
                       "2014_총계", "2014_신규", "2014_폐업",
                       "2015_총계", "2015_신규", "2015_폐업",
                       "2016_총계", "2016_신규", "2016_폐업",
                       "2017_총계", "2017_신규", "2017_폐업",
                       "2018_총계", "2018_신규", "2018_폐업" )
str( df_o )

# 지역별 백분율 계산 컬럼 추가
df.rate <- df_o %>% 
  mutate( 폐업률2014 = ( df_o$"2014_폐업" / ( df_o$"2014_총계" + df_o$"2014_폐업" ) ) * 100.0,
          폐업률2015 = ( df_o$"2015_폐업" / ( df_o$"2015_총계" + df_o$"2015_폐업" ) ) * 100.0,
          폐업률2016 = ( df_o$"2016_폐업" / ( df_o$"2016_총계" + df_o$"2016_폐업" ) ) * 100.0,
          폐업률2017 = ( df_o$"2017_폐업" / ( df_o$"2017_총계" + df_o$"2017_폐업" ) ) * 100.0,
          폐업률2018 = ( df_o$"2018_폐업" / ( df_o$"2018_총계" + df_o$"2018_폐업" ) ) * 100.0 )

# 제주 지역 년도별 폐업률 데이터
df <- df.rate %>% 
  filter( 지역 %in% "제주" ) %>%
  select( 지역, 폐업률2014, 폐업률2015, 폐업률2016, 폐업률2017, 폐업률2018 )
 
# '전국평균' 구하기(*세종 제외)
df.avg <- df.rate %>%
  filter( 지역 != "세종" ) %>% select( 폐업률2014, 폐업률2015, 폐업률2016, 폐업률2017, 폐업률2018 ) %>% colMeans()

# 그래프 변수 선언
gpl <- NULL

# 그래프 변경 시 아래 값 수정
year <- 2014:2018
title <- "제주의 5개년(2014~2018) 폐업률"
xLabel <- "년도"
yLabel <- "폐업률(%)"

# 지역별 데이터 선 그래프 그리기
areaNm <- df[ 1, "지역" ]
cnt <- as.vector( as.matrix( df[ 1, 2:ncol( df ) ] )[ 1, ] )
df.area <- data.frame( 년도 = year, 폐업률 = cnt )
areaColor <- "#F14B69"

# 그래프 데이터 선 그리기
ggplot() + 
  ggtitle( title ) + 
  labs( x = xLabel, y = yLabel ) + 
  geom_line( data = df.area, col = areaColor, mapping = aes( x = 년도, y = 폐업률, group = 1 ), size = 2 ) +
  geom_text( data = df.area , mapping = aes( x = 2018, y = 폐업률[5], group = 1 ),
             size = 5, col = areaColor, nudge_x = 0.2, label = areaNm ) +
  # 전국평균 선
  geom_line( aes( x = year, y = df.avg, group = 1 ), size = 2 ) +
  geom_text( aes( x = 2018, y = df.avg[5], group = 1 ),
             size = 5, col = "black", nudge_x = 0.3, label = "전국평균" )
