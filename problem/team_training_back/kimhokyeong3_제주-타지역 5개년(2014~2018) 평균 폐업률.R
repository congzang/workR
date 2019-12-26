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
  mutate( 폐업률 = ( ( df_o$"2014_폐업" / ( df_o$"2014_총계" + df_o$"2014_폐업" ) ) * 100 +
                    ( df_o$"2015_폐업" / ( df_o$"2015_총계" + df_o$"2015_폐업" ) ) * 100 +
                    ( df_o$"2016_폐업" / ( df_o$"2016_총계" + df_o$"2016_폐업" ) ) * 100 +
                    ( df_o$"2017_폐업" / ( df_o$"2017_총계" + df_o$"2017_폐업" ) ) * 100 +
                    ( df_o$"2018_폐업" / ( df_o$"2018_총계" + df_o$"2018_폐업" ) ) * 100 ) / 5 )

# 년도별 상위2, 하위2 지역명만 추출(*세종 제외)
df.pickArea <- df.rate %>% 
  filter( 지역 != "세종" & 지역 != "제주" & 지역 != "소계" ) %>% select( 지역, 폐업률 )

v.top2.pickArea <- c()
v.low2.pickArea <- c()

rankCol <- df.pickArea$"폐업률"
sortResult <- df.pickArea[ order( rank( -(rankCol) ) ), ]

v.top2.pickArea <- c( v.top2.pickArea, as.character( head( sortResult, 2 )$지역 ) ) # 비교를 위한 상위지역명 2개
v.low2.pickArea <- c( v.low2.pickArea, as.character( tail( sortResult, 2 )$지역 ) ) # 비교를 위한 하위지역명 2개

v.top2.pickArea <- rownames( table( v.top2.pickArea )[ order( -(table( v.top2.pickArea )) ) ][ 1:2 ] )
v.low2.pickArea <- rownames( table( v.low2.pickArea )[ order( -(table( v.low2.pickArea )) ) ][ 1:2 ] )
graphAreaNm <- c( v.top2.pickArea, v.low2.pickArea, "제주" ) # 그래프에 표시될 지역명(상위2+하위2+제주)

# 제주 + 상위2 + 하위2 지역의 데이터 추출
df <- df.rate %>%
  filter( 지역 %in% graphAreaNm ) %>% select( 지역, 폐업률 ) %>%
  mutate( barColor = ifelse( 지역 == "제주", "#F14B69", "gray" ) )

# 그래프 변경 시 아래 값 수정
title <- "제주와 일부 타 지역 5개년(2014~2018) 평균 폐업률"
xLabel <- "지역"
yLabel <- "폐업률(%)"

# '전국평균' 구하기(*세종 제외)
df.avg <- df.rate %>%
  filter( 지역 != "세종" ) %>% select( 폐업률 ) %>% colMeans()

ggplot() +
  geom_bar( stat = "identity", mapping = aes( x = "전국평균", y = df.avg ), fill = "black", width = 0.7 ) +
  scale_x_discrete( limits = c( "전국평균", graphAreaNm ) ) +
  geom_bar( data = df, stat = "identity", mapping = aes( x = 지역, y = 폐업률 ), fill = df$barColor, width = 0.7 ) + 
  labs( x = xLabel, y = yLabel ) +
  ggtitle( title ) + 
  scale_y_continuous( limits = c( 0, 15 ) )
  