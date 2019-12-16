library( rJava )
library( xlsx )
library( tidyverse )

getwd()
setwd( "D:/workR/problem/team_training/data" )

excel_o <- read.xlsx( "9.8.7_가동사업자_현황Ⅱ__사업존속연수_지역_업태_2007_2019.xlsx", sheetIndex = 3, encoding = "UTF-8", startRow = 2 )
df_o <- excel_o

# 컬럼이름만 바꿈
colnames( df_o ) <- c( "지역", "2014_총사업자수", "2014_1년미만", "2014_1~5년미만", "2014_5년이상",
                       "2015_총사업자수", "2015_1년미만", "2015_1~5년미만", "2015_5년이상", 
                       "2016_총사업자수", "2016_1년미만", "2016_1~5년미만", "2016_5년이상", 
                       "2017_총사업자수", "2017_1년미만", "2017_1~5년미만", "2017_5년이상",
                       "2018_총사업자수", "2018_1년미만", "2018_1~5년미만", "2018_5년이상")

str( df_o )

# 년도별 상위2, 하위2 지역 확인
df.pickArea <- df_o %>% 
  mutate( rate2014_2 = (df_o$"2014_1년미만"/df_o$"2014_총사업자수")*100 ) %>%
  mutate( rate2015_2 = (df_o$"2015_1년미만"/df_o$"2015_총사업자수")*100 ) %>%
  mutate( rate2016_2 = (df_o$"2016_1년미만"/df_o$"2016_총사업자수")*100 ) %>%
  mutate( rate2017_2 = (df_o$"2017_1년미만"/df_o$"2017_총사업자수")*100 ) %>%
  mutate( rate2018_2 = (df_o$"2018_1년미만"/df_o$"2018_총사업자수")*100 ) %>%
  filter( 지역 != "세종" & 지역 != "제주"  ) %>% select( 지역, rate2014_2, rate2015_2, rate2016_2, rate2017_2, rate2018_2 )

v.top2.pickArea <- c()
v.low2.pickArea <- c()

for( i in 2:ncol( df.pickArea ) ) {
  rankCol <- df.pickArea[ , i ]
  sortResult <- df.pickArea[ order( rank( -(rankCol) ) ), ]

  v.top2.pickArea <- c( v.top2.pickArea, as.character( head( sortResult, 2 )$지역 ) ) # 비교를 위한 상위지역 2개
  v.low2.pickArea <- c( v.low2.pickArea, as.character( tail( sortResult, 2 )$지역 ) ) # 비교를 위한 하위지역 2개
}

v.top2.pickArea <- rownames( table( v.top2.pickArea )[ order( -(table( v.top2.pickArea )) ) ][ 1:2 ] )
v.low2.pickArea <- rownames( table( v.low2.pickArea )[ order( -(table( v.low2.pickArea )) ) ][ 1:2 ] )
graphArea <- c( "제주", v.top2.pickArea, v.low2.pickArea ) # 그래프에 표시될 지역(제주, 상위2, 하위2)

# 전국 평균
df.avg <- df_o %>% 
  mutate( rate2014_2 = (df_o$"2014_1년미만"/df_o$"2014_총사업자수")*100 ) %>%
  mutate( rate2015_2 = (df_o$"2015_1년미만"/df_o$"2015_총사업자수")*100 ) %>%
  mutate( rate2016_2 = (df_o$"2016_1년미만"/df_o$"2016_총사업자수")*100 ) %>%
  mutate( rate2017_2 = (df_o$"2017_1년미만"/df_o$"2017_총사업자수")*100 ) %>%
  mutate( rate2018_2 = (df_o$"2018_1년미만"/df_o$"2018_총사업자수")*100 ) %>%
  filter( 지역 != "세종" ) %>% select( rate2014_2, rate2015_2, rate2016_2, rate2017_2, rate2018_2 ) %>% colMeans()

# 지역 데이터만 추출
df <- df_o %>% 
  mutate( rate2014_2 = (df_o$"2014_1년미만"/df_o$"2014_총사업자수")*100 ) %>%
  mutate( rate2015_2 = (df_o$"2015_1년미만"/df_o$"2015_총사업자수")*100 ) %>%
  mutate( rate2016_2 = (df_o$"2016_1년미만"/df_o$"2016_총사업자수")*100 ) %>%
  mutate( rate2017_2 = (df_o$"2017_1년미만"/df_o$"2017_총사업자수")*100 ) %>%
  mutate( rate2018_2 = (df_o$"2018_1년미만"/df_o$"2018_총사업자수")*100 ) %>%
  filter( 지역 %in% graphArea ) %>% select( 지역, rate2014_2, rate2015_2, rate2016_2, rate2017_2, rate2018_2  )

col2 <- list( 제주 = "#F14B69", 서울 = "darkblue", 경기 = "blue", 인천 = "yellow", 강원 = "gray", 대전 = "steelblue", 
             충북 = "black", 충남 = "pink", 세종 = "green", 광주 = "orange", 전북 = "maroon", 
             전남 = "skyblue", 대구 = "coral", 경북 = "brown", 부산 = "darkblue", 울산 = "darkviolet",
             경남 = "aquamarine" )

year <- 2014:2018
gpl <- NULL
df.filter <- df
title <- "제주와 일부 타 지역 1년 미만 사업자 생존율"

# 지역별 데이터 선 그래프 그리기
for( i in 1:nrow( df.filter ) ) {
  areaNm <- df.filter[ i, "지역" ]
  cnt <- as.vector( as.matrix( df.filter[ i, 2:ncol( df.filter ) ] )[ 1, ] )
  df.area <- data.frame( 년도 = year, 생존율 = cnt )
  areaCol <- ifelse( areaNm == "제주", "#F14B69", "gray" )
  
  if( is.null( gpl ) ) {
    gpl <- ggplot() + ggtitle( title ) + labs( x = "년도", y = "생존율(%)" )
  }
  
  gpl <- gpl + 
    geom_line( data = df.area, col = areaCol, mapping = aes( x = 년도, y = 생존율, group = 1 ), size = 2 ) +
    geom_text( data = df.area , mapping = aes( x = 2018, y = 생존율[5], group = 1 ),
               size = 5, col = areaCol, nudge_x = 0.2, label = areaNm )
    
}

# 전국평균 선 그리기 추가
gpl + geom_line( aes( x = year, y = df.avg, group = 1 ), size = 2 ) +
      geom_text( aes( x = 2018, y = df.avg[5], group = 1 ),
                 size = 5, col = "black", nudge_x = 0.3, label = "전국평균" )
