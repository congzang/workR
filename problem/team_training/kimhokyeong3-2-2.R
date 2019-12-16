library( rJava )
library( xlsx )
library( tidyverse )

getwd()
setwd( "D:/workR/problem/team_training/data" )

excel_o <- read.xlsx( "9.8.2_사업자_현황Ⅱ_지역_업태_2005_2019.xlsx", sheetIndex = 1, encoding = "UTF-8", startRow = 2 )
df_o <- excel_o

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

# 컬럼이름만 바꿈
area <- c( "소계", "제주" )

colnames( df_o ) <- c( "지역", 
                       "2014_총계", "2014_신규", "2014_폐업",
                       "2015_총계", "2015_신규", "2015_폐업",
                       "2016_총계", "2016_신규", "2016_폐업",
                       "2017_총계", "2017_신규", "2017_폐업",
                       "2018_총계", "2018_신규", "2018_폐업" )

df <- df_o %>% 
  mutate( 폐업률2014 = (df_o$"2014_폐업" / ( df_o$"2014_총계" + df_o$"2014_폐업" ) ) * 100.0,
         폐업률2015 = (df_o$"2015_폐업" / ( df_o$"2015_총계" + df_o$"2015_폐업" ) ) * 100.0,
         폐업률2016 = (df_o$"2016_폐업" / ( df_o$"2016_총계" + df_o$"2016_폐업" ) ) * 100.0,
         폐업률2017 = (df_o$"2017_폐업" / ( df_o$"2017_총계" + df_o$"2017_폐업" ) ) * 100.0,
         폐업률2018 = (df_o$"2018_폐업" / ( df_o$"2018_총계" + df_o$"2018_폐업" ) ) * 100.0 ) %>% 
  filter( 지역 %in% area ) %>%
  select( 지역, 폐업률2014, 폐업률2015, 폐업률2016, 폐업률2017, 폐업률2018 )


df.filter <- df
title <- "제주 5개년(2014~2018) 폐업률"
year <- 2014:2018
gpl <- NULL

for( i in 1:nrow( df.filter ) ) {
  areaNm <- df.filter[ i, "지역" ]
  cnt <- as.vector( as.matrix( df.filter[ i, 2:ncol( df.filter ) ] )[ 1, ] )
  df.area <- data.frame( 년도 = year, 폐업률 = cnt )
  
  areaCol <- "black"
  
  if( areaNm == "제주" ) {
    areaCol <- "#F14B69"
    
  } else {
    areaNm <- "전국 평균"
  }
  
  if( is.null( gpl ) ) {
    gpl <- ggplot() + ggtitle( title ) + labs( x = "년도", y = "폐업률(%)" )
  }
  
  gpl <- gpl +
    geom_line( data = df.area, col = areaCol, mapping = aes( x = 년도, y = 폐업률, group = 1 ), size = 2 ) +
    geom_text( data = df.area , mapping = aes( x = 2018, y = 폐업률[5], group = 1 ),
               size = 5, col = areaCol, nudge_x = 0.3, label = areaNm )
}

gpl