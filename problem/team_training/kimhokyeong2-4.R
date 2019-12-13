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

# 가공데이터..
area <- c( "충남", "광주", "경기", "강원", "서울", "제주" )

df <- df_o %>% 
  mutate( rate2014_2 = (df_o$"2014_1년미만"/df_o$"2014_총사업자수")*100 ) %>%
  mutate( rate2015_2 = (df_o$"2015_1년미만"/df_o$"2015_총사업자수")*100 ) %>%
  mutate( rate2016_2 = (df_o$"2016_1년미만"/df_o$"2016_총사업자수")*100 ) %>%
  mutate( rate2017_2 = (df_o$"2017_1년미만"/df_o$"2017_총사업자수")*100 ) %>%
  mutate( rate2018_2 = (df_o$"2018_1년미만"/df_o$"2018_총사업자수")*100 ) %>%
  filter( 지역 %in% area ) %>% select( 지역, rate2014_2, rate2015_2, rate2016_2, rate2017_2, rate2018_2  )



col2 <- list( 제주 = "red", 서울 = "darkblue", 경기 = "blue", 인천 = "yellow", 강원 = "gray", 대전 = "steelblue", 
             충북 = "black", 충남 = "pink", 세종 = "green", 광주 = "orange", 전북 = "maroon", 
             전남 = "skyblue", 대구 = "coral", 경북 = "brown", 부산 = "darkblue", 울산 = "darkviolet",
             경남 = "aquamarine" )

year <- 2014:2018
gpl <- NULL
df.filter <- df
title <- "제주와 일부 타 지역 1년 미만 사업자 생존율"

# cnt <- as.vector( as.matrix( df.filter[ 1, 2:ncol( df.filter ) ] )[ 1, ] )

for( i in 1:nrow( df.filter ) ) {
  areaNm <- df.filter[ i, "지역" ]
  cnt <- as.vector( as.matrix( df.filter[ i, 2:ncol( df.filter ) ] )[ 1, ] )
  df.area <- data.frame( 년도 = year, 생존율 = cnt )
  areaCol <- "gray"
  
  if( areaNm == "제주" ) {
    areaCol <- "#F14B69"
  } else if(  areaNm == "충남" |  areaNm == "경남" ) {
    areaCol <- "black"
  }
  
  
  if( is.null( gpl ) ) {
    gpl <- ggplot() + ggtitle( title ) + labs( x = "년도", y = "생존율(%)" )
  }
  
  gpl <- gpl + 
    geom_line( data = df.area, col = areaCol, mapping = aes( x = 년도, y = 생존율, group = 1 ), size = 2 ) +
    geom_text( data = df.area , mapping = aes( x = 2018, y = 생존율[5], group = 1 ),
               size = 5, col = areaCol, nudge_x = 0.2, label = areaNm )
  
}

gpl
