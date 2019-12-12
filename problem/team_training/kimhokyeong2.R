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
df <- df_o
#df <- df[ , c( "지역", "2014_1년미만", "2015_1년미만", "2016_1년미만", "2017_1년미만", "2018_1년미만" ) ]
# df <- df[ , c( "지역", "2014_1~5년미만", "2015_1~5년미만", "2016_1~5년미만", "2017_1~5년미만", "2018_1~5년미만" ) ]
df <- df[ , c( "지역", "2014_5년이상", "2015_5년이상", "2016_5년이상", "2017_5년이상", "2018_5년이상" ) ]

col <- c( "red", "blue", "yellow", "gray", "steelblue", 
          "black", "pink", "green", "orange", "maroon", 
          "skyblue", "coral", "brown", "cyan", "darkviolet",
          "aquamarine", "darkgreen", "darkblue", "darksalmon" )

col2 <- list( 제주 = "blue", 서울 = "red", 경기 = "blue", 인천 = "yellow", 강원 = "gray", 대전 = "steelblue", 
            충북 = "black", 충남 = "pink", 세종 = "green", 광주 = "orange", 전북 = "maroon", 
            전남 = "skyblue", 대구 = "coral", 경북 = "brown", 부산 = "cyan", 울산 = "darkviolet",
            경남 = "aquamarine" )

year <- 2014:2018
gpl <- NULL
expt.area <- c( "서울", "경기" )  # 제외지역
df.filter <- df %>% filter( !(df$"지역" %in% expt.area) ) 
title <- "5년 이상 사업자 수"

for( i in 1:nrow( df.filter ) ) {
  areaNm <- df.filter[ i, "지역" ]
  cnt <- as.vector( as.matrix( df.filter[ i, 2:ncol( df.filter ) ] )[ 1, ] )
  df.area <- data.frame( 년도 = year, 가동사업자수 = cnt )

    if( !(areaNm %in% expt.area) ) {
    if( is.null( gpl ) ) {
      gpl <- ggplot() + ggtitle( title ) + labs( x = "년도", y = "사업자수(개)" )
    }
    
    gpl <- gpl + 
            geom_line( data = df.area, col = col2[[ areaNm ]], mapping = aes( x = 년도, y = 가동사업자수, group = 1 ), size = 0.7 ) +
            geom_text( data = df.area , mapping = aes( x = 2018, y = 가동사업자수[ 5 ], group = 1 ), 
                       size = 3, col = col2[[ areaNm ]], nudge_x = 0.15, label = areaNm )
    
  }
}

gpl
