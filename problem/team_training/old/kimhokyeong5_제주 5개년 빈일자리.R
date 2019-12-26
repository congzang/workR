library( rJava )
library( xlsx )
library( tidyverse )

getwd()
setwd( "D:/workR/problem/team_training/data" )

excel_o1 <- read.xlsx( "시군구_단위_고용통계_(2014~2017)_제주_업종별.xlsx", sheetIndex = 1, encoding = "UTF-8", startRow = 2 )
excel_o2 <- read.xlsx( "시도_단위_고용통계_(2018)_전국_업종별.xlsx", sheetIndex = 1, encoding = "UTF-8", startRow = 2 )
df_o1 <- excel_o1
df_o2 <- excel_o2

str(df_o1)
str(df_o2)

# 필요 컬럼 추출, 컬럼이름 변경
df_o1 <- df_o1 %>% filter( 산업별 == "전산업") %>% select( 1, 3:ncol( df_o1 ) )
df_o2 <- df_o2 %>% filter( 산업분류별 == "전체" & 지역별 == "제주도" ) %>% select( 1, 3:ncol( df_o2 ) )

colnames( df_o1 ) <- c( "지역",
                       "전체종사자2014", "빈일자리수2014", "빈일자리율2014",
                       "전체종사자2015", "빈일자리수2015", "빈일자리율2015",
                       "전체종사자2016", "빈일자리수2016", "빈일자리율2016",
                       "전체종사자2017", "빈일자리수2017", "빈일자리율2017" )
colnames( df_o2 ) <- c( "지역", "전체종사자2018", "빈일자리수2018", "빈일자리율2018" )

df <- ( df_o1 %>% add_row( 지역 = "제주", 
                            전체종사자2014 = sum( df_o1$전체종사자2014 ), 빈일자리수2014 = sum( df_o1$빈일자리수2014 ), 빈일자리율2014 = sum( df_o1$빈일자리율2014 ),
                            전체종사자2015 = sum( df_o1$전체종사자2015 ), 빈일자리수2015 = sum( df_o1$빈일자리수2015 ), 빈일자리율2015 = sum( df_o1$빈일자리율2015 ),
                            전체종사자2016 = sum( df_o1$전체종사자2016 ), 빈일자리수2016 = sum( df_o1$빈일자리수2016 ), 빈일자리율2016 = sum( df_o1$빈일자리율2016 ),
                            전체종사자2017 = sum( df_o1$전체종사자2017 ), 빈일자리수2017 = sum( df_o1$빈일자리수2017 ), 빈일자리율2017 = sum( df_o1$빈일자리율2017 ) ) %>% 
            mutate( 전체종사자2018 = df_o2$전체종사자2018, 빈일자리수2018 = df_o2$빈일자리수2018, 빈일자리율2018 = df_o2$빈일자리율2018 ) )[ 3, ]

# 그래프 변수 선언
gpl <- NULL

# 그래프 변경 시 아래 값 수정
year <- 2014:2018
title <- "제주의 5개년(2014~2018) 빈일자리"
xLabel <- "년도"
yLabel <- "빈일자리수(개)"

areaNm <- df[ 1, "지역" ]
areaColor <- "#F14B69"
emptyCntCols <- which( startsWith( colnames( df ), "빈일자리수" ) )
emptyRateCols <- which( startsWith( colnames( df ), "빈일자리율" ) )
sec.axis.maxRate = max( df[ , emptyCntCols ] ) /  max( df[ , emptyRateCols ] )
v.emptyCnt <- as.vector( as.matrix( df[ 1, emptyCntCols ] )[ 1, ] )
v.emptyRate <- as.vector( as.matrix( df[ 1, emptyRateCols ] )[ 1, ] )

df <- data.frame( 년도 = year, 빈일자리수 = v.emptyCnt, 빈일자리율 = v.emptyRate )

# 그래프 그리기
ggplot() +
  geom_bar( data = df, stat = "identity", mapping = aes( x = 년도, y = 빈일자리수 ), fill = areaColor, width = 0.3 ) + 
  geom_line( data = df, col = "black", mapping = aes( x = 년도, y = 빈일자리율 * sec.axis.maxRate ), size = 2 ) +
  scale_y_continuous( sec.axis = sec_axis( ~.*( max( df$빈일자리수 ) /  max( df$빈일자리율 ) ), name = "빈일자리율(%)" ) ) +
  annotate( "text", x = df$년도, y = df$빈일자리율 * sec.axis.maxRate, label = paste( df$빈일자리율, "%" ), size = 5, hjust = 0.1, vjust = -1.2 )  +
  labs( x = xLabel, y = yLabel ) +
  ggtitle( title )