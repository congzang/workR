library( rJava )
library( xlsx )
library( tidyverse )

getwd()
setwd( "D:/workR/problem/team_training/data" )

excel_o <- read.xlsx( "시도_단위_고용통계_(2018)_전국_업종별.xlsx", sheetIndex = 1, encoding = "UTF-8", startRow = 2 )

# 필요 컬럼 추출, 컬럼이름 변경
df_o <- excel_o %>% filter( 산업분류별 != "전체" & 지역별 == "제주도" ) %>% select( 2:ncol( excel_o ) )
colnames( df_o ) <- c( "산업분류", "전체종사자", "빈일자리수", "빈일자리율" )

# 산업분류명 문자열에서 앞뒤 코드 삭제(B. 광업(05~08) -> 광업)
df_o$산업분류 <- str_sub( df_o$산업분류, 4,  str_length( df_o$산업분류 ) )
df_o$산업분류 <- str_split( df_o$산업분류 , pattern = "\\(", simplify = TRUE )[,1]

# 그래프 변수 선언
gpl <- NULL

# 그래프 변경 시 아래 값 수정
year <- 2014:2018
title <- "2018년 제주 산업분류별 빈일자리수"
xLabel <- "분류명"
yLabel <- "빈일자리수(개)"

areaNm <- df_o[ 1, "지역" ]
areaColor <- "#F14B69"

# 그래프 그리기
ggplot() +
  geom_bar( data = df_o, stat = "identity", mapping = aes( x = 산업분류, y = 빈일자리수 ), fill = areaColor, width = 0.3 ) + 
  labs( x = xLabel, y = yLabel ) +
  ggtitle( title ) +
  coord_flip()
