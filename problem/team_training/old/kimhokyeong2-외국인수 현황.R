library( rJava )
library( xlsx )
library( tidyverse )

getwd()
setwd( "D:/workR/problem/team_training/data" )

#excel_o <- read.xlsx( "시도별_외국인주민_현황.xlsx", sheetIndex = 1, encoding = "UTF-8", startRow = 4 )
df_o <- excel_o
str( df_o )

colnames( df_o )
head( df_o)

df_o <- df_o %>% filter( 행정구역별.1. == "제주특별자치도" ) %>% 
  select( 합계.2, 합계.3, 합계.6, 합계.7, 합계.10, 합계.11, 합계.14, 합계.15 )

colnames( df_o ) <- c( "외국인주민수2015", "외국인근로자수2015", "외국인주민수2016", "외국인근로자수2016",
                       "외국인주민수2017", "외국인근로자수2017", "외국인주민수2018", "외국인근로자수2018" )
#######################
# 1.외국인주민수 현황 #
#######################
df <- df_o %>% select( 외국인주민수2015, 외국인주민수2016, 외국인주민수2017, 외국인주민수2018 ) %>% t()

df <- data.frame( 인원수 = df,
                  년도 = str_sub( rownames( df ), -4,  str_length( rownames( df ) ) ) )

# 그래프 변경 시 아래 값 수정
title <- "제주 4개년(2015~2018) 외국인주민수"
xLabel <- "년도"
yLabel <- "인원수(명)"

myColor <- "#F14B69"

# 그래프 그리기
ggplot() +
  geom_bar( data = df, mapping = aes( x = 년도, y = 인원수 ), fill = myColor, stat = "identity", width = 0.5 ) + 
  labs( x = xLabel, y = yLabel ) +
  ggtitle( title )

##########################
# 2. 외국인근로자수 현황 #
##########################
df <- df_o %>% select( 외국인근로자수2015, 외국인근로자수2016, 외국인근로자수2017, 외국인근로자수2018 ) %>% t()

df <- data.frame( 인원수 = df,
                  년도 = str_sub( rownames( df ), -4,  str_length( rownames( df ) ) ) )

# 그래프 변경 시 아래 값 수정
title <- "제주 4개년(2015~2018) 외국인근로자수"
xLabel <- "년도"
yLabel <- "인원수(명)"

# 그래프 그리기
ggplot() +
  geom_bar( data = df, mapping = aes( x = 년도, y = 인원수 ), fill = "#C12B49", stat = "identity", width = 0.5 ) + 
  labs( x = xLabel, y = yLabel ) +
  ggtitle( title )

##################################################
# 3. 외국인주민에서 외국인근로자가 차지하는 비율 #
##################################################
df <- df_o %>% select( 외국인주민수2018, 외국인근로자수2018 ) %>% t()

forgnJumin2018Cnt <- df_o$외국인주민수2018

df <- data.frame( 인원수 = df,
                  구분 = str_sub( rownames( df ), 1,  str_length( rownames( df ) ) - 5 ) )

title <- "2018년 제주도 외국인주민수에서 외국인근로자가 차지하는 비율"
xLabel <- "구분"

ggplot( data = df ) +
  geom_bar( mapping = aes( x = "", y = 인원수, fill = 구분 ), stat = "identity", color = "white" ) +
  coord_polar( theta = "y" ) + 
  scale_fill_manual( values = c( myColor, "gray" ) ) +
  geom_text( aes( x = "", y = 인원수 ), label = paste( round( df$인원수/forgnJumin2018Cnt*100, 2 ), "%" ), position = position_stack( vjust = 0.5 ),
             size = 8, color = "white" ) +
  ggtitle( title ) + 
  theme_void()
