#
# kimhokyeong1.R
# - 외국인주민현황
# 김호경 2019.12.21 / 2019.12.23
#
library( rJava )
library( xlsx )
library( tidyverse )

getwd()
setwd( "D:/workR/problem/team_training/data" )

excel_o <- read.xlsx( "시도별_외국인주민_현황.xlsx", sheetIndex = 1, encoding = "UTF-8", startRow = 4 )
df_o <- excel_o
str( df_o )

colnames( df_o )
head( df_o )

df_o <- df_o %>% filter( 행정구역별.1. == "제주특별자치도" ) %>% 
  select( 합계.1, 합계.2, 합계.3, 합계.4, 합계.5, 합계.6, 
          합계.8, 합계.9, 합계.10, 합계.11, 합계.12, 합계.13,
          합계.15, 합계.16, 합계.17, 합계.18, 합계.19, 합계.20,
          합계.22, 합계.23, 합계.24, 합계.25, 합계.26, 합계.27 )

colnames( df_o ) <- c( "외국인주민2015", "외국인근로자2015", "결혼이민자2015", "유학생2015", "동포2015", "기타2015",
                       "외국인주민2016", "외국인근로자2016", "결혼이민자2016", "유학생2016", "동포2016", "기타2016",
                       "외국인주민2017", "외국인근로자2017", "결혼이민자2017", "유학생2017", "동포2017", "기타2017",
                       "외국인주민2018", "외국인근로자2018", "결혼이민자2018", "유학생2018", "동포2018", "기타2018" )

#######################
# 1.외국인주민 현황 #
#######################
# df <- df_o %>% select( 외국인주민2015, 외국인주민2016, 외국인주민2017, 외국인주민2018 ) %>% t()
# 
# df <- data.frame( 인원수 = df,
#                  년도 = str_sub( rownames( df ), -4,  str_length( rownames( df ) ) ) )
# 
# # 그래프 변경 시 아래 값 수정
# title <- "제주 4개년(2015~2018) 외국인주민"
# xLabel <- "년도"
# yLabel <- "인원수(명)"
# 
myColor <- "#F14B69"
# 
# # 그래프 그리기
# ggplot() +
#   geom_bar( data = df, mapping = aes( x = 년도, y = 인원수 ), fill = myColor, stat = "identity", width = 0.5 ) + 
#   labs( x = xLabel, y = yLabel ) +
#   ggtitle( title )

##########################
# 2. 외국인근로자 현황 #
##########################
# df <- df_o %>% select( 외국인근로자2015, 외국인근로자2016, 외국인근로자2017, 외국인근로자2018 ) %>% t()
# 
# df <- data.frame( 인원수 = df,
#                  년도 = str_sub( rownames( df ), -4,  str_length( rownames( df ) ) ) )
# 
# # 그래프 변경 시 아래 값 수정
# title <- "제주 4개년(2015~2018) 외국인근로자"
# xLabel <- "년도"
# yLabel <- "인원수(명)"
# 
# # 그래프 그리기
# ggplot() +
#   geom_bar( data = df, mapping = aes( x = 년도, y = 인원수 ), fill = "#C12B49", stat = "identity", width = 0.5 ) + 
#   labs( x = xLabel, y = yLabel ) +
#   ggtitle( title )

##################################################
# 3. 외국인주민에서 외국인근로자가 차지하는 비율 #
##################################################
df <- df_o %>% select( 외국인근로자2018, 결혼이민자2018, 유학생2018, 동포2018, 기타2018 ) %>% t()

forgnJumin2018Cnt <- df_o$외국인주민2018

# 원본데이터에서 주민수에 외국인주민가 포함되므로 pie chart 그리기 위해 주민수-근로자수로 계산한 값을 넣어줌
# df[ rownames( df ) == "외국인주민2018", 1 ] <- df[ rownames( df ) == "외국인주민2018", 1 ] - df[ rownames( df ) == "외국인주민2018", 1 ]

# df <- data.frame( 인원수 = round( df / forgnJumin2018Cnt * 100, 2 ),
#                  구분 = c( "외국인근로자 외 외국인", "외국인근로자" ) )

df <- data.frame( 인원수 = df,
                  구분 = substr( rownames( df ), 1, str_length( rownames( df ) ) - 4 ) )

title <- "2018년 제주도 외국인주민에서 외국인근로자가 차지하는 비율"
xLabel <- "구분"

ggplot( data = df ) +
  geom_bar( mapping = aes( x = "", y = 인원수, fill = 구분 ), stat = "identity", color = "white" ) +
  coord_polar( theta = "y" ) + 
  ggtitle( title ) +
  theme_void()
