#
# kimhokyeong.R
#
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

# 원본데이터에서 주민수에 외국인근로자수가 포함되므로 pie chart 그리기 위해 주민수-근로자수로 계산한 값을 넣어줌
df[ rownames( df ) == "외국인주민수2018", 1 ] <- df[ rownames( df ) == "외국인주민수2018", 1 ] - df[ rownames( df ) == "외국인근로자수2018", 1 ]

df <- data.frame( 인원수 = round( df / forgnJumin2018Cnt * 100, 2 ),
                 구분 = c( "외국인근로자 외 외국인", "외국인근로자" ) )

title <- "2018년 제주도 외국인주민수에서 외국인근로자가 차지하는 비율"
xLabel <- "구분"

ggplot( data = df ) +
  geom_bar( mapping = aes( x = "", y = 인원수, fill = 구분 ), stat = "identity", color = "white" ) +
  coord_polar( theta = "y" ) + 
  scale_fill_manual( values = c( myColor, "gray" ) ) +
  geom_text( aes( x = "", y = 인원수 ), 
             label = ifelse( df$구분 == "외국인근로자", paste( df$인원수, "%" ), "" ), 
             position = position_stack( vjust = 0.5 ),
             size = 8, color = "white" ) +
  ggtitle( title ) + 
  theme_void()

################
# 총생산금액   #
################
excel_o <- read.xlsx( "시도별_경제활동별_지역내총생산.xlsx", sheetIndex = 1, encoding = "UTF-8", startRow = 2 )
df_o <- excel_o
str( df_o )

colnames( df_o )

df_o <- df_o %>% filter( 시도별 == "제주특별자치도"　& !( 경제활동별 %in% c( "순생산물세", "총부가가치(기초가격)" ) ) ) %>% 
  select( 경제활동별, X2015년.기준년가격.연쇄..1, X2015년.기준년가격.연쇄..2, X2015년.기준년가격.연쇄..3 )

colnames( df_o ) <- c( "경제활동별", "생산금액2015", "생산금액2016", "생산금액2017" )

# 지역내총생산 금액
df.grdp <- df_o[ 1, c( "생산금액2015", "생산금액2016", "생산금액2017" ) ]

# 년도별 지역총생산 내 비율 변수 추가
df_o <- df_o %>% mutate( 생산금액2015rate = round( 생산금액2015 / df.grdp$"생산금액2015" * 100, 2 ),
                        생산금액2016rate = round( 생산금액2016 / df.grdp$"생산금액2016" * 100, 2 ),
                        생산금액2017rate = round( 생산금액2017 / df.grdp$"생산금액2017" * 100, 2 ) ) %>% 
  filter( 경제활동별 != "지역내총생산(시장가격)" )

# 상위 5개 업종만
df_o <- head( df_o[ order( df_o[ , "생산금액2017rate" ], decreasing = TRUE ), ], 5 )
df <- data.frame()

for( i in 1:nrow( df_o ) ) {
  addRowData <- data.frame( 경제활동별 = df_o[ i, "경제활동별" ],
                           년도 = as.factor( 2015:2017 ),
                           생산금액 = as.numeric( df_o[ i, c( "생산금액2015", "생산금액2016", "생산금액2017" ) ] ) )
  df <- rbind( df, addRowData )
}

# 그래프 변경 시 아래 값 수정
title <- "제주 3개년(2015~2017) 총생산 상위 5개 업종"
xLabel <- "업종명"
yLabel <- "총생산금액(원)"

grayByLv <- c( "#C0C0C0", "#A0A0A0", "#808080" )

# 그래프 그리기
ggplot() +
  geom_bar( data = df, mapping = aes( x = 경제활동별, y = 생산금액, fill = 년도 ), stat = "identity", position = "dodge", width = 0.5 ) + 
  scale_fill_manual( values = grayByLv ) +
  labs( x = xLabel, y = yLabel ) +
  ggtitle( title )

########################
# 읍면동별 외국인 밀집 #
########################
library( kormaps2014 )
library( moonBook2 )

excel_o <- read.xlsx( "읍면동별_세대_및_인구.xlsx", sheetIndex = 1, encoding = "UTF-8", startRow = 2 )
df_o <- excel_o
str( excel_o )

colnames( df_o )

# *** 년도 변경 시 select()의 변수명 변경 필요!!!
df_o <- df_o %>% select( 행정시별.2., 제주.인구.계...명., 외국인.계...명. ) %>% 
  filter( 행정시별.2. != "소계" ) %>% 
  mutate( code = "" )

colnames( df_o ) <- c( "행정구역별_읍면동", "총인구수", "외국인수", "code" )

df_o$행정구역별_읍면동 <- gsub( " ", "", df_o$행정구역별_읍면동 ) # whitespace제거

areacode <- changeCode( areacode ) # (*다시돌릴때는 history지우던가 이 부분 주석)

# changeCode()를 kormap3에 적용해야해서 myKormap3 변수 별도 생성(지도 출력시 에러 방지)
myKormap3 <- kormap3
myKormap3$name <- changeCode( myKormap3 )$name

# 제주 지역 code 별도 dataSet 생성
df_code.jeju <- myKormap3[ which( myKormap3$order == 1 & startsWith( as.character( myKormap3$code ), "390" ) ), ][ , c( "name", "code" ) ]

df_o$code <- df_code.jeju[ df_code.jeju$name == df_o$행정구역별_읍면동, "code" ]

# *** 년도 변경 시 title명 변경 필요!!!
ggChoropleth( df_o, kormap3, fillvar= "외국인수", 
              title = "2011 제주 읍면동별 외국인수 단계구분도",
              subarea = c( "제주" ) )

