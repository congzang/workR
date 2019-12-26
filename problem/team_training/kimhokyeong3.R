#
# kimhokyeong3.R
# - 읍면동별 외국인 밀집
# 김호경 2019.12.21 / 2019.12.23
#
library( rJava )
library( xlsx )
library( tidyverse )
library( kormaps2014 )
library( moonBook2 )

getwd()
setwd( "D:/workR/problem/team_training/data" )

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

# changeCode()를 kormap3에 적용해야해서 myKormap3 변수 별도 생성(지도 출력 시 에러 방지)
myKormap3 <- kormap3
myKormap3$name <- changeCode( myKormap3 )$name

# 제주 지역 code 별도 dataSet 생성
df_code.jeju <- myKormap3[ which( myKormap3$order == 1 & startsWith( as.character( myKormap3$code ), "390" ) ), ][ , c( "name", "code" ) ]

df_o$code <- df_code.jeju[ df_code.jeju$name == df_o$행정구역별_읍면동, "code" ]

# *** 년도 변경 시 title명 변경 필요!!!
ggChoropleth( df_o, kormap3, fillvar= "외국인수", 
              title = "2011 제주 읍면동별 외국인수 단계구분도",
              subarea = c( "제주" ) )
