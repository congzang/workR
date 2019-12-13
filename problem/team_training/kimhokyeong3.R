library( rJava )
library( xlsx )
library( tidyverse )

getwd()
setwd( "D:/workR/problem/team_training/data" )

excel_o <- read.xlsx( "9.8.2_사업자_현황Ⅱ_지역_업태_2005_2019.xlsx", sheetIndex = 1, encoding = "UTF-8", startRow = 2 )
df_o <- excel_o

str( df_o )

# 컬럼이름만 바꿈
area <- c( "강원", "전북", "경남", "대구", "대전", "제주" )

colnames( df_o ) <- c( "지역", 
                       "2014_총계", "2014_신규", "2014_폐업",
                       "2015_총계", "2015_신규", "2015_폐업",
                       "2016_총계", "2016_신규", "2016_폐업",
                       "2017_총계", "2017_신규", "2017_폐업",
                       "2018_총계", "2018_신규", "2018_폐업" )

df <- df_o %>% 
  mutate( 폐업률 = ( (df_o$"2014_폐업" / ( df_o$"2014_총계" + df_o$"2014_폐업" ) ) * 100 +
                         (df_o$"2015_폐업" / ( df_o$"2015_총계" + df_o$"2015_폐업" ) ) * 100 +
                         (df_o$"2016_폐업" / ( df_o$"2016_총계" + df_o$"2016_폐업" ) ) * 100 +
                         (df_o$"2017_폐업" / ( df_o$"2017_총계" + df_o$"2017_폐업" ) ) * 100 +
                         (df_o$"2018_폐업" / ( df_o$"2018_총계" + df_o$"2018_폐업" ) ) * 100 ) / 5 ) %>% 
  filter( 지역 %in% area ) %>% select( 지역, 폐업률 ) %>% 
  mutate( 막대색 = ifelse( 지역 == "제주", "#F14B69", ifelse( 지역 == "강원", "black", "gray" ) ) )


df.filter <- df
title <- "제주와 일부 타 지역 5개년(2014~2018) 평균 폐업률"

ggplot( data = df.filter ) +
  geom_bar( stat = "identity", mapping = aes( x = 지역, y = 폐업률 ), fill = df.filter$막대색, width = 0.7 ) + 
  labs( x = "지역", y = "폐업률(%)" ) +
  ggtitle( title )+ 
  scale_y_continuous( limits = c( 0, 15 ) ) 

