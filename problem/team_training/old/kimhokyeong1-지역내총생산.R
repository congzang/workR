library( rJava )
library( xlsx )
library( tidyverse )

getwd()
setwd( "D:/workR/problem/team_training/data" )

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
# myColorByLv <- c( "#F14B69", "#E13B59", "#C12B49" )

# 그래프 그리기
ggplot() +
  geom_bar( data = df, mapping = aes( x = 경제활동별, y = 생산금액, fill = 년도 ), stat = "identity", position = "dodge", width = 0.5 ) + 
  scale_fill_manual( values = grayByLv ) +
  labs( x = xLabel, y = yLabel ) +
  ggtitle( title )
