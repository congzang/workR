#
# 10일차
#
install.packages( "tidyverse" )
library( tidyverse )  # 관련있는 package를 한번에 로드함

dim( mpg )
str( mpg )
head( mpg )
View( mpg )

# 산점도
ggplot( data = mpg ) 
  + geom_point( mapping = aes( x = displ, y = hwy ) ) # ggplot(data = 대상) + geom_*(함수) ex) geom_point:산점도


month <- c( 1, 2, 3, 4, 5, 6 )
rain <- c( 55, 50, 45, 50, 60, 70 )
df <- data.frame( month, rain )
df

# 막대
ggplot( df, aes( x = month, y = rain ) ) +
  geom_bar( stat = "identity", width = 0.7, fill = "steelblue" )

ggplot( df, aes( x = month, y = rain ) ) +
  geom_bar( stat = "identity", width = 0.7, fill = "steelblue" ) +
  ggtitle( "월별 강수량" ) +
  theme( plot.title = element_text( size = 25, face = "bold", colour = "steelblue" ) ) +
  labs( x = "월", y = "강수량" ) + coord_flip()

ggplot( iris, aes( x = Petal.Length ) ) +
  geom_histogram( binwidth = 1.0 ) # 꽃잎의 단위를 0.5

ggplot( iris, aes( x = Sepal.Width, fill = Species, color = Species ) ) +
  geom_histogram( binwidth = 0.5, position = "dodge" ) +
  theme( legend.position = "top" )


# ggplot2 scatter chart
ggplot( data = iris, mapping = aes( x = Petal.Length, y = Petal.Width ) ) +
  geom_point()

ggplot( data = iris ) +
  geom_point( mapping = aes( x = Petal.Length, y = Petal.Width ) )

ggplot( data = iris, mapping = aes( x = Petal.Length, y = Petal.Width, color = Species, shape = Species ) ) +
  geom_point( size = 3 ) +
  ggtitle( "꽃잎의 길이와 폭" ) +
  theme( plot.title = element_text( size = 25, face = "bold", colour = "red" ) )

# ggplot Box plot
ggplot( data = iris, mapping = aes( y = Petal.Length ) ) +
  geom_boxplot( fill = "yellow" )

ggplot( data = iris, mapping = aes( y = Petal.Length, fill = Species ) ) +
  geom_boxplot()
str(iris)

# ggplot Line chart
year <- 1937:1960
cnt <- as.vector( airmiles )
df <- data.frame( year, cnt )
head( df )

ggplot( df, aes( x = year, y = cnt ) ) +
  geom_line( col = "red" )

# ggplot 작성 graph 꾸미기( 공 통 )
str( economics )

# 회귀선 형식의 사선(geom_abline)
ggplot( economics, aes( x = date, y = psavert ) ) +
  geom_line() +
  geom_abline( intercept = 12.18671, slope = -0.0005444 ) # intercept : y 절편값, slope : 기울기

# 평행선(geom_hline, geom_vline)
x_inter <- filter( economics, psavert == min( economics$psavert ) )$date

ggplot( economics, aes( x = date, y = psavert ) ) +
  geom_line() +
  geom_hline( yintercept = mean( economics$psavert ), colour = "blue" ) + # 수평선
  geom_vline( xintercept = x_inter, colour = "red" )                      # 수직선

ggplot( economics, aes( x = date, y = psavert ) ) +
  geom_line() +
  geom_vline( xintercept = x_inter )


# 텍스트 추가( 점에 실제값이 표시됨 )
ggplot( airquality, aes( x = Day, y = Temp ) ) +
  geom_point() + 
  geom_text( aes( label = Temp, vjust = -1, hjust = -1 ) ) # vjust,hjust: 텍스트의 위치

# 영역 지정 및 화살표 표시
ggplot( mtcars, aes( x = wt, y = mpg ) ) +
  geom_point() +
  annotate( "rect", xmin = 3, xmax = 4, ymin = 12, ymax = 21, alpha = 0.5, fill = "skyblue", color = "red" ) +     # 사각형
  annotate( "segment", x = 2.5, xend = 3.7, y = 10, yend = 17, color = "red", arrow = arrow() ) +   # 화살표
  annotate( "text", x = 2.5, y = 10, label = "화살표당" )                                           # 텍스트


# Treemap
install.packages( "treemap" )
library( treemap )

data( GNI2014 )
dim( GNI2014 )
str( GNI2014 )
View( GNI2014 )

treemap( GNI2014, index = c( "continent", "iso3" ), # 계층구조
         vSize = "population",                      # 타일 크기 (population변수값에 따라 타일너비가 결정됨)
         vColor = "GNI",                            # 타일 컬러 (GNI변수값에 떠러 색깔이 지정되는데 색이 진할수록 GNI값이 높음)
         type = "value",                            # 타일 컬러링 방법
         bg.labels = "yellow",                      # 레이블 배경색 (continent레이블의 배경색)
         title = "World's GNI" )                    # 제목

st <- data.frame( state.x77 )
st <- data.frame( st, stname = rownames( st ) )
treemap( st, index = c( "stname" ), vSize = "Area", vColor = "Income", type = "value", title = "미국 주별 수입"  )

# 산점도에 Bubble 추가( Bubble chart )
symbols( st$Illiteracy, st$Murder,                  # 원의 x, y좌표
         circles = st$Population,                   # 원의 반지름
         inches = 0.3,                              # 원 크기 조절값
         fg = "white",                              # 원 테두리 색
         bg = "lightgray",                          # 원 바탕색
         lwd = 1.5,                                 # 원 테두리선 두께께
         xlab = "rate of Illiteracy",
         ylab = "crime( murder) rate",
         main = "Illiteracy and Crime" )

text( st$Illiteracy, st$Murder, rownames( st ), cex = 0.6, col ="brown" ) # 텍스트 출력할 x,y좌표, 출력한 텍스트, 폰트크기, 폰트컬러


# Libraries 예제 http://r-graph-gallery.com/
install.packages( "plotly" )
install.packages( "viridis" )
install.packages( "hrbrthemes" )
install.packages( "gapminder" )
library(ggplot2)
library(dplyr)
library(plotly)
library(viridis)
library(hrbrthemes)

# The dataset is provided in the gapminder library
library(gapminder)
data <- gapminder %>% filter(year=="2007") %>% dplyr::select(-year)

# Interactive version
p <- data %>%
  mutate(gdpPercap=round(gdpPercap,0)) %>%
  mutate(pop=round(pop/1000000,2)) %>%
  mutate(lifeExp=round(lifeExp,1)) %>%
  
  # Reorder countries to having big bubbles on top
  arrange(desc(pop)) %>%
  mutate(country = factor(country, country)) %>%
  
  # prepare text for tooltip
  mutate(text = paste("Country: ", country, "\nPopulation (M): ", pop, "\nLife Expectancy: ", lifeExp, "\nGdp per capita: ", gdpPercap, sep="")) %>%
  
  # Classic ggplot
  ggplot( aes(x=gdpPercap, y=lifeExp, size = pop, color = continent, text=text)) +
  geom_point(alpha=0.7) +
  scale_size(range = c(1.4, 19), name="Population (M)") +
  scale_color_viridis(discrete=TRUE, guide=FALSE) +
  theme_ipsum() +
  theme(legend.position="none")

# turn ggplot interactive with plotly
pp <- ggplotly(p, tooltip="text")
pp

# save the widget
# library(htmlwidgets)
# saveWidget(pp, file=paste0( getwd(), "/HtmlWidget/ggplotlyBubblechart.html"))
