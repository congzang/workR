#
# kimhokyeong_191210.R
# 
# 김호경 2019.12.09 / 2019.12.10
# 
# 문1)
# R에서 제공하는 mtcars 데이터셋에서 gear(기어의 수)에 대해 ggplot으로
# 막대그래프를 작성하시오. 단, 제목과 x축 레이블은 ‘기어의 수’, y축 레이블
# 은 ‘빈도수’로 나타내시오.
ggplot( data = mtcars ) +
  geom_bar( mapping = aes( x = gear ) ) +
  labs( x = "기어의 수", y = "빈도수" ) +
  ggtitle( "기어의 수" )

# 문2)
# R에서 제공하는 mtcars 데이터셋에서 cyl(실린더의 수)에 대해 막대 색이
# 초록색인 막대그래프를 ggplot으로 작성하시오.
ggplot( data = mtcars ) +
  geom_bar( mapping = aes( x = cyl ), fill = "green" )

# 문3) 
# R에서 제공하는 mtcars 데이터셋에서 mpg(연비)에 대해 구간 간격이 5.0
# 인 히스토그램을 ggplot으로 작성하시오.
ggplot( data = mtcars ) +
  geom_histogram( mapping = aes( x = mpg ), binwidth = 5.0 )

# 
# 문4)
# R에서 제공하는 trees 데이터셋의 Girth(나무 둘레)에 대해 ggplot으로
# 히스토그램을 작성하시오. 여기에서는 히스토그램의 제목, x축 레이블, y축
# 레이블을 한글로 표시하시오. (구간 간격은 3.0, 막대의 색은 steelblue로 한다.)
ggplot( data = trees ) +
  geom_histogram( mapping = aes( x = Girth ), binwidth = 3.0, fill = "steelblue" ) +
  ggtitle( "나무 둘레" ) +
  labs( x = "나무둘레", y = "빈도수" )

# 문5)
# R에서 제공하는 mtcars 데이터셋에서 mpg(연비)를 x축으로 하고, wt(중
#                                             량)를 y축으로 하는 산점도를 ggplot으로 작성하시오. (단, 점의 색은 gear의
#                                                                                수에 따라 다르게 표시한다.)
ggplot( data = mtcars ) +
  geom_point( mapping = aes( x = mpg, y = wt, color = gear ) )

# 문6)
# R에서 제공하는 mtcars 데이터셋에서 mpg(연비)에 대해 ggplot으로 상
# 자그림을 작성하되, cyl(실린더 수)에 따라 그룹을 나누어 작성하시오.
ggplot( data = mtcars ) +
  geom_boxplot( mapping = aes( x = as.factor( mtcars$cyl ), y = mpg ) )

# 
# 문7) 
# 다음은 2015년부터 2026년도까지의 예상 인구수 추계 자료이다. 연도를
# x축으로 하여 ggplot으로 선그래프를 작성하시오.
# 
# 연도		총인구 (천명)		연도		총인구 (천명)
# 2015		51014				2021		52123
# 2016		51245				2022		52261
# 2017		51446				2023		52388
# 2018		51635				2024		52504
# 2019		51811				2025		52609
# 2020		51973				2026		52704	
year <- 2015:2026
pop <- c( 51014, 51245, 51446, 51635, 51811, 51973, 52123, 52261, 52388, 52504, 52609, 52704 )
df.pop <- data.frame( year, pop )

ggplot( data = df.pop ) +
  geom_line( mapping = aes( x = year, y = pop ) )

# 
# 문8)
# 다음과 같이 데이터셋 us를 생성한 후 물음에 답하시오. 여기서 state.x77
# 은 미국 50개 주의 통계정보가, state.division은 미국 50개 주의 지역 구분
# (예: 북부, 중부, 남부……) 정보가 저장된 데이터셋이다.
# 
us <- data.frame(state.x77, state.division)
str( us )
head( us )
View( us )

# 
# (1) 미국 50개 주에 대해 각각의 주들이 지역구분별로 묶인 트리맵을 작성하시오.
# 또한, 타일의 면적은 Population(인구수), 타일의 색은 Income(소득)으로 나타내고,
# 각각의 타일에는 주의 이름을 표시하시오. 마지막으로 이 트리맵에서 관찰할 수 있
# 는 것이 무엇인지 설명하시오
us$state <- rownames( us )
treemap( us, 
         index = c( "state.division", "state" ),
         vSize = "Population",
         vColor = "Income",
         type = "value" )

# East North Central, Middle Atlantic, Pacific 지역에 속하는 주들은 소득은 높은 편이다.
# Alaska주는 인구가 가장 적으나 소득은 가장 높다.

# (2) 미국 50개 주에 대해 각각의 주들이 지역구분별로 묶인 트리맵을 작성하시오.
# 또한, 타일의 면적은 HS.Grad(고등학교 졸업률), 타일의 색은 Murder(범죄률)로 나타
# 내고, 각각의 타일에는 주의 이름을 표시하시오. 마지막으로 이 트리맵에서 관찰할
# 수 있는 것이 무엇인지 설명하시오.
treemap( us, 
         index = c( "state.division", "state" ),
         vSize = "HS.Grad",
         vColor = "Murder",
         type = "value" )

# East South Central지역에 있는 주들은 다른 주들에 비해 범죄율이 높은 편이다.
# New England지역에 있는 주들은 범죄율이 대체로 낮고 고등학교 졸업률이 높은 편이다.
# 고등학교 졸업률이 낮은 편에 속하는 주들은 범죄율이 대체로 높다.

# (3) us 데이터셋에 대해 x축은 Income(소득), y축은 Illiteracy(문맹률), 원의 크기는
# Population(인구수), 원의 색은 green(초록색), 원 내부에는 주의 이름을 표시한 버
# 블차트를 작성하시오. 또한 이 버블차트에서 관찰할 수 있는 것이 무엇인지 설명하
# 시오.
symbols( us$Income, st$Illiteracy,
         circles = st$Population,
         bg = "green" )

text( us$Income, st$Illiteracy, rownames( st ), cex = 0.6 )

# 소득이 적은 주들은 인구수가 대체로 적다.
# 문맹률이 높은 주들은 소득이 대체로 낮고, 소득이 높은 주들은 문맹률이 적다.

# (4) us 데이터셋에 대해 x축은 Illiteracy(문맹률), y축은 Murder(범죄률), 원의 크기
# 는 Area(면적), 원의 색은 green(초록색), 원 내부에는 주의 이름을 표시한 버블차트
# 를 작성하시오. 또한 이 버블차트에서 관찰할 수 있는 것이 무엇인지 설명하시오.
symbols( us$Illiteracy, st$Murder,
         circles = st$Area,
         bg = "green" )

text( us$Illiteracy, st$Murder, rownames( st ), cex = 0.6 )

# 범죄율이 낮으면서 문맹률이 낮은 주들은 면적이 작다.
# 면적이 큰 주들은 범죄율이 높은 편이다.
