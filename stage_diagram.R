#
# 단계 구분도
#

#***** 필요시 설치 **
install.packages( "ggiraphExtra" ) # 단계구분도를 위한 Package
#******************** 

library( ggiraphExtra )

# 미국 주별 data set
dim( USArrests )
str( USArrests )
head( USArrests )

library( tibble )

# USArrests data set에 지역명 변수가 따로 없고, 대신 행이름이
# 지역명으로 되어있음
# tibble package의 rownames_to_column()를 이용해서 행 이름을
# state 변수로 바꿔서 data frame 생성
crime <- rownames_to_column( USArrests, var = "state" )
crime$state <- tolower( crime$state ) # 소문자로 수정
str( crime )

library( ggplot2 )

# 단계구분도를 만들려면 지역의 위도,경도 정보가 있는 지도 data 필요
# R에 내장된 map package에 미국 주별 위/경도를 나타내는 state가 있음

#***** 필요시 설치 **
#install.packages( "mapproj" )
#******************** 
#library( mapproj )

state_map <- map_data( "state" ) # ggplot2의 map_data()를 이용 data frame 생성 
str( state_map )

# ggiraphExtra package에 포함된 단계구분도 작성 함수
ggChoropleth( data = crime,
              aes( fill = Murder, 
                   map_id = state ),
              map = state_map,
              interactive = F )

ggChoropleth( data = crime,
              aes( fill = Murder, 
                   map_id = state ),
              map = state_map,
              interactive = T ) # interactive를 T로 하면 지도위에
# 마우스 움직임에 반응하는 
# interactive 단계구분도 작성


#
# 대한민국 지도기반 단계구분도
#

#***** 필요시 설치 **
install.packages( "stringi" )
install.packages( "devtools")
#******************** 

#***** 필요시 설치 **
devtools::install_github( "cardiomoon/kormaps2014" )
devtools::install_github( "cardiomoon/moonBook2" )
#******************** 

library( ggplot2 )
library( dplyr )
library( stringi )

library( ggiraphExtra )
library( kormaps2014 )
library( moonBook2 )

str( changeCode( areacode ) ) # kormaps2014 package의 changeCode()는

str( changeCode( kormap1 ) )  # encoding을 cp949로 변환
str( changeCode( korpop1 ) )

str( changeCode( kormap2 ) )
str( changeCode( korpop2 ) )

str( changeCode( kormap3 ) )
str( changeCode( korpop3 ) )

#theme_set( theme_gray( base_family = "NanumGothic" ) )

# 2015년도 시도별 인구분포 단계구분도
ggplot( korpop1, aes( map_id = code, fill = 총인구_명 ) ) +
  geom_map( map = kormap1, colour = "black", size = 0.1 )+
  expand_limits( x = kormap1$long, y = kormap2$lat ) +
  scale_fill_gradientn( colours = c( 'white', 'orange', 'red' ) ) +
  ggtitle( "2015년도 시도별 인구분포도" ) +
  coord_map()   

# ggChoropleth()이용 단계 구분도
ggChoropleth( korpop2, kormap2, fillvar = "남자_명" )

ggChoropleth( korpop3, kormap3, fillvar= "주택_계_호" )

# subarea 인수를 이용한 특정지역 표시 - Error
ggChoropleth( korpop3, kormap3, fillvar= "총인구_명",
              subarea = c( "전라", "광주" ) )


# ggChoropleth()이용 interactive plot
ggChoropleth( korpop2, kormap2, fillvar="남자_명",
              interactive = TRUE )

# Error
ggChoropleth( korpop3, kormap3, fillvar="남자_명",
              interactive = TRUE,
              subarea = c( "전라","광주" ),
              tooltip = "행정구역별_읍면동" )

# 결핵신환 발생 데이터 tbc - 국가통계포털( kosis.kr ) 제공
dim( tbc )
str( changeCode( tbc ) )

tbc.sub <- tbc[ tbc$year %in% c( 2001, 2005, 2010, 2015 ), ]

# 년도별로 면 분활된 단계구분도 
ggChoropleth( tbc.sub, kormap1, fillvar = "NewPts",
              facetvar = "year",
              tooltip = "name",
              interactive = TRUE ) # 한글 깨짐

