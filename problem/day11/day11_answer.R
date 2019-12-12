#
# day11_answer.R
#
# 10일차 차원 축소 / 지도 시각화 실습
#
#문1)
#R에서 제공하는 state.x77 데이터셋을 차원 축소하여 2차원 산점도와 3
#차원 산점도를 작성하시오. (state.x77은 매트릭스 타입이기 때문에 데이터프레임
#                으로 변환하여 실습한다.)

library( ggplot2 )
library( Rtsne )

ds <- data.frame( state.x77 )

## 중복 데이터 삭제 ( t-SNE에서는 중복데이터가 없어야 한다. )
dup = which( duplicated( ds ) )
dup

## Run t-SNE : 2D
tsne <- Rtsne( ds, dims = 2, perplexity = 10 )
df.tsne <- data.frame( tsne$Y )

ggplot( df.tsne, aes( x = X1, y = X2 ) ) +
  geom_point( size=2, color = 'red' )

# 3D plot
library( car )
library( rgl )
library( mgcv )

tsne <- Rtsne( ds, dims = 3, perplexity = 10 )
df.tsne <- data.frame( tsne$Y )
head( df.tsne )

scatter3d( x = df.tsne$X1, 
           y = df.tsne$X2, 
           z = df.tsne$X3,
           surface = FALSE )


#문2)
#R에서 제공하는 swiss 데이터셋을 차원 축소하여 2차원 산점도와 3차원
#산점도를 작성하시오.

library( Rtsne )

ds <- swiss

dup = which( duplicated( ds ) )
dup

tsne <- Rtsne( ds, dims = 2, perplexity = 10 )
df.tsne <- data.frame( tsne$Y )

ggplot( df.tsne, aes( x = X1, y = X2 ) ) +
  geom_point( size = 2, color = 'red' )

# 3D plot
library( car )
library( rgl )
library( mgcv )

tsne <- Rtsne( ds, dims = 3, perplexity = 10 )
df.tsne <- data.frame( tsne$Y )
head( df.tsne )

scatter3d( x = df.tsne$X1, 
           y = df.tsne$X2, 
           z = df.tsne$X3,
           surface = FALSE )

#문3) 
#R을 이용하여 지도를 출력하시오.
#(1) 서울시청을 중심으로 지도 크기는 600x600, 지도 유형은 roadmap인 지도를 출력
#하시오.

library( ggmap )

register_google( key = 'AIzaSyDlmljbgzrqBC-ug1Mr1Q1Y4gvEOkOcR_g' ) # 구글키 등록

gc <- geocode( enc2utf8( "서울시청" ) ) 
gc
cen <- as.numeric( gc )
map <- get_googlemap( center = cen,
                      size = c( 600,600 ),
                      zoom = 16,
                      maptype = 'roadmap' ) 
ggmap( map )


#(2) 금강산 지역을 근방으로 지도 크기는 500x500, 지도 유형은 hybrid, zoom은 8
#인 지도를 출력하시오.

gc <- geocode( enc2utf8( "금강산" ) ) 
gc
cen <- as.numeric( gc )
map <- get_googlemap( center = cen,
                      size = c(500,500),
                      zoom = 8,
                      maptype = 'hybrid' )
ggmap( map )

#(3) 강남역 근방으로 지도 크기는 640x640, 지도 유형은 roadmap, zoom은 16인 지
#도를 출력하시오.

gc <- geocode( enc2utf8( "강남역" ) ) 
gc
cen <- as.numeric( gc )
map <- get_googlemap( center = cen,
                      size = c(640,640),
                      zoom = 16,
                      maptype = 'roadmap' )
ggmap( map )

#(4) 지도 유형은 roadmap, zoom은 9인 경도 127.397692, 위도 36.337058 지역의 지
#도를 출력하시오.

cen <- c( 127.397692, 36.337058 )
map <- get_googlemap( center = cen,
                      size = c(640,640),
                      zoom = 9,
                      maptype = 'roadmap') 
ggmap(map)

#(5) 지도 유형은 roadmap, zoom은 10인 경도 135.502330, 위도 34.693594 지역의
#지도를 출력하시오.

cen <- c( 135.502330, 34.693594 )
map <- get_googlemap( center = cen,
                      size = c(640,640),
                      zoom = 10,
                      maptype = 'roadmap') 
ggmap( map )

#문4)
#R을 이용하여 서울시 한강 이남의 구청들의 위치에 마커와 구청 이름을
#지도 위에 표시하시오.

office.name <- c( '강남구청', '강동구청', '강서구청', '관악구청', '구로구청',
                  '금천구청', '동작구청', '서초구청', '송파구청', '양천구청', '영등포구청' )
gc <- geocode( enc2utf8( office.name ) ) 
gc
cen <- colMeans( as.matrix( gc ) ) 
map <- get_googlemap( center = cen,
                      size = c( 640, 640 ),
                      zoom = 11,
                      markers = gc,
                      maptype = 'roadmap' )
gmap <- ggmap( map )
gmap + 
  geom_text( data = data.frame( gc ),
             aes( x = lon, y = lat ),
             size = 3,
             label = office.name )

#문5)
#R을 이용하여 대한민국의 광역시를 지도 위에 출력하시오. 단, 마커와 광
#역시 이름을 함께 표시하시오.

office.name <- c( '광주광역시', '부산광역시', '인천광역시', '대구광역시',
                  '울산광역시', '대전광역시' )
gc <- geocode( enc2utf8( office.name ) )
gc
cen <- colMeans( as.matrix( gc ) ) 
map <- get_googlemap( center = cen,
                      size = c( 640, 640 ),
                      zoom = 7,
                      markers = gc,
                      maptype = 'roadmap')
gmap <- ggmap( map )
gmap + 
  geom_text( data = data.frame( gc ), 
             aes( x = lon,y = lat ), 
             size = 3,
             label = office.name ) 

#문6)
#R을 이용하여 서울, 경기, 강원 지역의 국립공원 위치를 지도 상에 마커로
#시하되 국립공원의 이름을 함께 표시하시오.

park.name <- c( '북한산', '설악산', '치악산', '오대산','태백산' )
gc <- geocode( enc2utf8( park.name ) )
gc
cen <- colMeans( as.matrix( gc ) ) 
map <- get_googlemap( center = cen,
                      size = c( 640, 640 ),
                      zoom = 8,
                      markers = gc,
                      maptype = 'roadmap' )
gmap <- ggmap( map )
gmap +
  geom_text( data = data.frame( gc ), 
             aes( x = lon, y = lat ), 
             size = 3,
             label = park.name )


#문7) 
#‘2018년도 시군구별 월별 교통사고 자료’로부터 서울시의 각 구별 1년 교
#통사고 발생건수를 지도상에 원의 크기로 나타내시오.

setwd( '경로지정' )
ds.tot <- read.csv( '도로교통공단_시도_시군구별_월별_교통사고(2018).csv' )
ds.seoul <- subset( ds.tot, 시도 == '서울' )
ds.year <- aggregate( ds.seoul[ , '발생건수' ],
                      by = list( 시군구 = ds.seoul$시군구 ), FUN = sum )
ds.year
gc <- geocode( enc2utf8( as.vector( ds.year$시군구 ) ) ) 
gc
cen <- colMeans( as.matrix( gc ) )
map <- get_googlemap( center = cen,
                      size = c( 640, 640 ),
                      zoom = 11,
                      maptype = 'roadmap' )
gmap <- ggmap(map)
gmap +
  geom_point( data = data.frame(gc),
              aes( x = lon, y = lat, size = ds.year$x ),
              alpha = 0.5,
              col = "red" ) +
  scale_size_continuous( range = c(1, 14 ) )

#문8)
#7번과 동일한 자료를 이용하여 제주시 1년 교통사고 발생건수를 지도상에 원의 크기로 나타내시오.

ds.jeju <- subset( ds.tot, 시도 == '제주' )
ds.year <- aggregate( ds.jeju[ , '발생건수' ],
                      by = list( 시군구 = ds.jeju$시군구 ), FUN = sum )
ds.year
gc <- geocode( enc2utf8( as.vector( ds.year$시군구 ) ) )
gc
cen <- colMeans( as.matrix( gc ) )
map <- get_googlemap( center = cen,
                      size = c( 640, 640 ),
                      zoom = 11,
                      maptype = 'roadmap' )
gmap <- ggmap(map)
gmap +
  geom_point( data = data.frame(gc),
              aes( x = lon, y = lat, size = ds.year$x ),
              alpha = 0.5,
              col = "red" ) +
  scale_size_continuous( range = c(1, 14 ) )
