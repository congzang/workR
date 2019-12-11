# 
# kimhokyeong_191211.R
#
# 김호경 2019.12.10 / 2019.12.11
# 
# 문1)
# R에서 제공하는 state.x77 데이터셋을 차원 축소하여 2차원 산점도와 3
# 차원 산점도를 작성하시오. (state.x77은 매트릭스 타입이기 때문에 데이터프레임
#                 으로 변환하여 실습한다.)
library( Rtsne )
library( car )
library( rgl )
library( mgcv )

df <- data.frame( state.x77 )
str( df )
head( df )

dup <- which( duplicated( df ) ); dup # 중복데이터 없음

# 차원 축소
tsne_2 <- Rtsne( df, dims = 2, perplexity = 10 )
df.tsne_2 <- data.frame( tsne_2$Y )                # 2차원 차원 축소 결과 set
tsne_3 <- Rtsne( df, dims = 3, perplexity = 10 )
df.tsne_3 <- data.frame( tsne_3$Y )                # 3차원 차원 축소 결과 set

# 산점도
ggplot( df.tsne_2, aes( x = X1, y = X2 ) ) + 
  geom_point()                                                 # 2차원 산점도

scatter3d( x = df.tsne_3$X1, y = df.tsne_3$X2, z = df.tsne_3$X3 ) # 3차원 산점도

# 문2)
# R에서 제공하는 swiss 데이터셋을 차원 축소하여 2차원 산점도와 3차원
# 산점도를 작성하시오.
# 
dup <- which( duplicated( swiss ) ); dup

# 차원 축소
tsne_2 <- Rtsne( swiss, dims = 2, perplexity = 10 )
df.tsne_2 <- data.frame( tsne_2$Y )                     # 2차원 set
tsne_3 <- Rtsne( swiss, dims = 3, perplexity = 10 )
df.tsne_3 <- data.frame( tsne_3$Y )                     # 3차원 set

# 산점도
ggplot( df.tsne_2, mapping = aes( x = df.tsne_2$X1, y = df.tsne_2$X2 ) ) +
  geom_point()                                                    # 2차원 산점도

scatter3d( x = df.tsne_3$X1, y = df.tsne_3$X2, z = df.tsne_3$X3 ) # 3차원 산점도

# 문3) 
# R을 이용하여 지도를 출력하시오.
# (1) 서울시청을 중심으로 지도 크기는 600x600, 지도 유형은 roadmap인 지도를 출력
# 하시오.
library( ggmap )
register_google( key = "AIzaSyDL72thu4hYwYs6zG2k-NBAT3tL1ZApIgc" )

gc <- geocode( enc2utf8( "서울시청" ) ); gc
cen <- as.numeric( gc ); cen

map <- get_googlemap( center = cen, size = c( 600, 600 ), maptype = "roadmap" )
ggmap( map )

# (2) 금강산 지역을 근방으로 지도 크기는 500x500, 지도 유형은 hybrid, zoom은 8
# 인 지도를 출력하시오.
gc <- geocode( enc2utf8( "금강산" ) ); gc
cen <- as.numeric( gc ); cen

map <- get_googlemap( center = cen, size = c( 500, 500 ), maptype = "hybrid", zoom = 8 )
ggmap( map )

# (3) 강남역 근방으로 지도 크기는 640x640, 지도 유형은 roadmap, zoom은 16인 지
# 도를 출력하시오.
gc <- geocode( enc2utf8( "강남역" ) )
cen <- as.numeric( gc )

map <- get_googlemap( center = cen, size = c( 640, 640 ), maptype = "roadmap", zoom = 16 )
ggmap( map )

# (4) 지도 유형은 roadmap, zoom은 9인 경도 127.397692, 위도 36.337058 지역의 지
# 도를 출력하시오.
map <- get_googlemap( center = c( 127.397692, 36.337058 ), zoom = 9, maptype = "roadmap" )
ggmap( map )

# (5) 지도 유형은 roadmap, zoom은 10인 경도 135.502330, 위도 34.693594 지역의
# 지도를 출력하시오.
map <- get_googlemap( center = c( 135.502330, 34.693594 ), maptype = "roadmap", zoom = 10 )
ggmap( map )

# 
# 문4)
# R을 이용하여 서울시 한강 이남의 구청들의 위치에 마커와 구청 이름을
# 지도 위에 표시하시오.
gu_names <- c( "강서구청", "양천구청", "구로구청", "영등포구청", "금천구청", 
              "관악구청", "동작구청", "서초구청", "강남구청", "송파구청", "강동구청" )
gu_gcs <- geocode( enc2utf8( gu_names ) )

df <- data.frame( guname = gu_names, gu_lon = gu_gcs$lon, gu_lat = gu_gcs$lat )

cen <- c( mean( df$gu_lon ), mean( df$gu_lat ) )
map <- get_googlemap( center = cen, marker = gu_gcs, maptype = "roadmap", zoom = 11 )
gmap <- ggmap( map )

gmap +
  geom_text( data = df, aes( x = gu_lon, y = gu_lat ), size = 5, label = df$guname )

# 문5)
# R을 이용하여 대한민국의 광역시를 지도 위에 출력하시오. 단, 마커와 광
# 역시 이름을 함께 표시하시오.
gwang_names <- c( "인천광역시", "대전광역시", "광주광역시", "대구광역시", "울산광역시", "부산광역시" )
gwang_gcs <- geocode( enc2utf8( gwang_names ) )

df <- data.frame( name = gwang_names, lon = gwang_gcs$lon, lat = gwang_gcs$lat )

cen <- c( mean( df$lon ), mean( df$lat ) )
map <- get_googlemap( center = cen, marker = gwang_gcs, maptype = "roadmap", zoom = 7 )
gmap <- ggmap( map )

gmap +
  geom_text( data = df, aes( x = lon, y = lat ), size = 5, label = df$name)

# 문6)
# R을 이용하여 서울, 경기, 강원 지역의 국립공원 위치를 지도 상에 마커로
# 시하되 국립공원의 이름을 함께 표시하시오.
korpark_names <- c( "설악산 국립공원", "오대산 국립공원", "북한산 국립공원", "치악산 국립공원", "태백산 국립공원" )
korpark_gcs <- geocode( enc2utf8( korpark_names ) )

df <- data.frame( name = korpark_names, lon = korpark_gcs$lon, lat = korpark_gcs$lat )

cen <- c( mean( df$lon ), mean( df$lat ) )
map <- get_googlemap( center = cen, marker = korpark_gcs, maptype = "roadmap", zoom = 7 )
gmap <- ggmap( map )

gmap +
  geom_text( data = df, aes( x = lon, y = lat ), size = 3, label = df$name, color = "red" )

# 문7) 
# ‘2018년도 시군구별 월별 교통사고 자료’로부터 서울시의 각 구별 1년 교
# 통사고 발생건수를 지도상에 원의 크기로 나타내시오.
library( dplyr )

setwd( "D:/workR/problem/day11" )

df <- read.csv( "도로교통공단_시도_시군구별_월별_교통사고(2018).csv" )
df.gu <- df %>% filter( 시도 == "서울" ) %>%
  group_by( 시군구 ) %>% summarise( 발생건수 = sum( 발생건수 ) )

gcs <- geocode( enc2utf8( as.vector( df.gu$시군구 ) )  )

cen <- c( mean( gcs$lon ), mean( gcs$lat ) )
map <- get_googlemap( center = cen, maptype = "roadmap", zoom = 11 )
gmap <- ggmap( map )

gmap +
  geom_point( data = df.gu, aes( x = gcs$lon, y = gcs$lat, size = 발생건수 ), alpha = 0.5, col = "blue" )  +
  scale_size_continuous( range = c( 1, 20 ) )

# 문8)
# 7번과 동일한 자료를 이용하여 제주시 1년 교통사고 발생건수를 지도상에 원의 크기로 나타내시오.# 
df <- read.csv( "도로교통공단_시도_시군구별_월별_교통사고(2018).csv" )
df.jejusi <- df %>% filter( 시도 == "제주" & 시군구 == "제주시" ) %>% 
  group_by( 시군구 ) %>% 
  summarise( 발생건수 = sum( 발생건수 ) )

gcs <- geocode( enc2utf8( as.vector( df.jejusi$시군구 ) )  )

map <- get_googlemap( center = c( gcs$lon, gcs$lat ), maptype = "roadmap", zoom = 12 )
gmap <- ggmap( map )

gmap +
  geom_point( data = df.jejusi, aes( x = gcs$lon, y = gcs$lat, size = 발생건수 ), alpha = 0.5, col = "blue" )  +
  scale_size_continuous( range = c( 1, 20 ) )
