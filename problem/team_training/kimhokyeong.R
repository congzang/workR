library( rJava )
library( xlsx )
library( dplyr )
library( tidyverse )

setwd( "D:/workR/problem/team_training/data" )

excel_o <- read.xlsx( "지역별(전국표시)_창업기업수_20191212154254.xlsx", sheetIndex = 1, encoding = "UTF-8", startRow = 1 )
df2 <- excel_o
df <- df2[ -1, ]

# rownames( df ) <- df$지역별.1.
# df <- df[ , -1 ]

for( i in 2:ncol( df ) ) {
  df[ , i ] <- as.numeric( as.character( df[ , i ] ) )  # Factor->숫자형 변환
}

str( df )
df

year <- c( 2016.0, 2016.5 , 2017.0, 2017.5 , 2018.0, 2018.5, 2019.0 )

colnames( df ) <- c( "지역", year )
# 
# class( df[ 1, ] )
# year <- colnames( df )[ 2:ncol( df ) ]
# 
# str( df[ 1, 2:ncol( df ) ] )
# 
# colnames( df[ -1 ] )
# 
# year <- colnames( df[ -1 ] )
# cnt <- as.vector( as.matrix( df[ 1, 2:ncol( df ) ] )[ 1, ] )
# df.area <- data.frame( 년도 = year, 사업체수 = cnt )
# 
# gpl <- ggplot()
# gpl <- gpl + geom_line( data = df.area, col = "red", mapping = aes( x = 년도, y = 사업체수, group = 1 ) )
# 
# year <- colnames( df[ -1 ] )
# cnt <- as.vector( as.matrix( df[ 2, 2:ncol( df ) ] )[ 1, ] )
# df.area <- data.frame( 년도 = year, 사업체수 = cnt )
# gpl <- gpl + geom_line( data = df.area, col = "purple", mapping = aes( x = 년도, y = 사업체수, group = 1 ) )
# 
# gpl

col <- c( "red", "blue", "yellow", "gray", "steelblue", 
          "black", "pink", "green", "orange", "maroon", 
          "skyblue", "coral", "brown", "cyan", "darkviolet",
          "aquamarine", "darkgreen", "darkblue", "darksalmon" )

year <- colnames( df[ -1 ] )
gpl <- NULL
for( i in 2:nrow( df ) ) {
  cnt <- as.vector( as.matrix( df[ i, 2:ncol( df ) ] )[ 1, ] )
  df.area <- data.frame( 년도 = year, 사업체수 = cnt )
  
  if( is.null( gpl ) ) {
    gpl <- ggplot()
  }
  
  gpl <- gpl + geom_line( data = df.area, col = col[i], mapping = aes( x = 년도, y = 사업체수, group = 1 ), size = 1 )
}

gpl




# 전국 제외
for( i in 2:nrow( df ) ) {
  area_val <- df[ i, 2:ncol( df ) ]
  
  if( i == 2 ) {
    plot( year, area_val, type = "b", col = i, xlab = "반기", ylab = "사업체수", ylim = c( 4000, 200000 ) )
  
  } else {
    lines( year, area_val, type = "b", col = i )    
  }
}

# 전국, 서울, 경기 제외
isPlot <- FALSE

for( i in 2:nrow( df ) ) {
  area_nm <- df[ i, 1 ]
  area_val <- df[ i, 2:ncol( df ) ]
  
  if( area_nm != "서울" & area_nm != "경기" ) {
    if( !isPlot ) {
      plot( year, area_val, type = "b", col = i, xlab = "반기", ylab = "사업체수", ylim = c( 4000, 45000 ) )
      isPlot <- TRUE
      
    } else {
      lines( year, area_val, type = "b", col = i )    
    }
  }
}



