#
# Web Scraping : 웹사이트에 있는 정보를 추출하는 컴퓨팅 기술
#     웹 문서(사이트)는 통상 텍스트와 이미지가 혼합되어 있는 HTML 형식으로 구성
#     웹스크래핑은 비구조화된 웹문서 자료를 정형화된(구조화된) 형태로 변환하여 데이터베이스나 스프레드시트에 저장, 분석할 수 있도록 하는 것
#
# 참고 : https://dr-hkim.github.io/Naver-News-Web-Scraping-using-Keywords-in-R/
#        http://datamining.dongguk.ac.kr/lectures/bigdata/_book/section-27.html            
#
# HTML (HyperText Markup Language)
#   팀 버너스리가 개발한 마크업 요소(tag)와 속성등을 이용하여 웹 페이지를 쉽게 작성할 수 있도록 하는 마크업 언어
# 
# <!DOCTYPE html>
# <html>
#     <body>
#       <p>This is a paragraph.</p>
#       <p>This is another paragraph.</p>
#     </body>
#  </html>
#
#
# XML(Extensible Markup Language)
#   XML은 서로 다른 유형의 데이터를 기술하는 마크업 언어
#   다른 종류의 시스템간 (특히 인터넷에 연결된 시스템)끼리 데이터를 쉽게 주고 받을 수 있도록 고안
#   HTML의 한계에 대한 대안
#
# <note>
#   <to>Tove</to>
#   <from>Jani</from>
#   <heading>Reminder</heading>
#   <body>Don't forget me this weekend!</body>
# </note>
#
#
# 웹 문서 가져오기
#
# 웹 크롤러(web crawler): 조직적, 자동화된 방법으로 웹을 탐색하는 컴퓨터 프로그램
# 웹 크롤링(web crawling): 웹 크롤러가 하는 작업
#
# R 에서 웹문서 가져오기
#
# 웹에 있는 데이터를 가져오는 단계
#     요청: GET과 POST 방식
#     추출 및 저장
# 관련 R 패키지
#   XML, RCurl, httr, rvest, …
#
# 
install.packages( "rvest" )

## 네이버 뉴스에서 원하는 키워드의 검색 결과를 웹크롤링(스크래핑)하는 코드
## 제작: hkim (dr-hkim.github.io)

## 패키지 불러오기
library(rvest)
library(dplyr)

## 변수 입력하기
QUERY <- "제주" # 검색키워드
DATE  <- as.Date( as.character( 20191211 ), format = "%Y%m%d" ) # 검색시작날짜 & 검색종료날짜
DATE  <- format( DATE, "%Y.%m.%d" )
PAGE  <- 1

naver_url_1 <- "https://search.naver.com/search.naver?&where=news&query="
naver_url_2 <- "&pd=3&ds="
naver_url_3 <- "&de="
naver_url_4 <- "&start="

## 날짜 리스트 만들기
DATE_START <- as.Date( as.character( 20191211 ), format = "%Y%m%d" ) # 시작일자
DATE_END   <- as.Date( as.character( 20191211 ), format = "%Y%m%d" ) # 종료일자
DATE <- DATE_START:DATE_END
DATE <- as.Date( DATE, origin = "1970-01-01" )

## 게시물 번호 리스트 만들기
PAGE <- seq( from = 1, to = 41, by = 10 ) # 시작값과 종료값을 지정해줄 수 있습니다.
PAGE <- seq( from = 1, by = 10, length.out = 5) # 시작값과 원하는 갯수를 지정할 수도 있습니다.

## 네이버 검색결과 url 리스트에서 관련기사 url 리스트 만들기
news_url <- c()
news_date <-c() 

for ( date_i in DATE ){
  for ( page_i in PAGE ){
    dt <- format( as.Date( date_i, origin = "1970-01-01" ), "%Y.%m.%d" )
    naver_url <- paste0( naver_url_1, QUERY, naver_url_2, dt, naver_url_3, dt, naver_url_4, page_i )
    html <- read_html( naver_url )
    temp <- unique( html_nodes( html, '#main_pack' ) %>%     # id= 는 # 을 붙인다
                      html_nodes( css = '.news ' ) %>%         # class= 는 css= 를 붙인다 
                      html_nodes( css = '.type01' ) %>%
                      html_nodes( 'a' )%>%
                      html_attr( 'href' ) )
    news_url <- c( news_url, temp )
    news_date <- c( news_date, rep( dt, length( temp ) ) )
  }
  print( dt ) # 진행상황을 알기 위함이니 속도가 느려지면 제외
}

NEWS0 <- as.data.frame( cbind( date = news_date, url = news_url, query = QUERY))
NEWS1 <- NEWS0[ which( grepl( "news.naver.com", NEWS0$url ) ), ]         # 네이버뉴스(news.naver.com)만 대상으로 한다   
NEWS1 <- NEWS1[ which( !grepl( "sports.news.naver.com", NEWS1$url ) ), ] # 스포츠뉴스(sports.news.naver.com)는 제외한다  
NEWS2 <- NEWS1[ !duplicated( NEWS1 ), ] # 중복된 링크 제거 


## 뉴스 페이지에 있는 기사의 제목과 본문을 크롤링
NEWS2$news_title   <- ""
NEWS2$news_content <- ""

for ( i in 1:dim( NEWS2 )[ 1 ] ){
  html <- read_html( as.character( NEWS2$url[ i ] ) )
  temp_news_title   <- repair_encoding( html_text( html_nodes( html, '#articleTitle' ) ), from = 'utf-8' )
  temp_news_content <- repair_encoding( html_text( html_nodes( html, '#articleBodyContents') ), from = 'utf-8' )
  if ( length( temp_news_title ) > 0 ) {
    NEWS2$news_title[ i ]   <- temp_news_title
    NEWS2$news_content[i] <- temp_news_content
  }
}

NEWS2$news_content <- gsub( "// flash 오류를 우회하기 위한 함수 추가\nfunction _flash_removeCallback()", "", NEWS2$news_content )
NEWS <- NEWS2 # 최종 결과 저장
NEWS
NEWS$news_content

# 워드클라우드
library( KoNLP )
useSejongDic()

word_data <- sapply( NEWS$news_content, extractNoun, USE.NAMES = F )
word_data

undata <- unlist( word_data )
undata

word_table <- table( undata )
word_table

undata2 <- undata[ nchar( undata ) >= 2 ]
undata2
word_table2 <- table( undata2 )
word_table2

sort( word_table2, decreasing = T )

library( wordcloud2 )

wordcloud2( word_table2, minRotation = -pi / 6, maxRotation = -pi / 6, rotateRatio = 1 )