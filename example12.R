#
# 12일차
#
# 워드 클라우드( Word Cloud )
#
# 한글 워드클라우드 절차
# 1. Java 실행환경 구축
# 2. 자료 수집( Text 자료 )
#   2.1. Text file 형태로 수집
#   2.2. Web scraping을 이용하여 수집
# 3. 명사 추출
# 4. 추출된 단어(주로 명사)에 대한 빈도수 계산 및 시각화
# 5. word cloud 작성
# 6. 전처리 과정 수행
#  6.1. 불필요한 단어 삭제
#  6.2. 생략된 단어를 사전에 등재

# 1. Java 실행환경 구축
Sys.setenv( JAVA_HOME = "C:/Program Files/Java/jdk1.8.0_231/jre" )

# 필요시 설치
install.packages( "wordcloud" )     # word cloud
install.packages( "wordcloud2" )    # word cloud
install.packages( "KoNLP" )         # 한국어 처리
install.packages( "RColorBrewer" )  # 색상 선택

library( wordcloud )
library( wordcloud2 )
library( KoNLP )
library( RColorBrewer )

library( dplyr )
library( ggplot2 )

# 2. 자료 수집( Text 자료 )
setwd( "D:/workR" )
text <- readLines( "mis_document.txt", encoding = "UTF-8" ) # !!!!!텍스트파일이나 소스파일 등 파일 내에 마지막 줄에 공백줄 추가!!!!!
text

buildDictionary( ext_dic = "woorimalsam" )
pal2 <- brewer.pal( 8, "Dark2" ) # 워드클라우드로 표현 시 색상 팔레트 설정

# 3. 명사 추출
noun <- sapply( text, extractNoun, USE.NAMES = FALSE ) # 파일에서 명사 추출( USE.NAMES = FALSE : 행 이름은 사용하지 않음 )

# 4. 추출된 단어(주로 명사)에 대한 빈도수 계산 및 시각화
noun2 <- unlist( noun )     # list -> vector로 변환
wordcount <- table( noun2 )
sort.noun <- sort( wordcount, decreasing = TRUE )[1:10]
class(sort.noun)
sort.noun <- sort.noun[ -1 ] # 공백 count 된 것은 뺌
barplot( sort.noun, names.arg = names( sort.noun ), col = "steelblue", main  = "빈도수 높은 단어", ylab = "단어 빈도수" )

df <- as.data.frame( sort.noun )
df
ggplot( df, aes( x = df$noun2, y = df$Freq ) ) + 
  geom_bar( stat = "identity", width = 0.7, fill = "steelblue" ) +
  ggtitle( "빈도수 높은 단어" ) +
  theme( plot.title = element_text( size = 25, face = "bold", colour = "steelblue", hjust = 0, vjust = 1 ) ) +
  labs( x = "명사", y = "단어빈도수" ) +
  geom_text( aes( label = df$Freq ), hjust = -0.3 ) + # 빈도 표시
  coord_flip()

# 5. word cloud 작성
pal3 <- brewer.pal( 9, "Blues" )[ 5:9 ] # 워드클라우드로 표현 시 색상 팔레트 설정
wordcloud( names( wordcount ),        # 단어
           freq = wordcount,          # 단어 빈도
           scale = c( 6, 0.7 ),       # 단어 글자크기( 최대, 최소 )
           min.freq = 3,              # 단어 최소 빈도
           random.order = FALSE,      # 단어 출력 위치 (FALSE: 빈도수 높은 단어가 중간에 위치)
           rot.per = .1,              # 90도 회전 단어 비율 (.1 = 10%)
           colors = pal3 )            # 단어 색깔

# 6. 전처리 과정 수행
#  6.2. 생략된 단어를 사전에 등재
buildDictionary( ext_dic = "woorimalsam",
                 user_dic = data.frame( "정치", "ncn" ), # 등재할 단어(ncn: 명사(품사 표시))
                 replace_usr_dic = TRUE )
noun <- sapply( text, extractNoun, USE.NAMES = FALSE )
noun2 <- unlist( noun )

#  6.1. 불필요한 단어 삭제
noun2 <- noun2[ nchar( noun2 ) > 1 ] # 글자수가 2자 이상인 단어만
noun2 <- gsub( "하지", "", noun2 ) # param1문자를 param2로 치환
noun2 <- gsub( "때문", "", noun2 )
wordcount <- table( noun2 )

wordcloud( names( wordcount ),        # 단어
           freq = wordcount,          # 단어 빈도
           scale = c( 6, 0.7 ),       # 단어 글자크기( 최대, 최소 )
           min.freq = 3,              # 단어 최소 빈도
           random.order = FALSE,      # 단어 출력 위치 (FALSE: 빈도수 높은 단어가 중간에 위치)
           rot.per = .1,              # 90도 회전 단어 비율 (.1 = 10%)
           colors = pal3 )            # 단어 색깔


#
# 애국가 형태소 분석
#
library( KoNLP )
useSystemDic()
useSejongDic()
useNIADic()

#
# 애국가 가사 : https://mois.go.kr/frt/sub/a06/b08/nationalIcon_3/screen.do
# 

# 1. 사전 설정
useSejongDic()

# 2. 텍스트 데이터 가져오기
setwd( "D:/workR" )
word_data <- readLines( "애국가(가사).txt" )
word_data

# 3. 명사 추출
word_data2 <- sapply( word_data, extractNoun, USE.NAMES =  FALSE )
word_data2

#   3.1 제대로 추출 되지 않은 단어를 사용자 사전에 등록
add_words <- c( "백두산", "남산", "철갑", "가을", "하늘", "달" )
buildDictionary( user_dic = data.frame( add_words, rep( "ncn", length( add_words ) ) ),
                 replace_usr_dic = TRUE )

get_dictionary( "user_dic" )

word_data2 <- sapply( word_data, extractNoun, USE.NAMES = FALSE )
word_data2

# 4. 행렬을 벡터로 변환
undata <- unlist( word_data2 )
undata

# 5. 사용 빈도 확인
word_table <- table( undata )
word_table

# 6. 필터링 : 두 글자 이상 단어만 선별, 공백이나 한 자리 문자를 걸러냄
undata2 <- undata[ nchar( undata ) >= 2 ]
undata2
word_table2 <- table( undata2 )
word_table2

# 7. 데이터 정렬
sort( word_table2, decreasing = TRUE )

# 애국가 형태소 분석 완료
# 가장 기본적인 전처리만 수행, 100% 정확한 데이터라 볼 수 없음

# 8. word cloud 작성 후 분석
library( wordcloud2 )
wordcloud2( word_table2 )

#   8-1. 배경 및 색상 변경
wordcloud2( word_table2,
            color = "random-light",
            backgroundColor = "black" )

#   8-2. 모양 변경
wordcloud2( word_table2, fontFamily = "맑은고딕", size = 1.2, color = "random-light", backgroundColor = "black", shape = "star" )

#   8-3. 선택 색상 반복
wordcloud2( word_table2, size = 1.6, color = rep_len( c( "red", "blue" ), nrow( word_table2 ) ) )
wordcloud2( demoFreq, size = 1.6, color = rep_len( c( "red", "blue" ), nrow( demoFreq ) ) )

#   8-4. 일정 방향 정렬
wordcloud2( demoFreq,
            minRotation = -pi / 6,
            maxRotation = -pi / 6,
            rotateRatio = 1 )

