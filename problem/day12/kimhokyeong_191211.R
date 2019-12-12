#
# kimhokyeong_191211.R
# 
# 김호경 2019.12.11 / 2019.12.11
#
# 문1)
# 20대 국회 개원 여·야 3당 대표 국회연설문에 대해 각각 워드클라우드를
# 작성하시오.
# 예제소스 파일은 ‘ex_10-1.txt’, ‘ex_10-2.txt’, ‘ex_10-3.txt’이다.
library( wordcloud )
library( KoNLP )
library( RColorBrewer )

useSejongDic()

setwd( "D:/workR/problem/day12" )

text1 <- readLines( "ex_10-1.txt", encoding = "UTF-8" )
text2 <- readLines( "ex_10-2.txt", encoding = "UTF-8" )
text3 <- readLines( "ex_10-3.txt", encoding = "UTF-8" )

add_words <- c( "대한민국", "더불어민주당", "대표", "이명박", "조세개혁", "여러분", "정상화", "중국", "세월호", "백남기", 
                "잠수함발사미사일", "동아시아", "패착", "주도", "분열탄", "융합탄", "비상대책위원회", "생계형", "안심전환대출",
                "블루오션", "4차산업혁명", "인큐베이팅", "사내유보금", "조세불평등", "논리", "논의" )

buildDictionary( ext_dic = "woorimalsam", user_dic = data.frame( add_words, rep( "ncn", length( add_words ) ) ), replace_usr_dic = TRUE )

get_dictionary( "user_dic")

noun1_o <- sapply( text1, extractNoun, USE.NAMES = FALSE )
noun2_o <- sapply( text2, extractNoun, USE.NAMES = FALSE )
noun3_o <- sapply( text3, extractNoun, USE.NAMES = FALSE )

noun1 <- unlist( noun1_o )
noun2 <- unlist( noun2_o )
noun3 <- unlist( noun3_o )

wordcount1 <- table( noun1 )
wordcount2 <- table( noun2 )
wordcount3 <- table( noun3 )

ext.char <- c( "빚", "땀", "돈", "삶", "핵" )

wordcount1 <- wordcount1[ nchar( names( wordcount1 ) ) >= 2 | names( wordcount1 ) %in% ext.char ]
wordcount2 <- wordcount2[ nchar( names( wordcount2 ) ) >= 2 | names( wordcount1 ) %in% ext.char ]
wordcount3 <- wordcount3[ nchar( names( wordcount3 ) ) >= 2 | names( wordcount1 ) %in% ext.char ]

wordcount1 <- sort( wordcount1, decreasing = TRUE )
wordcount2 <- sort( wordcount2, decreasing = TRUE )
wordcount3 <- sort( wordcount3, decreasing = TRUE )

pal <- brewer.pal( 8, "Dark2" )

wordcloud( names( wordcount1 ), 
           freq = wordcount1, 
           scale = c( 10, 0.7 ),
           random.order = FALSE,
           rot.per = .1,
           colors = pal )

wordcloud( names( wordcount2 ), 
           freq = wordcount2, 
           scale = c( 10, 0.7 ),
           random.order = FALSE,
           rot.per = .1,
           colors = pal )

wordcloud( names( wordcount3 ), 
           freq = wordcount3, 
           scale = c( 10, 0.7 ),
           random.order = FALSE,
           rot.per = .1,
           colors = pal )

# 문2)
# 스티브 잡스의 스탠포드 대학 졸업식 연설문에 대해 워드클라우드를 작성
# 하시오.
# Tip. 예제소스 파일은 ‘ex_10-4.txt’이다.
text <- readLines( "ex_10-4.txt", encoding = "UTF-8" )
noun <- sapply( text, extractNoun, USE.NAMES = FALSE )
noun <- unlist( noun )
wordcount <- table( noun )

wordcount <- wordcount[ nchar( names( wordcount ) ) >= 2 ]

wordcount <- sort( wordcount, decreasing = TRUE )

wordcloud( names( wordcount ),
           freq = wordcount,
           scale = c( 6, 0.7 ),
           random.order = FALSE,
           rot.per = .1,
           colors = pal )

# 문3) 
# 오바마 대통령의 데통령 당선 연설문에 대해 워드클라우드를 작성하시오
# Tip. 예제소스 파일은 ‘ex_10-5.txt’이다.
text <- readLines( "ex_10-5.txt", encoding = "UTF-8" )

noun <- sapply( text, extractNoun, USE.NAMES = FALSE )
noun <- unlist( noun )

wordcount <- table( noun )
wordcount <- sort( wordcount, decreasing = TRUE )

wordcount <- wordcount[ nchar( names( wordcount ) ) >= 2 ]

wordcloud( names( wordcount ),
           freq = wordcount,
           scale = c( 6, 0.7 ),
           random.order = FALSE,
           rot.per = .1,
           colors = pal )
