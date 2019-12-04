#
# day6_answer.R
#
# 6일차 다변량(다중변수) 연속형 data 분석 실습
#
#문1)
#다음은 직장인 10명의 수입과 교육받은 기간을 조사한 자료이다. 산점도와 상관계수를 구하고, 
#수입과 교육기간 사이에 어떤 상관관계가 있는지 설명하시오.
#
#수입 	 121 99 41 35 40 29 35 24 50 60
#교육기간 19  20 16 16 18 12 14 12 16 17
income <- c( 121, 99, 41, 35, 40, 29, 35, 24, 50, 60 )
period <- c( 19, 20, 16, 16, 18, 12, 14, 12, 16, 17 )

tbl <- data.frame( income, period )
tbl 

plot( income~period, data = tbl )
res <- lm( income~period, data = tbl )
abline( res )

cor( income, period )

  # 상관계수가 0.7292108으로 비교적 강한 양의 상관관계를 가진다.
  # 교육받은 기간이 길수록 수입이 증가

#문2)
#다음은 대학생 10명의 성적과 주당 TV 시청시간을 조사한 자료이다. 산점도와 상관계수를 구하고, 
#성적과 TV 시청시간 사이에 어떤 상관관계가 있는지 설명하시오.
#
#성적 	 77.5 60 50 95 55 85 72.5 80 92.5 87.5
#시청시간 14   10 20  7 25  9 15   13  4   21
score <- c( 77.5, 60, 50, 95, 55, 85, 72.5, 80, 92.5, 87.5 )
tv <- c( 14, 10, 20, 7, 25, 9, 15, 13, 4, 21 )

tbl <- data.frame( score, tv )
tbl 

plot( score~tv, data = tbl )
res <- lm( score~tv, data = tbl )
abline( res )

cor( score, tv )

  # 상관계수가 -0.6283671로 비교적 강한 음의 상관관계를 가진다.
  # TV 시청 시간이 증가할수록 성적은 감소한다.

#문3) 
#R에서 제공하는 mtcars 데이터셋에서 mpg와 다른 변수들 간의 상관계수를
#구하시오. 어느 변수가 mpg와 가장 상관성이 높은지 산점도와 함께 설명하시오.			
head( mtcars )

cor( mtcars )
cor( mtcars )[ 1, ]

  # disp와의 상관계수가 -0.8475514로 가장 상관성이 높다.  

#문4)
#다음은 2015년부터 2026년도까지의 예상 인구수 추계자료이다. 연도를 x
#축으로 하여 선그래프를 작성하시오.
#
#연도		총인구 (천명)		연도		총인구 (천명)
#2015		51014				2021		52123
#2016		51245				2022		52261
#2017		51446				2023		52388
#2018		51635				2024		52504
#2019		51811				2025		52609
#2020		51973				2026		52704
year <- 2015:2026
pop <- c( 51014, 51245, 51446, 51635, 51811, 51973, 52123, 52261, 52388, 52504, 52609, 52704 )
plot( year, pop, type = 'l', main = '인구수 추계' )

#문5)
#R에서 제공하는 trees 데이터셋에 대해 다음 문제를 해결하기 위한 R 코
#드를 작성하시오.
#
#(1) 나무의 지름(Girth)과 높이(Height)에 대해 산점도와 상관계수를 보이시오.
class( trees )
dim( trees )
str( trees )
head( trees )
tail( trees )

tbl <- data.frame( trees$Girth, trees$Height )
tbl

plot( trees$Girth, trees$Height )

plot( trees.Girth~trees.Height, data = tbl )
res <- lm( trees.Girth~trees.Height, data = tbl )
abline( res )

cor( trees$Girth, trees$Height )

#(2) trees 데이터셋에 존재하는 3개 변수 간의 산점도와 상관계수를 보이시오.
plot( trees )
pairs( trees )
cor( trees )
