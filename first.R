#
# R 프로그래밍 1일차
#
# first.R
#
# 작성자 : 김호경
#
# 최초작성일 : 2019. 11. 26.
#
print( "Hello, World!!!" )

number <- 10
number2 = 100

number3 <- number

numberValue <- 1            # camel 표기법
str_value <- "R language"   # snake 표기법
booleanvalue <- TRUE

numberValue <- "R script"
numberValue <- 1

print( numberValue )
print( str_value )
print( booleanvalue )
print( number )

cat( "Numeric number : ", numberValue, "\n" )
cat( "String : ", str_value, "\n" )
cat( "Boolean value : ", booleanvalue, "\n" )

numberValue <- scan()
cat( "Numeric number : ", numberValue, "\n" )


########## 산술연산 #########
number1 <- 10
number2 <- 20
resultAdd <- number1 + number2
resultSub <- number1 - number2
resultMul <- number1 * number2
resultDiv <- number1 / number2
resultRem <- number2 %% number1
resultSec <- number2 ** 5

print( resultAdd )
print( resultSub )
print( resultMul )
print( resultDiv )
print( resultRem )
print( resultSec )

number1 <- 0
number1 <- number1 + 10
number1
number1 <- number1 + 10
number1
number1 <- number1 + 10
number1
number2 <- 100
number1 <- number2 + 10
number1
number2

print( ( number1 + 10 ) * number2 / 20 )

number300 <- 10
number300 <- number300 + 10
number300

number301 <- number300 * 2
number301

######### 관계연산 #########
number1 <- 10.5
number2 <- 10
print( number1 > number2 )
print( number1 >= number2 )
print( number1 < number2 )
print( number1 <= number2 )
print( number1 == number2 )
print( number1 != number2 )

######### 논리연산 #########
print( number1 > 10 & number2 > 10 )  # AND
print( number1 > 10 | number2 > 10 )  # OR
print( !( number1 > 10 ) )            # NOT

number <- "100"
str <- "R language"
result = number + str
print( result )

#
######### 제어구조 - 선택구조 #########
#

# 1. 양자택일 구조
job.type <- 'A'

if( job.type  == 'B' ) {
  bonus <- 200  # 참일 경우
} else {
  bonus <- 100  # 거짓일 경우
}
cat ("job type : ", job.type, "\t\tbonus : ", bonus )


# 2. 단순선택 구조
job.type <- 'B'

if( job.type == 'A' ) {
  bonus <- 200
}

cat ("job type : ", job.type, "\t\tbonus : ", bonus )

# 3. 다중선택 구조
score <- 85

if( score >= 90 ) {
  grade <- 'A'
} else if( score >= 80 ) {
  grade <- 'B'
} else if( score >= 70 ) {
  grade <- 'C'
} else if( score >= 60 ) {
  grade <- 'D'
} else {
  grade <- 'F'
}

cat ("score : ", score, "\t\tgrade : ", grade )

######## 문제 - 짝/홀수 구분 START ########
number <- 15
remainder <- number %% 2

if( remainder == 0 ) {
  str <- "짝수"
} else {
  str <- "홀수"
}

cat( "Number : ", number, "는 ", str, "이다." )
######## 문제 - 짝/홀수 구분 END ########


a <- 5
b <- 20

if( a > 5  & b > 5 ) {
  cat(a, " > 5 and ", b," > 5")
} else {
  cat(a, " <= 5 or ", b," <= 5")
}

a <- 10
b <- 20

if( a > b ) {
  c <- a
} else {
  c <- b
}
cat("a = ", a, "\tb = ", b, "\tc = ", c)

c <- ifelse( a > b, a, b )
cat("a = ", a, "\tb = ", b, "\tc = ", c)


######## 문제 - 최대값 START ########
a <- 0
b <- -10
c <- -80
maxNum <- ifelse( a > b, a, b )
maxNum <- ifelse( c > maxNum, c, maxNum )

cat("최대값 : ", maxNum)
######## 문제 - 최대값 END ########


######## 반복구조 ########
# 1. for문
for( i in 1:10 ) {
  print( "*" )
}

multiple <- 2
for( i in 2:9 ) {
  cat( multiple, 'X', i, '=', multiple * i, "\n" )
}

# 2. while문
i <- 1              # i = 반복제어변수(while문에서는 필수)
while( i <= 10 ) {  # 조건이 참인 경우에만 수행
  print( i )
  i <- i + 1        # i값 변화
}

multiple <- 2
i <- 2
while( i <= 9 ) {
  cat( multiple, 'X', i, '=', multiple * i, "\n" )
  i <- i + 1
}


######## 문제 - 숫자 출력 START ########
# 방법1. 줄바꾸기 위한 별도의 변수 생성
newlineCnt <- 1             # 초기화

for( i in 1:100 ) {
  cat( i, "\t" )
  newlineCnt <- newlineCnt + 1
  
  if( newlineCnt > 10 ) {
    cat("\n")
    newlineCnt <- 1         # Reset
  }
}

# 방법2. i값이 10으로 나누어지는 값으로 10개를 구분
for( i in 1:100 ) {
  cat( i, "\t" )
  
  if( (i %% 10) == 0 ) {
    cat("\n")
  }
}
######## 문제 - 숫자 출력 END ########
numCnt <- 0
for( i in 1:1000 ) {
  
  # 3의 배수이거나 5의 배수인 경우
  if( i %% 3 == 0 | i %% 5 == 0 ) {
    cat( i, " \t" )
    numCnt <- numCnt + 1
    
    # 1줄에 10개씩 출력
    if( numCnt %% 10 == 0 ) {
      cat( "\n" )
    }
  }
}

cat("3의 배수이거나 5의 배수의 숫자갯수 : ", numCnt)