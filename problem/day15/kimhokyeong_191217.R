# 
# kimhokyeong_191217.R
# 김호경 2019.12.17 / 2019.12.17
#
# 문1)
# trees 데이터셋에 대해 다음의 문제를 해결하는 R 코드를 작성하시오.
# 
# (1) 나무 둘레(Girth)와 나무의 키(Height)로 나무의 볼륨을 예측하는 다중선형 회귀
# 모델을 만드시오.
str( trees )
model <- lm( Volume~Girth + Height, data = trees )

# (2) 다중선형 회귀모델을 이용하여 trees 데이터셋의 나무 둘레(Girth)와 나무의 키
# (Height)로 나무의 볼륨을 예측하시오.
pred <- predict( model, trees )

# (3) (2)에서 예측한 볼륨과 실제 trees 데이터셋의 볼륨(Volume)이 얼마나 차이가
# 나는지 보이시오. (예측값, 실제값, 예측값-실제값을 나타낸다.)
df <- data.frame( pred, trees[ , 3 ], as.vector( residuals( model ) ) )
colnames( df ) <- c( "예측값", "실제값", "오차" )
df

# 
# 문2)
# mtcars 데이터셋에서 다른 변수들을 이용하여 연비(mpg)를 예측하는 다중 회귀모델을 만드시오.
#
# (1) 전체 변수를 이용하여 연비(mpg)를 예측하는 회귀모델을 만들고 회귀식을 나타
# 내시오.
model <- lm( mpg~., data = mtcars )

# 회귀식 :
# mpg = (12.30337) + (-0.11144 * mtcars$cyl) + (0.01334 * mtcars$disp) + (-0.02148 * mtcars$hp) + (0.78711 * mtcars$drat) + (-3.71530 * mtcars$wt)
#        + (0.82104 * mtcars$qsec) + (0.31776 * mtcars$vs) + (2.52023 * mtcars$am) + (0.65541 * mtcars$gear) +(-0.19942 * mtcars$carb)

# (2) 연비(mpg)를 예측하는 데 도움이 되는 변수들만 사용하여 예측하는 회귀모델을
# 만들고 회귀식을 나타내시오.
summary( model )

model2 <- stepAIC( model )
summary( model2 )

model3 <- lm( mpg~wt + qsec + am, data = mtcars )
summary( model3 )
# 회귀식 :
# mpg = (9.618) + (-3.917 * mtcars$wt) + (1.226 * mtcars$qsec) + (2.936 * mtcars$am)

# (3) (1), (2)에서 만든 예측모델의 설명력(Adjusted R-squared)을 비교하시오.
# (1): 0.8066, (2):  0.8336
# 
# 문3) 
# UCLA 대학원의 입학 데이터를 불러와서 mydata에 저장한 후 다음 물음에 답하시오.
# 
mydata <- read.csv( "https://stats.idre.ucla.edu/stat/data/binary.csv" )
str( mydata )
head( mydata )

# (1) gre, gpa, rank를 이용해 합격 여부(admit)를 예측하는 로지스틱 모델을 만드시오(0: 불합격, 1:합격).
model <- glm( admit~., data = mydata )

# (2) mydata에서 합격 여부(admit)를 제외한 데이터를 예측 대상 데이터로 하여 (1)에서 만든 모델에 입력하여 
# 합격 여부를 예측하고 실제값과 예측값을 나타내시오.
pred <- predict( model, mydata[ , -1 ] )
pred <- round( pred, 0 )

df <- data.frame( pred, mydata$admit )
colnames( df ) <- c( "에측값", "실제값" )
df

# 
# (3) 만들어진 모델의 예측 정확도를 나타내시오.
answer <- mydata$admit
answer == pred

acc <- mean( answer == pred )
acc
