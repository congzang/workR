#
# 13일차
#
# 단순선형 회귀분석( Simple linear regression analysis )
# 
# Modeling : 현실 세계에서 일어나는 현상을 수학식으로 표한하는 행위
#            데이터과학에서 Modeling이란 수집한 data를 이용하여 최적의 모델을 찾아내는 과정
#                                         (훈련 data)
# 
# 데이터 과학에서
#   독립변수 x -> 설명변수(explanatory variable), 특징(feature)
#   종속변수 y -> 반응변수(response variable), 레이블(label)
#   x가 입력되면 y를 맞추어야 하는 문제, y를 ground truth로 간주
#
#
# << 최적의 모델을 찾는 과정>>
#   모델 : y = wx + b   -> w, b를 계수(매개변수)
# 
#     1. 모델 선택 - 선형 방정식 선택
#     2. 주어진 data(훈련 data)를 적용하여 매개변수 결정
#         >> lm()
#     3. 예측은 훈련 data에 없는 새로운 data로 모델이 레이블을 추정하는 과정
#         >> predict()
#     4. 완성된 모델에 대한 품질 평가 (모델이 예측을 정확하게 하는가?)
#         >> summary()의 결과를 이해(p값, 회귀선 기준으로 데이터 퍼져있는 정도 판단 가능)
#
# * cars
# Coefficients:
#             Estimate Std.     Error   t value   Pr(>|t|)    
# (Intercept)     -17.5791     6.7584    -2.601     0.0123 *   ( <- 0.0123(>0.05) : 회귀선을 기준으로 데이터가 퍼져있다.(오차날 가능성이 큼) )
#   speed           3.9324     0.4155     9.464   1.49e-12 *** ( <- 1.49e-12(p값) : 통계적으로 유의미하다.)
#
#
#
#
# 회귀분석(Regression Analysis)
#   - 관찰된 연속형 변수들에 대해 두 변수 사이의 모형을 구한뒤 적합도를 측정해 내는 분석방법
#   - 시간에 따라 변화하는 데이터나 어떤 영향, 가설적 실험, 인과 관계의 모델링 등의 통계적 예측에 이용될 수 있다.
# 
# 단순 선형 회귀분석(Simple linear regression analysis)
#   - 독립변수와 종속변수와의 관계가 선형으로 표현
#   - 하나의 독립변수를 다루는 분석방법
#
# 단순선형 회귀모델의 회귀식 : y = wx + b( w,b : 상수 )
#   - w, b는 어떻게 찾을 수 있을까?
#   - x, y로 구성된 data를 이용하여 w, b를 찾아내는 모델
#
# 주행거리와 제동거리 사이의 회귀모델
str( cars )
head( cars )

# 산점도를 통한 선형 관계 확인
plot( dist~speed, data = cars )
plot( cars )

# 회귀모델 구하기
# 종속(반응)변수~독립(설명)변수 순서로 지정
model <- lm( dist~speed, cars )
model
# * lm 결과 :
#       Coefficients: ( = 매개변수(계수) )
#       (Intercept)        speed  
#         (= b값)         (= w값)
#           -17.579        3.932
#
# 회귀선
abline( model )

coef( model ) # 매개변수(계수) - w, b값 출력
cars
fitted( model )     # 훈련 data에 있는 샘플에 대한 예측값
residuals( model )  # 잔차 : 예측값(fitted)과 실제값(cars)과의 차이 ( 회귀식으로 추정된 값과의 차이 )
# 잔차 제곱합을 평균제곱오차( MES - Mean Squared error )로 변환
deviance( model ) / length( cars$speed ) # deviance() : 잔차의 제곱합

b <- coef( model )[ 1 ] # b값
w <- coef( model )[ 2 ] # w값

speed <- 21.5 # 현재 cars에 없는 데이터로 예측
dist <- w * speed + b
dist


df <- data.frame( speed = c( 4.0, 7.0, 21.5, 25.0, 25.5, 26.0, 26.5, 27.0, 27.5, 28.0 ) )
predict( model, df ) # 예측 수행 함수 ( = dist <- w * speed + b )
plot( df$speed, predict( model, df ), col = "red", cex = 2, pch = 20 )
abline( model )

speed <- cars[ , 1 ]
pred <- w * speed + b # = fitted()
pred

compare <- data.frame( pred, cars [ , 2 ], pred-cars[ , 2 ] ) # (pred-cars[ , 2 ] = residuals())
compare
colnames( compare ) <- c( "예상(fitted)", "실제(cars data)", "오차(residuals)" )

head( fitted( model ), 3 )    # 예측
head( residuals( model ), 3 ) # 추정된 값과의 차이
head( compare, 3 )

summary( model )
#
# 
# Call:
#   lm(formula = dist ~ speed, data = cars)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -29.069  -9.525  -2.272   9.215  43.201 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -17.5791     6.7584  -2.601   0.0123 *  
#   speed         3.9324     0.4155   9.464 1.49e-12 *** ( -> '*'의 개수가 많을수록 speed가 dist에 영향을 많이 미친다 라고 할 수 있음)
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 15.38 on 48 degrees of freedom
# Multiple R-squared:  0.6511,	Adjusted R-squared:  0.6438 
# F-statistic: 89.57 on 1 and 48 DF,  p-value: 1.49e-12

# 평균은 클수록, 분산은 작을수록, 데이터 크기가 클수록 믿음이 커진다. -> t-통계량(t-statistics) / t-값(t-value)
# t-값이 크면 대립가설에 대한 믿음이 강해진다.
# t-값이 작으면 대립가설에 대한 믿음이 약해진다.
# 데이터를 통해 '대립가설이 통계학적으로 유의미하다.'라는 것을 증명하고 확인하는 작업을 t-검정(t-test)라 한다.
#
# '귀무가설이 참이라고 가정했을 때, 표본으로부터 얻어지는 통계치가 나타날(관측될) 확률'을 계산하는데 이 때 계산된 확률값을 p값이라 한다.
# p값이 매우 낮으면(0.05 이하), 이러한 표본 통계값은 우연히 나타나기 어려운 케이스이기 때문에 
# 우리는 귀무가설을 채택하지 않고(기각하고), 대안적인 가설인 가설, 즉 대립가설을 채택한다. (= p값이 높으면 귀무가설, p값이 낮으면 대립가설 채택)

str( cars )
head( cars )
car_model <- lm( dist~speed, data = cars )
coef( car_model )
plot( cars )
abline( car_model, col = "red" )
summary( car_model ) # p-value = 1.091e-14이고, 0.05보다 작으므로 유의미하다.

str( women )
head( women )
women_model <- lm( weight~height, data = women )
coef( women_model )
plot( women_model )
abline( women_model, col = "red" )
summary( women_model ) # p-value = 1.091e-14이고, 0.05보다 작으므로 유의미하다. ( = height가 커지면 weight도 커진다. )
# 
# * cars
# Coefficients:
#             Estimate Std.     Error   t value   Pr(>|t|)    
# (Intercept)     -17.5791     6.7584    -2.601     0.0123 *   ( <- 0.0123(>0.05) : 회귀선을 기준으로 데이터가 퍼져있다.(오차날 가능성이 큼) )
#   speed           3.9324     0.4155     9.464   1.49e-12 *** ( <- 1.49e-12 : 통계적으로 유의미하다.)
#   
#   * women
# Coefficients:
#             Estimate Std.     Error   t value   Pr(>|t|)    
# (Intercept)    -87.51667    5.93694    -14.74   1.71e-09 *** ( <- 1.71e-09(<0.05) : 회귀선에 데이터가 모여있다.(오차날 가능성이 작음) )
#   height         3.45000    0.09114     37.85   1.09e-14 *** ( <- 1.49e-12 : 통계적으로 유의미하다.)