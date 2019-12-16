# 
# kimhokyeong_191216.R
# 김호경 2019.12.16 / 2019.12.16
# 
# 문1)
# state.x77 데이터셋에서 문맹률(Illiteracy)을 이용해 범죄율(Murder)을 예측
# 하는 단순선형 회귀모델을 만드시오. 그리고 문맹률이 0.5, 1.0, 1.5일 때 범
# 죄율을 예측하여 보시오.
# 
st <- data.frame( state.x77 )
str( st )
model <- lm( Murder~Illiteracy, st )

df <- data.frame( Illiteracy = c( 0.5, 1.0, 1.5 ) )
pred <- predict( model, df )

plot( df$Illiteracy, pred, col = "red", cex = 2, pch = 20 )
abline( model )

# 문2)
# trees 데이터셋에서 나무둘레(Girth)로 나무의 볼륨(Volume)을 예측하는 단
# 선형 회귀모델을 만드시오. 그리고 나무 둘레가 8.5, 9.0, 9.5일 때, 나무의
# 볼륨(Volume)을 예측하여 보시오.
str( trees )
model <- lm( Volume~Girth, trees )
df <- data.frame( Girth = c( 8.5, 9.0, 9.5 ) )
pred <- predict( model, df )

plot( df$Girth, pred, col = "red", cex = 2, pch = 20 )
abline( model )

# 
# 문3) 
# pressure 데이터셋에서 온도(temperature)로 기압(pressure)을 예측하는 단
# 순선형 회귀모델을 만드시오. 그리고 온도가 65, 95, 155일 때 기압을 예측
# 하여 보시오.
str( pressure )
model <- lm( pressure~temperature, pressure )
df <- data.frame( temperature = c( 65, 95, 155 ) )
pred <- predict( model, df )

plot( df$temperature, pred, col = "red", cex = 2, pch = 20 )
abline( model )
