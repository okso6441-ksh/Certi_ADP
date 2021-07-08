library(TTR); library(forecast)
# 0. 데이터 로드 & 시계열 객체 
write.csv(kings, 'C:/Users/okso6/temp/ADP/king.csv')
kings_ts <- ts(kings); kings_ts
plot.ts(kings_ts)

# 이동평균 MA; n년마다 평균
kings_sma3 <- SMA(kings_ts, n = 3)
kings_sma8 <- SMA(kings_ts, n = 8)
kings_sma12 <- SMA(kings_ts, n = 12)

par(mfrow = c(2,2))
plot.ts(kings_ts); plot.ts(kings_sma3)
plot.ts(kings_sma8); plot.ts(kings_sma12)

# 차분을 통해 데이터 정상화
kings_diff1 <- diff(kings_ts, differences = 1)
kings_diff2 <- diff(kings_ts, differences = 2)
kings_diff3 <- diff(kings_ts, differences = 3)

plot.ts(kings_ts)
plot.ts(kings_diff1)# 1차 차분만 해도 어느정도 정상화 패턴을 보임
plot.ts(kings_diff2)
plot.ts(kings_diff3)

mean(kings_diff1); sd(kings_diff1)

# 1차 차분해야 정상성 가짐 
auto.arima(kings)   # --> ARIMA(0,1,1)

par(mfrow = c(2,1))
# 1차 차분한 데이터로 ARIMA 모형 확인
acf(kings_diff1, lag.max = 20)      # lag 2부터 점선 안에 존재. lag 절단값 = 2. --> MA(1)
pacf(kings_diff1, lag.max = 20)     # lag 4에서 절단값 --> AR(3)

# 예측
kings_arima <- arima(kings_ts, order = c(3,1,1))# 차분통해 확인한 값 적용
kings_fcast <- forecast(kings_arima, h=5)#forecast.ARIMA(kings_arima, h = 5)
kings_fcast
plot.forecast(kings_fcast)

# 15. ------------------------------------------

# 0. 시계열 객체 형태 ts()
sales <- c(18,33,41,7,34,35,24,25,24,21,25,20,
           22,31,40,29,25,21,22,54,31,25,26,35)
tsales <- ts(sales, start=c(2003, 1), frequency = 12)
tsales # 시계열 자료: 시작, 끝, 주기 
plot(tsales)
start(tsales); end(tsales); frequency(tsales)

# 시계열 객체의 하위셋 
tsales.subset <- window(tsales, start=c(2003,5), end=c(2004,6))
tsales.subset

# 단순 이동 평균 평활(smoothing) ma
library(forecast)
opar <- par(no.readonly = T); par(mfrow=c(2,2))
ylim <- c(min(Nile), max(Nile))
plot(Nile, main="Raw ts")
plot(ma(Nile, 3), main="k3", ylim=ylim)
plot(ma(Nile, 7), main="k7", ylim=ylim)
plot(ma(Nile, 15), main="k15", ylim=ylim)
par(opar)

# stl: loess 평활방법 
plot(AirPassengers) # 승법모델 y=추세*계절*불규칙
lAirPassengers <- log(AirPassengers) # 가법모델로 y=++ 
plot(lAirPassengers, ylab="log(AirPassengers)")

fit <- stl(lAirPassengers, s.window = "period")
plot(fit)
exp(fit$time.series)

par(mfrow=c(2,1)) # 계절분해 시각화 
monthplot(AirPassengers, ) 
seasonplot(AirPassengers, year.labels = "TRUE")

# 지수예측모델 
fit <- ets(nhtemp, model="ANN")
fit # alpha: 가중치 감소율
forecast(fit, 1) # 1step ahead
plot(forecast(fit, 1))

plot(forecast(fit, 1), xlab="Year"
     , ylab=expression(paste("Temp (", degree*F, ")",))
)

accuracy(fit)

fit<-ets(log(AirPassengers), model="AAA")
accuracy(fit)
pred <- forecast(fit, 5)
pred$mean <- exp(pred$mean)
pred$lower <- exp(pred$lower)
pred$upper <- exp(pred$upper)
p <- cbind(pred$mean, pred$lower, pred$upper)
dimnames(p)[[2]] <- c("mean", "Lo80", "Lo95", "Hi80", "Hi95")

fit <- ets(JohnsonJohnson) # 자동

library(tseries) # ARIMA
# 1. 시계열 변환 
plot(Nile)
ndiffs(Nile) # d=1
dNile <- diff(Nile) 
plot(dNile) # 보다 정상적
adf.test(dNile) # 정상적
# 2. 가능한 모델 선택 p, d=1, q 
Acf(dNile) # q=점선 밖(pq순서주의)
Pacf(dNile) # p=절단점 - 1 
# 3. 모델 적합(파라미터 적용)
fit<-arima(Nile, order = c(0,1,1)) # dNile 아님
# 결과 Coefficients: 이동평균계수, AIC와 함께 제공 
accuracy(fit)
# 4. 모델 적합성 평가
# 적합: 잔차 평균 0 & 정규분포 & 자동상관성 0
qqnorm(fit$residuals)
qqline(fit$residuals)
Box.test(fit$residuals, type="Ljung-Box")
# (적합시) 5. 예측 
plot(forecast(fit, 3)) # 청색점: 시점 추정, 회색: 80/95%신뢰구간 

# 자동 ARIMA
# 가능한 모델 중 AIC 최소화 
fit<-auto.arima(sunspots)
forecast(fit, 3)
accuracy(fit)