library(tseries)
library(EnvStats)
library(forecast)
library(stats)


#load data
data1<-Inflow
head(Inflow)
data.ts<-ts(Inflow, start= c(2003,1), end=c(2014, 12), frequency = 12)
Inflow=Inflow
library(timeSeries)
data.ts=as.ts(data.ts)

str(data.ts)

plot.ts(data.ts)


#uji statsioneritas Terhadap Varians-> Box Cox 
stas.Var<-BoxCox.lambda(Inflow)
stas.var  

#uji Stasioneritas Terhadap Mean-> ADF Test
library(tseries)
adf.test(data.ts)


#Differencing
Dataarima.dif<-diff(data.ts)
adf.test(Dataarima.dif)

#Plot ACF dan PACF
acf(Dataarima.dif)
pacf(Dataarima.dif)

#ARIMA
fit=arima(Dataarima.dif,c(1,1,0))
fit

#White Noise
Box.test(fit$residuals,type = "Ljung-Box")

#uji Normality
residual=resid(fit)
residual
ks.test(residual,"pnorm",mean(residual),sd(residual))

#ARIMA
fit=arima(Dataarima.dif,c(2,1,0))
fit

#White Noise
Box.test(fit$residuals,type = "Ljung-Box")

#Uji Normalitas
residual=resid(fit)
residual
ks.test(residual,"pnorm",mean(residual),sd(residual))

#ARIMA
fit=arima(Dataarima.dif,c(3,1,0))
fit

#White Noise
Box.test(fit$residuals,type = "Ljung-Box")

#Uji Normalitas
residual=resid(fit)
residual
ks.test(residual,"pnorm",mean(residual),sd(residual))

#ARIMA
fit=arima(Dataarima.dif,c(5,1,0))
fit

#White Noise
Box.test(fit$residuals,type = "Ljung-Box")

#Uji Normalitas
residual=resid(fit)
residual
ks.test(residual,"pnorm",mean(residual),sd(residual))

#ARIMA
fit=arima(Dataarima.dif,c(5,1,0))
fit

#White Noise
Box.test(fit$residuals,type = "Ljung-Box")

#Uji Normalitas
residual=resid(fit)
residual
ks.test(residual,"pnorm",mean(residual),sd(residual))

#ARIMA
fit=arima(Dataarima.dif,c(11,1,0))
fit

#White Noise
Box.test(fit$residuals,type = "Ljung-Box")

#Uji Normalitas
residual=resid(fit)
residual
ks.test(residual,"pnorm",mean(residual),sd(residual))

#ARIMA
fit=arima(Dataarima.dif,c(1,1,11))
fit

#White Noise
Box.test(fit$residuals,type = "Ljung-Box")

#Uji Normalitas
residual=resid(fit)
residual
ks.test(residual,"pnorm",mean(residual),sd(residual))

#ARIMA
fit=arima(Dataarima.dif,c(2,1,11))
fit

#White Noise
Box.test(fit$residuals,type = "Ljung-Box")

#Uji Normalitas
residual=resid(fit)
residual
ks.test(residual,"pnorm",mean(residual),sd(residual))

#ARIMA
fit=arima(Dataarima.dif,c(3,1,11))
fit

#White Noise
Box.test(fit$residuals,type = "Ljung-Box")

#Uji Normalitas
residual=resid(fit)
residual
ks.test(residual,"pnorm",mean(residual),sd(residual))

#ARIMA
fit=arima(Dataarima.dif,c(11,1,11))
fit

#White Noise
Box.test(fit$residuals,type = "Ljung-Box")

#Uji Normalitas
residual=resid(fit)
residual
ks.test(residual,"pnorm",mean(residual),sd(residual))

#ARIMA
fit=arima(Dataarima.dif,c(1,1,11))
fit

#White Noise
Box.test(fit$residuals,type = "Ljung-Box")

#Uji Normalitas
residual=resid(fit)
residual
ks.test(residual,"pnorm",mean(residual),sd(residual))


#ARIMA
fit=arima(Dataarima.dif,c(3,1,11))
fit

#White Noise
Box.test(fit$residuals,type = "Ljung-Box")

#Uji Normalitas
residual=resid(fit)
residual
ks.test(residual,"pnorm",mean(residual),sd(residual))


#ARIMA
fit=arima(Dataarima.dif,c(5,1,11))
fit

#White Noise
Box.test(fit$residuals,type = "Ljung-Box")

#Uji Normalitas
residual=resid(fit)
residual
ks.test(residual,"pnorm",mean(residual),sd(residual))

#ARIMA
fit=arima(Dataarima.dif,c(11,1,11))
fit

#White Noise
Box.test(fit$residuals,type = "Ljung-Box")

#Uji Normalitas
residual=resid(fit)
residual
ks.test(residual,"pnorm",mean(residual),sd(residual))


#ARIMA
fit=arima(Dataarima.dif,c(1,1,12))
fit

#White Noise
Box.test(fit$residuals,type = "Ljung-Box")

#Uji Normalitas
residual=resid(fit)
residual
ks.test(residual,"pnorm",mean(residual),sd(residual))


#ARIMA
fit=arima(Dataarima.dif,c(3,1,12))
fit

#White Noise
Box.test(fit$residuals,type = "Ljung-Box")

#Uji Normalitas
residual=resid(fit)
residual
ks.test(residual,"pnorm",mean(residual),sd(residual))


#ARIMA
fit=arima(Dataarima.dif,c(5,1,12))
fit

#White Noise
Box.test(fit$residuals,type = "Ljung-Box")

#Uji Normalitas
residual=resid(fit)
residual
ks.test(residual,"pnorm",mean(residual),sd(residual))


#ARIMA
fit=arima(Dataarima.dif,c(11,1,12))
fit

#White Noise
Box.test(fit$residuals,type = "Ljung-Box")

#Uji Normalitas
residual=resid(fit)
residual
ks.test(residual,"pnorm",mean(residual),sd(residual))


#ARIMA
fit=arima(Dataarima.dif,c(1,1,13))
fit

#White Noise
Box.test(fit$residuals,type = "Ljung-Box")

#Uji Normalitas
residual=resid(fit)
residual
ks.test(residual,"pnorm",mean(residual),sd(residual))


#ARIMA
fit=arima(Dataarima.dif,c(2,1,13))
fit

#White Noise
Box.test(fit$residuals,type = "Ljung-Box")

#Uji Normalitas
residual=resid(fit)
residual
ks.test(residual,"pnorm",mean(residual),sd(residual))


#ARIMA
fit=arima(Dataarima.dif,c(3,1,13))
fit

#White Noise
Box.test(fit$residuals,type = "Ljung-Box")

#Uji Normalitas
residual=resid(fit)
residual
ks.test(residual,"pnorm",mean(residual),sd(residual))


#ARIMA
fit=arima(Dataarima.dif,c(5,1,13))
fit

#White Noise
Box.test(fit$residuals,type = "Ljung-Box")

#Uji Normalitas
residual=resid(fit)
residual
ks.test(residual,"pnorm",mean(residual),sd(residual))


#ARIMA
fit=arima(Dataarima.dif,c(11,1,13))
fit

#White Noise
Box.test(fit$residuals,type = "Ljung-Box")

#Uji Normalitas
residual=resid(fit)
residual
ks.test(residual,"pnorm",mean(residual),sd(residual))

#Forecasting/Peramalan
fcast=forecast(fit,h=12)
fcast
plot(fcast)
