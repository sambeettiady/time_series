#setwd("E:/PGDBA 2017-19/IIM C Third Semester/Selected Aspects of Predictive Modelling/Project")
library(timeDate)
library(timeSeries)
library(fBasics)
library(urca)
library(lmtest)
library(sandwich)


rm(list = ls())
# Reading Data
da=read.csv("Stock Market_Economic Data.csv",header=T)
head(da)
attach(da)
dim(da)
names(da)


## creating yearly time series objects starting from 1960 
Turnover_ts=ts(Turnover[1:66],frequency=12,start=c(2012,4))

Capitalization_ts = ts(Market.Capitalization[1:66],frequency=12,start=c(2012,4))

TurnoverRatio_ts = ts(Turnover.Ratio[1:66],frequency=12,start=c(2012,4))

WPI_ts=ts(WPI[1:66],frequency=12,start=c(2012,4))

Inflation_ts=ts(Inflation[1:66],frequency=12,start=c(2012,4))

FDI_ts=ts(FDI[1:66],frequency=12,start=c(2012,4))

Nifty_ts  = ts(Nifty[1:66],frequency=12,start=c(2012,4))


IIP_ts=ts(IIP[1:66],frequency=12,start=c(2012,4))
par(mfcol=c(3,1))

VIX_ts = ts(VIX[1:66],frequency=12,start=c(2012,4))

yy=cbind(TurnoverRatio_ts, Inflation_ts, FDI_ts)
MTSplot(yy)

####################################################
### Checking for stationarity for Turnover and WPI #
####################################################
plot(Turnover_ts)
Turnover_ADF=ur.df(Turnover_ts,type="trend",selectlags = c("AIC"))
summary(Turnover_ADF)
### HO can not be rejected
### thus stochastic stationary - differencing to see it if it is I(1)
Turnover_diff = diff(Turnover_ts)
plot(Turnover_diff)
Turnover_diff_ADF=ur.df(Turnover_diff,type="none",selectlags = c("AIC"))
summary(Turnover_diff_ADF)
### Stationary - thus I(1)

plot(TurnoverRatio_ts,main='Time Series - Turnover Ratio',xlab='Time',ylab='Turnover Ratio',lwd=3,col='darkblue')
Turnover_ADF=ur.df(TurnoverRatio_ts,type="drift",selectlags = c("AIC"))
summary(Turnover_ADF)
### HO can not be rejected
### thus stochastic stationary - differencing to see it if it is I(1)
Turnover_diff = diff(TurnoverRatio_ts)
plot(Turnover_diff,main='Time Series - Turnover Ratio (Differenced)',xlab='Time',ylab='Turnover Ratio (Differenced)',lwd=3,col='darkblue')
Turnover_diff_ADF=ur.df(Turnover_diff,type="none",selectlags = c("AIC"))
summary(Turnover_diff_ADF)
### Stationary - thus I(1)


par(bg = 'snow1',col.lab="blue4")
plot(Capitalization_ts,main='Time Series - Market Capitalization',xlab='Time',ylab='Market Capitalisation',lwd=3,col='darkblue')
Capitalization_ADF=ur.df(Capitalization_ts,type="trend",selectlags = c("AIC"))
summary(Capitalization_ADF)
### HO can not be rejected
### thus stochastic stationary - differencing to see it if it is I(1)
Capitalization_diff = diff(Capitalization_ts)
plot(Capitalization_diff,main='Time Series - Market Capitalization (Differenced)',xlab='Time',ylab='Market Capitalisation (Differenced)',lwd=3,col='darkblue')
Capitalization_diff_ADF=ur.df(Capitalization_diff,type="none",selectlags = c("AIC"))
summary(Capitalization_diff_ADF)
### Stationary - thus I(1)


### log of capitalization
plot(log(Capitalization_ts),main='Time Series - Market Capitalization (Log)',xlab='Time',ylab='Market Capitalisation (Log)',lwd=3,col='darkblue')
log_Capitalization_ADF=ur.df(log(Capitalization_ts),type="trend",selectlags = c("AIC"))
summary(log_Capitalization_ADF)
log_Capitalization_ts = log(Capitalization_ts)
### HO can not be rejected
### thus stochastic stationary - differencing to see it if it is I(1)
log_Capitalization_diff = diff(log(Capitalization_ts))
plot(log_Capitalization_diff,main='Time Series - Market Capitalization (Log & Differenced)',xlab='Time',ylab='Market Capitalisation (Log & Differenced)',lwd=3,col='darkblue')
log_Capitalization_diff_ADF=ur.df(log_Capitalization_diff,type="none",selectlags = c("AIC"))
summary(log_Capitalization_diff_ADF)
### Stationary - thus I(1)

######
plot(log(Nifty_ts),main='Time Series - Nifty (Log)',xlab='Time',ylab='Nifty (Log)',lwd=3,col='darkblue')
Nifty_ADF=ur.df(log(Nifty_ts),type="trend",selectlags = c("AIC"))
summary(Nifty_ADF)
### HO can not be rejected
### thus stochastic stationary - differencing to see it if it is I(1)
Nifty_diff = diff(log(Nifty_ts))
plot(Nifty_diff,main='Time Series - Nifty (Log & Differenced)',xlab='Time',ylab='Nifty (Log & Differenced)',lwd=3,col='darkblue')
Nifty_diff_ADF=ur.df(Nifty_diff,type="none",selectlags = c("AIC"))
summary(Nifty_diff_ADF)
### Stationary - thus I(1)

plot(TurnoverRatio_ts)
TurnoverRatio_ADF=ur.df(TurnoverRatio_ts,type="drift",selectlags = c("AIC"))
summary(TurnoverRatio_ADF)
### HO rejected at 5% not at 1% - stationart at 5% not at 1%

plot(Inflation_ts,main='Time Series - Inflation',xlab='Time',ylab='Inflation',lwd=3,col='darkblue')
Inflation_ADF=ur.df(Inflation_ts,type="none",selectlags = c("AIC"))
summary(Inflation_ADF)
### HO not stationary for both mean = 0 and non-zero drift
Inflation_diff = diff(Inflation_ts)
plot(Inflation_diff,main='Time Series - Inflation (Differenced)',xlab='Time',ylab='Inflation (Differenced)',lwd=3,col='darkblue')
Inflation_diff_ADF=ur.df(Inflation_diff,type="none",selectlags = c("AIC"))
summary(Inflation_diff_ADF)
#### Ho rejected. Thus it is I(1)

plot(log(FDI_ts),main='Time Series - FDI (Log)',xlab='Time',ylab='FDI (Log)',lwd=3,col='darkblue')
FDI_ADF=ur.df(log(FDI_ts),type="none",selectlags = c("AIC"))
summary(FDI_ADF)
### HO non-stationary.
### thus stochastic stationary - differencing to see it if it is I(1)
FDI_diff = diff(log(FDI_ts))
plot(FDI_diff,main='Time Series - FDI (Log & Differenced)',xlab='Time',ylab='FDI (Log & Differenced)',lwd=3,col='darkblue')
FDI_diff_ADF=ur.df(FDI_diff,type="none",selectlags = c("AIC"))
summary(Nifty_diff_ADF)
### Stationary - thus I(1)



plot(IIP_ts)
IIP_ADF=ur.df(IIP_ts,type="trend",selectlags = c("AIC"))
summary(IIP_ADF)
### dETERMINISTIC tREND - nOT I(1)

z1 = read.csv(file = 'VIX.csv')

z2 = ts(z1$Adj.Close[37:108],frequency=12,start=c(2012,4))
par(bg = 'snow1',col.lab="blue4")
plot(z2,main='Time Series - VIX',xlab='Time',ylab='VIX',lwd=3,col='darkblue')
plot(z2)
Turnover_ADF=ur.df(z2,type="drift",selectlags = c("AIC"))
summary(Turnover_ADF)
