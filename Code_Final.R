#setwd("E:/PGDBA 2017-19/IIM C Third Semester/Selected Aspects of Predictive Modelling/Project")
library(timeDate)
library(timeSeries)
library(fBasics)
library(urca)
library(lmtest)
library(sandwich)
library(readr)
library(MTS)
library(vars)

rm(list = ls())
# Reading Data
da=read.csv("Stock Market_Economic Data.csv")
head(da)
tail(da)
attach(da)
dim(da)
names(da)

## creating yearly time series objects starting from 1960 
Turnover_ts=ts(Turnover[1:66],frequency=12,start=c(2012,4))

Capitalization_ts = ts(Market.Capitalization[1:66],frequency=12,start=c(2012,4))

TurnoverRatio_ts = ts(Turnover.Ratio,frequency=12,start=c(2012,4),end = c(2017,9))

WPI_ts=ts(WPI,frequency=12,start=c(2012,4),end = c(2017,9))

Inflation_ts=ts(Inflation,frequency=12,start=c(2012,4),end = c(2017,9))

FDI_ts=ts(FDI,frequency=12,start=c(2012,4),end = c(2017,9))

Nifty_ts  = ts(Nifty,frequency=12,start=c(2012,4),end = c(2017,9))

IIP_ts=ts(IIP,frequency=12,start=c(2009,4),end = c(2017,9))
par(mfcol=c(3,1))

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


plot(Capitalization_ts)
Capitalization_ADF=ur.df(Capitalization_ts,type="trend",selectlags = c("AIC"))
summary(Capitalization_ADF)
### HO can not be rejected
### thus stochastic stationary - differencing to see it if it is I(1)
Capitalization_diff = diff(Capitalization_ts)
plot(Capitalization_diff)
Capitalization_diff_ADF=ur.df(Capitalization_diff,type="none",selectlags = c("AIC"))
summary(Capitalization_diff_ADF)
### Stationary - thus I(1)


### log of capitalization
plot(log(Capitalization_ts))
log_Capitalization_ADF=ur.df(log(Capitalization_ts),type="trend",selectlags = c("AIC"))
summary(log_Capitalization_ADF)
log_Capitalization_ts = log(Capitalization_ts)
### HO can not be rejected
### thus stochastic stationary - differencing to see it if it is I(1)
log_Capitalization_diff = diff(log(Capitalization_ts))
plot(log_Capitalization_diff)
log_Capitalization_diff_ADF=ur.df(log_Capitalization_diff,type="none",selectlags = c("AIC"))
summary(log_Capitalization_diff_ADF)
### Stationary - thus I(1)


######
plot(Nifty_ts)
Nifty_ADF=ur.df(Nifty_ts,type="trend",selectlags = c("AIC"))
summary(Nifty_ADF)
### HO can not be rejected
### thus stochastic stationary - differencing to see it if it is I(1)
Nifty_diff = diff(Nifty_ts)
plot(Nifty_diff)
Nifty_diff_ADF=ur.df(Nifty_diff,type="none",selectlags = c("AIC"))
summary(Nifty_diff_ADF)
### Stationary - thus I(1)




plot(TurnoverRatio_ts)
TurnoverRatio_ADF=ur.df(TurnoverRatio_ts,type="trend",selectlags = c("AIC"))
summary(TurnoverRatio_ADF)
### HO rejected at 5% not at 1% - stationart at 5% not at 1%



plot(Inflation_ts)
Inflation_ADF=ur.df(Inflation_ts,type="none",selectlags = c("AIC"))
summary(Inflation_ADF)
### HO not stationary for both mean = 0 and non-zero drift
Inflation_diff = diff(Inflation_ts)
plot(Inflation_diff)
Inflation_diff_ADF=ur.df(Inflation_diff,type="none",selectlags = c("AIC"))
summary(Inflation_diff_ADF)
#### Ho rejected. Thus it is I(1)

plot(FDI_ts)
FDI_ADF=ur.df(FDI_ts,type="none",selectlags = c("AIC"))
summary(FDI_ADF)
### HO not stationary.
FDI_diff = diff(FDI_ts)
plot(FDI_diff)
FDI_diff_ADF=ur.df(FDI_diff,type="none",selectlags = c("AIC"))
summary(FDI_diff_ADF)
### H0 stationary. I(1) h ye toh


####################################################
### Checking for co-integration  #########
####################################################
yy=cbind( TurnoverRatio_ts, Nifty_ts, FDI_ts, Inflation_ts )
MTSplot(yy)
 
## ecdet parameter also. K, and spec param also.


?ca.jo
coint=ca.jo(yy,ecdet="none",type="eigen",spec=c("longrun"),K=12)

summary(coint)
#### r = 2 , impying that there are 2 linear combinations



### At 5%, both r=0 and r=1 rejected for Turnover Ratio and WPI. Thus, r=2. Thus, stationary - VAR.
### However, if original series taken, we get r=0 , implying a univariate case, i.e, long-term 
### dependence may not exist.


### To be done
# Error correstion and long term coefficients
mcr = cajorls(coint)
mcr
summary(mcr$rlm)


## Now expressing VEC in VAR form
fore=vec2var(coint, r = 1)

varnames = c('Turnover.Ratio','Nifty','FDI','Inflation')
predict_and_plot = function(model_object=fore,dabur=da,varnames1=varnames,forecast_period=6,ci=0.95,plot=TRUE){
    nrows = dim(model_object$y)[1]
    ncols = dim(model_object$y)[2]
    pred = predict(model_object, n.ahead = forecast_period, ci = ci, dumvar = NULL)
    for(i in 1:ncols){
        x = dabur[varnames1[i]][(nrows+1):(nrows+forecast_period),]
        print(paste('Results for: ',dimnames(model_object$y)[[2]][i],sep = ''))
        print(paste('RMSE: ',sqrt(mean((pred$fcst[[i]][,1] - x)^2)),sep = ''))
        print(paste('MAE: ',mean(abs(pred$fcst[[i]][,1] - x)),sep = ''))
        print(paste('MAPE: ',100*mean(abs((pred$fcst[[i]][,1] - x)/(x))),'%',sep = ''))
    }
    plot(pred)
}
predict_and_plot()
