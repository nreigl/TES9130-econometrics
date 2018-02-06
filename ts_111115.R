# ts_111115

### Load and require snippet
# install necessary packages
PACKAGES<-c("dplyr","vars","brew", "seasonal", "boot", "Hmisc", "forecast")

inst<-match(PACKAGES, .packages(all=TRUE))
need<-which(is.na(inst))
if (length(need)>0) install.packages(PACKAGES[need])

# load required packages
lapply(PACKAGES, require, character.only=T)


# load dataset
CPI_1970_2007 <- read.table("~/studium/TUT/econometric (phd) - TES9130/matlab/CPI_1970_2007.csv", quote="\"", comment.char="")


# format as time series object
CPI.ts<-ts(CPI_1970_2007, start = c(1970,1), freq = 12)

# check for seasonal component
plot(decompose(CPI.ts))
## no clear visible seasonal component

plot(CPI.ts)
## probably struc. break starting in the great moderation
## no visible trend


#test for stationarity/ unit root 
## systemic testing structure
acf(CPI.ts)
pacf(CPI.ts)

adf_p2_SA_CPI_data.ts<-summary(ur.df(SA_CPI_data.ts, type = "drift", lags = 2)) # constant
adf_p12_SA_CPI_data.ts<-summary(ur.df(SA_CPI_data.ts, type = "drift", lags = 12)) # constant
adf_p24_SA_CPI_data.ts<-summary(ur.df(SA_CPI_data.ts, type = "drift", lags = 24)) # constant

# first diff. 
acf(diff(CPI.ts))
pacf(diff(CPI.ts))

adf_p1_SA_CPI_data.ts<-summary(ur.df(diff(SA_CPI_data.ts), type = "none", lags = 1)) # constant
adf_p1_SA_CPI_data.ts<-summary(ur.df(diff(SA_CPI_data.ts), type = "none", lags = 12)) # constant
adf_p1_SA_CPI_data.ts<-summary(ur.df(diff(SA_CPI_data.ts), type = "none", lags = 24)) # constant

kpss_drift_SA_CPI_data.ts<-summary(ur.kpss(diff(SA_CPI_data.ts), type = "mu")) # constant

# Find the AR order
m1_mean<-ar(CPI.ts, demean = TRUE)
m1<-ar(CPI.ts)
m1$order
m1_mean$order
m1$aic
m1_mean$aic


# attribute AR order value
AR25<-m1$order

m2<-arima(CPI.ts, order = c(AR25, 0, 0))
summary(m2)
m2

# box test
Box.test(m2$residuals, lag = 25, type = "Ljung")
# analyze the residuals from the fit for any signs of non–randomness.
tsdiag(m2)

# residuals
m2$residuals
plot(m2$residuals)

# residual standard error
sqrt(m2$sigma2)

# Test for process parameters
shapiro.test(CPI.ts)
shapiro.test(diff(CPI.ts))


wilcox.test(CPI.ts, diff(CPI.ts))


# work with simulated time series 
set.seed(123)
ts_AR <- arima.sim(n = 10000, list(ar = 0.5))

ar_fit <- arima(ts_AR, order = c(1, 0, 0), include.mean = FALSE)
ts_res <- residuals(ar_fit)
ts_res <- ts_res - mean(ts_res)
ar_model <- list(ar = coef(ar_fit))

# How to simulate specific tail processes
# mildly long-tailed
arima.sim(n = 63, list(ar = c(0.8897, -0.4858), ma = c(-0.2279, 0.2488)),
          rand.gen = function(n, ...) sqrt(0.1796) * rt(n, df = 5))


# DF test 
set.seed(4321)
E=rnorm(340)
ts1<-cumsum(E)
plot(ts1, type="line")

# test manually
# test if the regression coefficient in the linear regression is – or not – null
lags=0
z=diff(ts1)
n=length(z)
z.diff=embed(z, lags+1)[,1]
z.lag.1=ts1[(lags+1):n]
summary(lm(z.diff~0+z.lag.1 ))

# testing procedure is based on the Student’s t value
summary(lm(z.diff~0+z.lag.1 ))$coefficients[1,3]

# Now, use the df test from the urca package
df=ur.df(ts1,type="none",lags=0)
summary(df)
## the value for the t-statistic is the same
## the same would apply to the ADF when we repeat the exercise with lags



# Simple time series forecasts
## simulate an AR(2) process
set.seed(321)
ts2 <- arima.sim(n = 200, list(ar = c(0.8, -0.6)), innov = rnorm(200))
ts.plot(ts2)
acf(ts2)
pacf(ts2)

# for h steps ahead forecast with confidence intervals
plot(predict(ts2, h = 40))

# now, simulate an ARMA (2,2)
ts3 <-  arima.sim(list(order = c(2,0,2), ar = c(0.8, -0.6), ma= c(0.8, 0.6)), n = 200, innov = rnorm(200))
ts.plot(ts3)
acf(ts3)
pacf(ts3)

# for h steps ahead forecast with confidence intervals
plot(predict(ts3, h = 100))

# simulate an ARIMA (2,2)
ts4 <-  arima.sim(list(order = c(2,2,2), ar = c(0.8, -0.6), ma= c(0.8, -0.6)), n = 200, innov = rnorm(200))
ts.plot(ts4)
acf(ts4)
pacf(ts4)

# for h steps ahead forecast with confidence intervals
plot(predict(ts4, h = 100))

