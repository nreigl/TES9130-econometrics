# ts_111115

### Load and require snippet
# install necessary packages
PACKAGES<-c("dplyr","vars","brew", "seasonal", "boot", "Hmisc")

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


# work with seas. unadj. data!!
# test for stationarity
# there does not seem to be a trend. exclude a trend
# estimate lag order via VAR: set max. lag structure to 24 max lags + intercept. no dummy
## check the t-statistics for overfitting: plus minus 2 is boarderline 
# get VAR model statistics
# test acf, pacf, normality, 

# run the analysis on the undiff. and diff. model
# forecast: specify horizon and interval


# write a proper simulation in R
# 



var.2c <- VAR(CPI.ts, p = 2, type = "const")
plot(var.2c)




# check for stationartiy
adf_p2_SA_CPI_data.ts<-summary(ur.df(SA_CPI_data.ts, type = "trend", lags = 2)) # trend and constant
adf_p1_SA_CPI_data.ts<-summary(ur.df(diff(SA_CPI_data.ts), type = "drift", lags = 1)) # constant
adf_p1_SA_CPI_data.ts
adf_p2_SA_CPI_data.ts

kpss_trend_SA_CPI_data.ts<-summary(ur.kpss(SA_CPI_data.ts, type = "tau")) # trend and constant
kpss_drift_SA_CPI_data.ts<-summary(ur.kpss(diff(SA_CPI_data.ts), type = "mu")) # constant

adf_p2_SA_CPI_data.ts<-summary(ur.df(CPI.ts, type = "trend", lags = 2)) # trend and constant
adf_p1_SA_CPI_data.ts<-summary(ur.df(diff(CPI.ts), type = "drift", lags = 1)) # constant
plot(diff(CPI.ts))

adf_p1_SA_CPI_data.ts
adf_p2_SA_CPI_data.ts





# quickly simulate an AR 
set.seed(123)
ts_AR <- arima.sim(n = 10000, list(ar = 0.5))


ar_fun <- function(ts) c(ar = coef(arima(ts, order = c(1, 0, 0),
                                         include.mean = FALSE)), ts = ts)

ar_sim <- function(res, n.sim, ran.args) {
  rg <- function(n, res) sample(res, n, replace = TRUE)
  ts <- ran.args$ts
  model <- ran.args$model
  arima.sim(model = model, n = n.sim,
            rand.gen = rg, res = c(res))
}

ar_fit <- arima(ts_AR, order = c(1, 0, 0), include.mean = FALSE)
ts_res <- residuals(ar_fit)
ts_res <- ts_res - mean(ts_res)
ar_model <- list(ar = coef(ar_fit))


set.seed(1)
ar_boot <- tsboot(ts_res, ar_fun,
                  R = 99, sim = "model",
                  n.sim = 100, orig.t = FALSE,
                  ran.gen = ar_sim,
                  ran.args = list(ts = ts_AR, model = ar_model))

coefmat <- apply(ar_boot$t, 1, "[", 1)
seriesmat <- apply(ar_boot$t, 1, "[", -1)

hist(coefmat)


