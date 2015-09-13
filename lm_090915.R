# lm 09.09.2015

# load required packages
library(hadleyverse)
library(foreign)
library(haven) # assuming stata13 files
library(checkpoint)
library(ProjectTemplate)
library(explainr)
library(magrittr)
library(psych)
library(MASS)


# set WD
setwd("~/studium/TUT/econometric (phd) - TES9130/R/econometrics - TES9130")

# load .dta file
# Cars<-read_dta("auto.dta")
# Cars<-as.data.frame(Cars)
# write.csv(Cars, file = "Cars.csv")
Cars <- read.csv("~/studium/TUT/econometric (phd) - TES9130/R/econometrics - TES9130/Cars.csv")

# discriptive statistics
summary(Cars)
str(Cars)

# Visualize dataset
pairs(Cars)

# replace empty character strings with R typical NA
Cars$rep78[Cars$rep78 == ""]<-NA

# Run a linear regression price on mpg, weight and trunk size.
lm.fit<-lm(price ~ mpg + weight, data=Cars) 
summary(lm.fit)

# list R stored regression parameters
str(lm.fit) # or
names(lm.fit)


## Visualize regression 
par(mfrow = c(2,3))  ## define plot margin
plot(lm.fit, which = 1:5, sub.caption = "price ~ mpg + weight")

# show vector of regression coefficients
class(lm.fit$coefficients)

# 1.1. calculate the K x K matrix X'X and display that cross-product matrix
crossprod(Cars$mpg, Cars$mpg)
crossprod(Cars$mpg, Cars$weight) 
crossprod(Cars$weight, Cars$weight)
### Question: How is the Intercept calculated?



# 1.2. Calculate the matrix X'Y. effectively calculates the 1xK vector Y'X
sum((Cars$price)%*%Cars$mpg)
sum((Cars$price)%*%Cars$weight)
### Question: How is the Intercept calculated?

# 1.3. Calculate the coefficient vector of OLS coefficient estimates
## calculate coefficients from first principles
X <- cbind(1,Cars$mpg,Cars$weight)
manualOLS<-solve( t(X) %*% X ) %*% t(X) %*% Cars$price ## beta_hat = X'X)^-1 X'y   denote the familiar OLS estimator of beta
manualOLS<-as.data.frame(manualOLS)

library(broom)
lm.fit.tidy<-tidy(lm.fit)

## check for is equal true
manualOLS & lm.fit.tidy$estimate



## 16.09.2015 ## 
# Calculate the standard errors for the OLS estimated coefficients
e_diag<-lm.fit.tidy$std.error
sd(Cars$mpg)
sd(Cars$weight)


# Calculate the Mean Square Error e(rmse)=e'e/N-K
