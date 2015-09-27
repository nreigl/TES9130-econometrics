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
Cars <- read.csv("~/studium/TUT/econometric (phd) - TES9130/R/econometrics - TES9130/econometrics-TES9130/Cars.csv")

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


###24.09.2015###
# 1.1. calculate the K x K matrix X'X and display that cross-product matrix
crossprod(Cars$mpg, Cars$mpg)
crossprod(Cars$mpg, Cars$weight) 
crossprod(Cars$weight, Cars$weight)

# Calculate Intercept
X = model.matrix(lm.fit)
attr(1,"assign")
result=t(X) %*% X
print(result)
## Alternative, these commands to calculate singular coefficients and intercept
x<-as.matrix(cbind(int=1,Cars$mpg,Cars$weight))
y<-as.vector(Cars$price)
i<-diag(1,nrow=nrow(x), ncol=ncol(x))
print(result)


# 1.2. Calculate the matrix X'Y. effectively calculates the 1xK vector Y'X
sum((Cars$price)%*%Cars$mpg)
sum((Cars$price)%*%Cars$weight)
result=t(X)
Y<-(Cars$price)
attr(1,"assign")
result=(t(X) %*% Y)
print(result)
## Both commands give us same results.
X = model.matrix(lm.fit)
attr(1,"assign")
result=t(X)
Y<-(Cars$price)
attr(1,"assign")
result=(t(X) %*% Y)
print(result)


# 1.3. Calculate the coefficient vector of OLS coefficient estimates
## calculate coefficients from first principles
X <- cbind(1,Cars$mpg,Cars$weight)
manualOLS<-solve( t(X) %*% X ) %*% t(X) %*% Cars$price ## beta_hat = X'X)^-1 X'y   denote the familiar OLS estimator of beta
manualOLS<-as.data.frame(manualOLS)


# Matrix X'X
XtX <- t(X) %*% X
### XtX <- crossprod(X)   gives us same result 
print(XtX)

# Matrix (X'X)^(-1)
invXtX <- solve(XtX)
print(invXtX)
y<-(Cars$price)
beta <- solve(t(X) %*% X) %*% t(X) %*% y
print(beta)


## Display the calculated coefficient vector and compare with the STATA 
## stored vector e(b) With this command I see that bhat and vector have the same values
lm.fit$coef
c(beta[1])


# Task 2: Calculate the standard errors for the OLS estimated coefficients 
# and compare the result with STATA stored matrix e(V)
SE<- summary(lm.fit)$sigma^2 * solve(t(X) %*% X)
print(SE)

# Extract the diagonal vector of estimate variances
i<-diag(SE)
print(i)

# Generate the vector of standard errors for mpg and weight variables
lm.fit <- lm(price ~ mpg + weight, data=Cars)
coef(summary(lm.fit))[, "Std. Error"]
## Standard Errors are right but it takes also intercept

# Calculate the OLS estimator Variance-Covariance Matrix VCE or V(b) using s^2(X'X)^-1
# Matrix X'X
XtX <- t(X) %*% X
# XtX <- crossprod(X)    
print(XtX)

# Matrix (X'X)^(-1)
invXtX <- solve(XtX)
print(invXtX)
SE<-summary(lm.fit)$sigma^2 *invXtX
print(SE)


# Calculate the Mean Square Error e(rmse)=e'e/N-K. Make matrix from my variable called resid. 
## It does not make a matrix because it is a singular vector 
### QUESTION: How is the matrix calculated?

# GOODNESS-OF-FIT: Calculation of F-statistic and R-square
fit <- lm(price ~ mpg + weight, data=Cars)
summary(fit)
RSS <- sum(residuals(fit)^2)

# Model with only intercept
fit1 <- lm(price ~ 1, data=Cars)
summary(fit1)
RSS1 <- sum(residuals(fit1)^2)

# F-Statistic
Fvalue <- (RSS1-RSS)/(3-1)/RSS*(74-3)
Fvalue
# Probability of F-test
pf(Fvalue,2,71,lower.tail=FALSE)


# Calculate the R-square
lm.fit <- lm(price ~ mpg + weight, data=Cars)
summary(lm.fit)$r.squared 

# Calculate the adjusted R-square: R2-(1-R2)*df_m/df_r
summary(lm(price ~ mpg + weight, data=Cars))$adj.r.squared

# Calculate the R_square as a squared correlation of predicted and true values of y
yhat = X %*% beta
cat("yhat vector:\n")
print(yhat)
cor(yhat,Cars$price) ## or cor(yhat,Y)
R2<-cor(yhat,Cars$price)^2
R2

# Calculation of confidence intervals
lm.fit <- lm(price ~ mpg + weight, data=Cars)
confint(lm.fit)
confint(lm.fit, "wt")


library(broom)
lm.fit.tidy<-tidy(lm.fit)

## check for:  is equal true
manualOLS & lm.fit.tidy$estimate

## 16.09.2015 ## 
# Calculate the standard errors for the OLS estimated coefficients
e_diag<-lm.fit.tidy$std.error
sd(Cars$mpg)
sd(Cars$weight)

# Calculate the Mean Square Error e(rmse)=e'e/N-K

sessionInfo()
