# lm_300915

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
        library(arm)
        library(broom)
        library(gmm)) # Generalized Method of Moments and Generalized Empirical Likelihood
        library(mvtnorm)
        library(sem) # General Structural Equation Models

# set wd
setwd("~/studium/TUT/econometric (phd) - TES9130/R/econometrics - TES9130/econometrics-TES9130")



## idea for the simulation  is taken from Jacob Simmering and adapted for course needs
## http://jacobsimmering.com/2014/01/10/InstrumentalVariables.html

# # clear workspace and set seed
rm(list=ls())
set.seed(10001)

# $\mathbb{E}(x) = \alpha_{0}+\alpha_{1} x^{*}+\alpha_{2}z $
# and 
# $\mathbb{E}(y) = \beta_{0}+\beta_{1} x+u $

# where $ x^{*} $is some latent part of $x$ and $u$ is still unobserved
# Generate x* and c and using a common variance. $\rho = 0.5$
## NB relating to the lab exercise $x^${*}$ relates to $v$!! 
xStarAndU <- mvrnorm(1000, c(20, 15), matrix(c(1, 0.5, 0.5, 1), 2, 2))
xStar <- xStarAndU[, 1]
u <- xStarAndU[, 2]

# Generate instrument z (which is purely random)
z <- rnorm(1000)
# Generate regressor x which is correlated with z, and with 
x <- xStar + z
# using 1 makes it easy to estimate how 'wrong' an estimator is. Also create some noise on y
y <- 1 + x + u + rnorm(1000, 0, 0.5)

mat<-cbind(y,x,z,u,xStar)
cor(mat)
cov(mat)

cor(x,u) ## we see a moderate correlation of x and u (positiv) --> we have an endogenitiy problem 
cor(z,u) ## Z instrument: is not correlated with error term u. z should be independent of u by assumtion

pairs(mat)

# estimate the full model (the model that we would estimate if we knew u)
lmFull<-lm(y ~ x + u)
summary(lmFull)
beta_x <- summary(lmFull)$coefficients[2, 1]
# robust 
lmFullR<-rlm(y ~ x + u)
summary(lmFullR)
beta_x_R <- summary(lmFullR)$coefficients[2, 1]


# run regression without unknown u
lm1endo<-lm(y ~ x)
summary(lm1endo)
beta_x_endo <- summary(lm1endo)$coefficients[2, 1]
(beta_x_endo-beta_x)*100
## the estimate was about 128% of the true effect of x

# plot residual behaviour 
opar <- par(mfrow = c(2,2), oma = c(0, 0, 1.1, 0))
plot(lm1endo, las = 1)

# robust 
lm1endoR<-rlm(y ~ x)
summary(lm1endoR)
opar <- par(mfrow = c(2,2), oma = c(0, 0, 1.1, 0))
plot(lm1endoR, las = 1)

# Check for the significance of coefficient z in reduced form to avoid weak instruments problem
lm2<-lm(x ~ z)
summary(lm2)

# break the correlation between the “x part” and the unobserved u by using the instrument and 2SLS
xHat <- lm(x ~ z)$fitted.values ## extract fitted mean values
lm3<-lm(y ~ xHat) ## 
summary(lm3)
## 2SLS as I have done it here result in parameter estimates thate are correct but the errors are not.
## Using the method this, you get standard errors from the second equation residuals ($\mathbb{E}(y) = \beta_{0}+\beta_{1} x+u $)
# whereas you want them from the first ($\mathbb{E}(x) = \alpha_{0}+\alpha_{1} x^{*}+\alpha_{2}z $).

## ivreg from the AER package will take care of that problem.
library("AER")
lmIV <- ivreg(y ~ x | z)
summary(lmIV) ## IV estimates are unbiased
summary(lmIV, diagnostics = T)


##############################################
## not calculated the IV estimator manually ##
##############################################

## Caclulate Hausman test manually:
fm_ols <- lm(y ~ x)
fm_iv <- ivreg(y ~ x | z)
cf_diff <- coef(fm_iv) - coef(fm_ols)
vc_diff <- vcov(fm_iv) - vcov(fm_ols)
x2_diff <- as.vector(t(cf_diff) %*% solve(vc_diff) %*% cf_diff)
pchisq(x2_diff, df = 2, lower.tail = FALSE)

## 2SLS via systemfit(), Hausman via hausman.systemfit()
library("systemfit")
sm_ols <- systemfit(y ~ x, method = "OLS")
sm_iv <- systemfit(y ~ x , method = "2SLS", inst = ~ z)
hausman.systemfit(sm_iv, sm_ols)
                  
## summary diagnostics from the AER package
summary(lmIV, diagnostics = T)
summary.ivreg


## Interpretation of summary ivreg 
## F-test on the first stage regression; Null is that the instrument is weak.
## Wu- Hausman reject H_{0}.  alternative hypothesis, b0 is consistent, whereas b1 isn’t. 
## Sargan test of overidentifying  is not applicable as there are no more instruments than regressors)
                  ### Interpretation of Wu-Hausman??


                  