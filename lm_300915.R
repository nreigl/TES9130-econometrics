# lm_300915

# load required packages
library(hadleyverse)
library(foreign)
library(haven) # assuming stata13 files
library(checkpoint)
library(ProjectTemplate)
library(explainr)
library(magrittry
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
                  





## ivreg2
ivreg2 <- function(form,endog,iv,data,digits=3){
  # library(MASS)
  # model setup
  r1 <- lm(form,data)
  y <- r1$fitted.values+r1$resid
  x <- model.matrix(r1)
  aa <- rbind(endog == colnames(x),1:dim(x)[2])  
  z <- cbind(x[,aa[2,aa[1,]==0]],data[,iv])  
  colnames(z)[(dim(z)[2]-length(iv)+1):(dim(z)[2])] <- iv  
  # iv coefficients and standard errors
  z <- as.matrix(z)
  pz <- z %*% (solve(crossprod(z))) %*% t(z)
  biv <- solve(crossprod(x,pz) %*% x) %*% (crossprod(x,pz) %*% y)
  sigiv <- crossprod((y - x %*% biv),(y - x %*% biv))/(length(y)-length(biv))
  vbiv <- as.numeric(sigiv)*solve(crossprod(x,pz) %*% x)
  res <- cbind(biv,sqrt(diag(vbiv)),biv/sqrt(diag(vbiv)),(1-pnorm(biv/sqrt(diag(vbiv))))*2)
  res <- matrix(as.numeric(sprintf(paste("%.",paste(digits,"f",sep=""),sep=""),res)),nrow=dim(res)[1])
  rownames(res) <- colnames(x)
  colnames(res) <- c("Coef","S.E.","t-stat","p-val")
  # First-stage F-test
  y1 <- data[,endog]
  z1 <- x[,aa[2,aa[1,]==0]]
  bet1 <- solve(crossprod(z)) %*% crossprod(z,y1)
  bet2 <- solve(crossprod(z1)) %*% crossprod(z1,y1)
  rss1 <- sum((y1 - z %*% bet1)^2)
  rss2 <- sum((y1 - z1 %*% bet2)^2)
  p1 <- length(bet1)
  p2 <- length(bet2)
  n1 <- length(y)
  fs <- abs((rss2-rss1)/(p2-p1))/(rss1/(n1-p1))
  firststage <- c(fs)
  firststage <- matrix(as.numeric(sprintf(paste("%.",paste(digits,"f",sep=""),sep=""),firststage)),ncol=length(firststage))
  colnames(firststage) <- c("First Stage F-test")
  # Hausman tests
  bols <- solve(crossprod(x)) %*% crossprod(x,y) 
  sigols <- crossprod((y - x %*% bols),(y - x %*% bols))/(length(y)-length(bols))
  vbols <- as.numeric(sigols)*solve(crossprod(x))
  sigml <- crossprod((y - x %*% bols),(y - x %*% bols))/(length(y))
  x1 <- x[,!(colnames(x) %in% "(Intercept)")]
  z1 <- z[,!(colnames(z) %in% "(Intercept)")]
  pz1 <- z1 %*% (solve(crossprod(z1))) %*% t(z1)
  biv1 <- biv[!(rownames(biv) %in% "(Intercept)"),]
  bols1 <- bols[!(rownames(bols) %in% "(Intercept)"),]
  # Durbin-Wu-Hausman chi-sq test:
  # haus <- t(biv1-bols1) %*% ginv(as.numeric(sigml)*(solve(crossprod(x1,pz1) %*% x1)-solve(crossprod(x1)))) %*% (biv1-bols1)
  # hpvl <- 1-pchisq(haus,df=1)
  # Wu-Hausman F test
  resids <- NULL
  resids <- cbind(resids,y1 - z %*% solve(crossprod(z)) %*% crossprod(z,y1))
  x2 <- cbind(x,resids)
  bet1 <- solve(crossprod(x2)) %*% crossprod(x2,y)
  bet2 <- solve(crossprod(x)) %*% crossprod(x,y)
  rss1 <- sum((y - x2 %*% bet1)^2)
  rss2 <- sum((y - x %*% bet2)^2)
  p1 <- length(bet1)
  p2 <- length(bet2)
  n1 <- length(y)
  fs <- abs((rss2-rss1)/(p2-p1))/(rss1/(n1-p1))
  fpval <- 1-pf(fs, p1-p2, n1-p1)
  #hawu <- c(haus,hpvl,fs,fpval)
  hawu <- c(fs,fpval)
  hawu <- matrix(as.numeric(sprintf(paste("%.",paste(digits,"f",sep=""),sep=""),hawu)),ncol=length(hawu))
  #colnames(hawu) <- c("Durbin-Wu-Hausman chi-sq test","p-val","Wu-Hausman F-test","p-val")
  colnames(hawu) <- c("Wu-Hausman F-test","p-val")  
  # Sargan Over-id test
  ivres <- y - (x %*% biv)
  oid <- solve(crossprod(z)) %*% crossprod(z,ivres)
  sstot <- sum((ivres-mean(ivres))^2)
  sserr <- sum((ivres - (z %*% oid))^2)
  rsq <- 1-(sserr/sstot)
  sargan <- length(ivres)*rsq
  spval <- 1-pchisq(sargan,df=length(iv)-1)
  overid <- c(sargan,spval)
  overid <- matrix(as.numeric(sprintf(paste("%.",paste(digits,"f",sep=""),sep=""),overid)),ncol=length(overid))
  colnames(overid) <- c("Sargan test of over-identifying restrictions","p-val")
  if(length(iv)-1==0){
    overid <- t(matrix(c("No test performed. Model is just identified")))
    colnames(overid) <- c("Sargan test of over-identifying restrictions")
  }
  full <- list(results=res, weakidtest=firststage, endogeneity=hawu, overid=overid)
  return(full)
}
ivreg2()



                  
                  