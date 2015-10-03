# lm_230915

# load required packages
library(hadleyverse)
library(foreign)
library(haven) # assuming stata13 files
library(checkpoint)
library(ProjectTemplate)
library(explainr)
library(magrittry)
        library(psych)
        library(MASS)
        library(arm)
        library(broom)
        library(lmtest)
        library(RFGLS)
        library(gvlma) # Methods from the paper: Pena, EA and Slate, EH, ``Global Validation of Linear Model Assumptions,'' J. American Statistical Association, 101(473):341-354, 2006

        # set wd
        setwd("~/studium/TUT/econometric (phd) - TES9130/R/econometrics - TES9130/econometrics-TES9130")

#UPLOAD THE DATASET
# Cars <- read.dta("Dataset_Cars old.dta") # depending in your system. 
        Cars <- read.csv("~/studium/TUT/econometric (phd) - TES9130/R/econometrics - TES9130/econometrics-TES9130/Cars.csv") # csv file is on github
        
## Heteroskedasticity Plot
#BROWSE
View(Cars)
dim(Cars)

# discriptive statistics
summary(Cars)
str(Cars)

# Visualize dataset
pairs(Cars)

# replace empty character strings with R typical NA
Cars$rep78[Cars$rep78 == ""]<-NA

#LINEAR REGRESSION PRICE ON mpg, weight and trunk size
lm1<-lm(price ~ mpg + weight, data=Cars)
lm1
summary(lm1)
str(lm1)

resid <- residuals(lm1)  
summary(resid) 
resid=lm1$residuals            ### Output as in STATA

### Generate the absolute value of residuals
absresid<- abs(resid)
summary(absresid)

#### Construct the graph for two variables and combine them
# Scatterplot Price-mpg
attach(Cars)
plot(Cars$mpg, Cars$price, 
     main="Scatterplot",
     xlab="mpg ", 
     ylab="Price ", pch=20, col="blue")
     lines(lowess(mpg, price), col="red") ### Lowess line

# Scatterplot Price-weight
attach(Cars)
plot(Cars$weight, Cars$price, 
     main="Scatterplot",
     xlab="weight ", ylab="Price ", pch=20, col="blue")
     lines(lowess(weight,price), col="red")

##Combine two graphs above
attach(Cars)
par(mfrow=c(1,2))
plot(Cars$mpg, Cars$price, 
     main="Scatterplot",
     xlab="mpg ", 
     ylab="Price ", pch=20, col="blue")
lines(lowess(mpg, price), col="red") ### Lowess line

plot(Cars$weight, Cars$price, 
     main="Scatterplot",
     xlab="weight ", ylab="Price ", pch=20, col="blue")
lines(lowess(weight,price), col="red")

### Problems with x-axis and y-axis solved. Graphs look the same as in STATA


##### TESTING FOR HETEROSKEDASTICITY 
## Breusch-Pagan test for heteroskedasticity
lm1<-lm(price ~ mpg + weight, data=Cars)
lm1
lm1$coef
bptest(lm1)    ##Different output from STATA (in STATA we reject the null hypotheses, in R not!)
### We have to reject the null hypotheses 
# running a global validation of linear model assumptions (gvlma) on the lm object.
gvlma(x = lm1) 
par(mfrow=c(2,1))
plot(lm1)


## White test for heteroskedasticity

# QUESTION: WHICH PACKAGE DO WE HAVE TO USE FOR WHITE TEST?


### White test step-by-step
lm1<-lm(price ~ mpg + weight, data=Cars)
resid <- residuals(lm1) 
summary(resid)        

### Auxiliar regression. We do not have any useful information but we need it
### Generate square residuals
resid2<-resid^2  
summary(resid2)
### Generate square mpg
mpg2<- mpg^2
summary(mpg2)
### Generate square weight
weight2<- weight^2
summary(weight2)

### Generate interaction mpg and weigth
mpg_weight<- Cars$mpg*weight
summary(mpg_weight)               ###Same output as in STATA
lm2<- lm(resid2 ~ mpg2 + weight2 + mpg + weight + mpg_weight, data=Cars)
lm2

### Compute the statistic nR2 and test with chi-square distribution
summary(lm2)$r.squared  
###or we can also run these commands
N<-(lm2$df)+length(lm2$coef)
R2<-summary(lm2)$r.squared 
R2
whitestat<- 74*0.1713119
whitestat       ###Same output as in STATA.
                                                ### QUESTION: how do we compute "chi2tail"?
                
### CALCULATE AND COMPARE OLS WITH FGLS OR WLS
lm1<-lm(price ~ mpg + weight, data=Cars)
### Genetare dummy named "one" 
one<- 1
summary(one)
### Generate the Non Linear Least Squares

  
  
### CALCULATE FGLS OR WLS MANUALLY: b_FGLS=inv(X'WX)X'WY, where W is the weighting or transformation matrix

### Regress WLS with STATA procedure

### For every model we have to estimate vector coefficients and variance-covariance

### CALCULATE THE WHITE (1980) ROBUST COVARIANCE MATRIX: inv(xx)*(N/N-k)*A*inv(xx), where A=sum[(x_i)'e_i(e_i)'x_i]

### generate id for each observation

### Regress the model with robust standard errors option and compare the results.



#### Last commit ######
############################################################  












        
        
