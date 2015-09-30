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
        
        
        # set wd
        setwd("~/studium/TUT/econometric (phd) - TES9130/R/econometrics - TES9130/econometrics-TES9130")

#UPLOAD THE DATASET
Cars <- read.dta("Dataset_Cars old.dta")

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
#### Scatterplot price-mpg
attach(Cars)
plot(price, mpg, main="Scatterplot",xlab="mpg ", ylab="Price ", pch=20, col="blue")
     lines(lowess(price,mpg), col="red") ### Lowess line

#### Scatterplot price-weight
attach(Cars)
plot(price, weight, main="Scatterplot",xlab="weight ", ylab="Price ", pch=20, col="blue")
      lines(lowess(price,weight), col="red")

### Problems with x-axis and y-axis. They are reversed!!!! WHY?

##Combine two graphs above
attach(Cars)
par(mfrow=c(1,2))
plot(price, mpg, main="Scatterplot",xlab="mpg ", ylab="Price ", pch=20, col="blue")
lines(lowess(price,mpg), col="red") ### Lowess line

plot(price, weight, main="Scatterplot",xlab="weight ", ylab="Price ", pch=20, col="blue")
lines(lowess(price,weight), col="red")
### FIX THE COMBINE GRAPH WHEN AXISES PROBLEM IS SOLVED.


##### TESTING FOR HETEROSKEDASTICITY 
## Breusch-Pagan test for heteroskedasticity


## White test for heteroskedasticity




### White test step-by-step


### Auxiliar regression. We do not have any useful information but we need it
  
  
### CALCULATE AND COMPARE OLS WITH FGLS OR WLS
  
  
### CALCULATE FGLS OR WLS MANUALLY: b_FGLS=inv(X'WX)X'WY, where W is the weighting or transformation matrix


### Regress WLS with STATA procedure


### For every model we have to estimate vector coefficients and variance-covariance


### CALCULATE THE WHITE (1980) ROBUST COVARIANCE MATRIX: inv(xx)*(N/N-k)*A*inv(xx), where A=sum[(x_i)'e_i(e_i)'x_i]


### generate id for each observation


### Regress the model with robust standard errors option and compare the results.


#### Last commit ######
############################################################  












        
        
