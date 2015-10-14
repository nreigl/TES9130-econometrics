# lm_071015

#LOAD REQUIRED PACKAGES
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
library(lmtest)
library(RFGLS)
library(gvlma)
library(gmm)
library(AER)


# set wd
setwd("~/studium/TUT/econometric (phd) - TES9130/R/econometrics - TES9130/econometrics-TES9130")



#UPLOAD THE DATASET
Schooling<-read_dta("Schooling_dta.dta")

## Heteroskedasticity Plot
#BROWSE
View(Schooling)
dim(Schooling)

# discriptive statistics
summary(Schooling)
str(Schooling)

# Visualize dataset
pairs(Schooling)


# Generate age^2 variable
age762 <- Schooling$age76^2
summary(age762)

# Estimate OLS
lm1<- lm(lwage76 ~ black + smsa76 + south76 + iqscore + ed76 + exp76, data=Schooling)  
lm1
lm1$coef
summary(lm1)
str(lm1)     #### Same output as in STATA


# Estimate reduced form w.r.t ed76
lm2 <- lm(ed76 ~ black + smsa76 + south76 + iqscore + momed + daded + age76 + age762, data=Schooling)  
lm2
lm2$coef
summary(lm2)
str(lm2)     #### Same output as in STATA


# Test for checking the possible weak correlation problem for potential instruments - parents education upon schooling.
fs = lm(ed76 ~ black + smsa76 + south76 + iqscore + momed + daded + age76 + age762, data=Schooling)  
# null first-stage (i.e. exclude IVs):
fn = lm(lwage76 ~ black + smsa76 + south76 + iqscore + exp76, data=Schooling)
# simple F-test
waldtest(fs, fn)$F[2]   #### Got error because the model has lwage and there are not same variables 


### Run IV regression where experience and schooling are instrumented with age and mother education
#### 2SLS ESTIMATION
SLS<- ivreg(lwage76 ~ ed76 + exp76 | black + smsa76 + south76 + iqscore + momed + age762, data=Schooling)
summary(SLS) 
#### OUTPUT DIFFERENT FROM STATA. 
#### Instrumented:  ed76 exp76   while IV are black smsa76 south76 iqscore momed age762


#Run overidentified IV regression where experience and schooling are instrumented with age and mother and father education
SLS1<- ivreg(lwage76 ~ ed76 + exp76 | black + smsa76 + south76 + iqscore + momed + daded + age76 + age762, data=Schooling)
summary(SLS1)
#### OUTPUT DIFFERENT FROM STATA. 
#### Instrumented:  ed76 exp76   while IV are black smsa76 south76 iqscore momed daded age76 age762


# Tests of overidentifying restrictions
summary(SLS1,diagnostics=TRUE) 


#Run overidentified IV regression where experience and schooling are instrumented with age and mother and father education
SLS2<- ivreg(lwage76 ~ ed76 + exp76 | black + smsa76 + south76 + iqscore + momed + daded + age762, data=Schooling)
summary(SLS2)
#### OUTPUT DIFFERENT FROM STATA. 
#### Instrumented:  ed76 exp76   while IV are black smsa76 south76 iqscore momed daded age762

# Tests of overidentifying restrictions
summary(SLS2,diagnostics=TRUE) 


# Run IV regression with more efficient GMM option accounting for non-iid errors



