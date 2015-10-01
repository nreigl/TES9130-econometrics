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
        library(gmm) # Generalized Method of Moments and Generalized Empirical Likelihood
        
        
        # set wd
        setwd("~/studium/TUT/econometric (phd) - TES9130/R/econometrics - TES9130/econometrics-TES9130")
        
# read in data
Schooling<-read_dta("Schooling_dta.dta")
summary(Schooling)
View(Schooling)
        
#### Estimate OLS
lm1<- lm(lwage76 ~ ed76 +  exp76 + exp762 + black + smsa76, data=Schooling)  
lm1
lm1$coef
summary(lm1)
str(lm1)     #### Same output as in STATA

#### Estimate reduced from w.r.t ed76
lm1<- lm(ed76 ~ exp76 + exp762 + black + smsa76 + momed + daded, data=Schooling)  
lm1
lm1$coef
summary(lm1)
str(lm1)                        ### As in STATA ###

#### Test for checking the possible weak correlation problem for potential instruments - parents education upon schooling.
fs = lm(ed76 ~ exp76 + exp762 + black + smsa76 + momed + daded, data=Schooling)
# null first-stage (i.e. exclude IVs):
fn = lm(ed76 ~ exp76 + exp762 + black + smsa76, data=Schooling)
# simple F-test
waldtest(fs, fn)$F[2]      #### As in STATA ###


### Run IV regression where experience and schooling are instrumented with age and mother and father education
# Generate age762 square
age762<- Schooling$age76^2
summary(age762sq)               ### Output ok!


### Run IV regression where experience and schooling are instrumented with age and mother and father education


### Generate table to compare OLS and IV


#### Last commit ####
        
